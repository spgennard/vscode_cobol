import ISourceHandler from "./isourcehandler";
import { cobolKeywordDictionary, cobolProcedureKeywordDictionary, cobolStorageKeywordDictionary } from "./keywords/cobolKeywords";

import { FileSourceHandler } from "./FileSourceHandler";
import { String } from 'typescript-string-operations';

import * as fs from 'fs';
import * as path from 'path';
import * as crypto from 'crypto';

import { logMessage, logException, isDirectory, isFile } from "./extension";

import { expandLogicalCopyBookToFilenameOrEmpty } from "./opencopybook";
import { Hash } from "crypto";
import { ICOBOLSettings, COBOLSettingsHelper } from "./iconfiguration";
import { Uri } from "vscode";

var lzjs = require('lzjs');

export enum COBOLTokenStyle {
    CopyBook = "Copybook",
    ProgramId = "Program-Id",
    ImplicitProgramId = "ImplicitProgramId-Id",
    FunctionId = "Function-Id",
    EndFunctionId = "EndFunctionId",
    Constructor = "Constructor",
    MethodId = "Method-Id",
    Property = "Property",
    ClassId = "Class-Id",
    InterfaceId = "Interface-Id",
    ValueTypeId = "Valuetype-Id",
    EnumId = "Enum-id",
    Section = "Section",
    Paragraph = "Paragraph",
    Division = "Division",
    EntryPoint = "Entry",
    Variable = "Variable",
    Constant = "Constant",
    EndDelimiter = "EndDelimiter",
    Exec = "Exec",
    EndExec = "EndExec",
    Declaratives = "Declaratives",
    EndDeclaratives = "EndDeclaratives",
    DeclarativesSection = "DeclarativesSection",
    Null = "Null"
}

export function splitArgument(input: string): string[] {
    let ret: string[] = [];
    let inQuote: boolean = false;
    let inQuoteSingle: boolean = false;
    let lineLength = input.length;
    let cArg: string = "";

    for (let i = 0; i < lineLength; i++) {
        let c = input.charAt(i);

        /* handle quotes */
        if (c === '\'' && !inQuote) {
            inQuoteSingle = !inQuoteSingle;
            cArg += c;
            if (inQuoteSingle === false) {
                ret.push(cArg);
                cArg = "";
            }
            continue;
        }

        if (c === "\"" && !inQuoteSingle) {
            inQuote = !inQuote;
            cArg += c;
            if (inQuote === false) {
                ret.push(cArg);
                cArg = "";
            }
            continue;
        }

        if (inQuote || inQuoteSingle) {
            cArg += c;
            continue;
        }

        /* skip white space */
        if ((c === ' ') || (c === '\t')) {
            if (cArg.length !== 0) {
                ret.push(cArg);
                cArg = "";
            }
            while ((c === ' ') || (c === '\t')) {
                i++;
                c = cArg.charAt(i);
            }
            i--;
            continue;
        }

        cArg += c;
    }

    if (cArg.length !== 0) {
        ret.push(cArg);
    }

    return ret;
}

export class COBOLToken {
    public filename: string;
    public tokenType: COBOLTokenStyle;
    public startLine: number;
    public startColumn: number;
    public tokenName: string;
    public tokenNameLower: string;
    public description: string;
    public parentToken: COBOLToken | undefined;
    public endLine: number;
    public endColumn: number;
    public inProcedureDivision: boolean;
    public extraInformation: string;
    public skipToEnd: boolean;
    public endOfSkip: boolean;

    static Null: COBOLToken = new COBOLToken("", COBOLTokenStyle.Null, -1, "", "", "", undefined, false, "");

    public getEndDelimiterToken(): COBOLToken {
        return new COBOLToken(this.filename, COBOLTokenStyle.EndDelimiter, this.startLine, "", this.tokenName, this.description, this.parentToken, this.inProcedureDivision, "");
    }

    public constructor(filename: string, tokenType: COBOLTokenStyle, startLine: number, line: string, token: string, description: string,
        parentToken: COBOLToken | undefined, inProcedureDivision: boolean, extraInformation: string) {
        this.filename = filename;
        this.tokenType = tokenType;
        this.startLine = startLine;
        this.tokenName = token.trim();
        this.tokenNameLower = this.tokenName.toLowerCase();
        this.startColumn = line.indexOf(this.tokenName);
        this.description = description;
        this.endLine = this.endColumn = 0;
        this.parentToken = parentToken;
        this.inProcedureDivision = inProcedureDivision;
        this.extraInformation = extraInformation;

        this.skipToEnd = false;
        this.endOfSkip = false;

        switch (tokenType) {
            case COBOLTokenStyle.FunctionId:
                this.skipToEnd = true;
                break;
            case COBOLTokenStyle.EndFunctionId:
                this.endOfSkip = true;
                break;
        }
        if (this.tokenName.length !== 0) {
            /* ensure we don't have any odd start columns */
            if (this.startColumn < 0) {
                this.startColumn = 0;
            }
        }
    }
}

export class SourceReference {
    public fileIdentifer: number;
    public line: number;
    public columnn: number;

    public constructor(fileIdentifer: number, line: number, column: number) {
        this.fileIdentifer = fileIdentifer;
        this.line = line;
        this.columnn = column;
    }

}

class Token {
    public line: string = "";

    private lineTokens: string[] = [];
    private lineLowerTokens: string[] = [];
    private tokenIndex: number = 0;

    public currentToken: string = "";
    public prevToken: string = "";
    public nextToken: string = "";
    public nextPlusOneToken: string = "";  // only used for method-id. get property xxx

    public currentTokenLower: string = "";
    public prevTokenLower: string = "";
    public nextTokenLower: string = "";

    public currentCol: number = 0;
    public prevCol: number = 0;
    public rollingColumn: number = 0;

    public endsWithDot: boolean = false;

    public constructor(line: string, previousToken?: Token) {
        this.line = line;
        this.setupLine();

        if (previousToken !== undefined) {
            // wire in previous token into this token
            if (previousToken.lineTokens.length > 0) {
                let lastToken = previousToken.lineTokens[previousToken.lineTokens.length - 1];
                this.prevCol = previousToken.line.indexOf(lastToken);
                this.prevToken = lastToken;
                this.prevTokenLower = lastToken.toLowerCase();
            }
        }
    }

    public static Blank = new Token("", undefined);

    private setupLine() {
        this.lineTokens = splitArgument(this.line);
        this.lineLowerTokens = [];
        for (let c = 0; c < this.lineTokens.length; c++) {
            this.lineLowerTokens.push(this.lineTokens[c].toLowerCase());
        }
        this.tokenIndex = 0;
        this.setupToken();
    }

    private setupToken() {
        this.prevToken = this.currentToken;
        this.prevTokenLower = this.currentTokenLower;

        this.currentToken = this.lineTokens[this.tokenIndex];
        if (this.currentToken === undefined) {
            this.currentToken = this.currentTokenLower = "";
        } else {
            this.currentTokenLower = this.lineLowerTokens[this.tokenIndex];
        }

        this.prevCol = this.currentCol;
        this.currentCol = this.line.indexOf(this.currentToken, this.rollingColumn);
        this.rollingColumn = this.currentCol + this.currentToken.length;

        /* setup next token + 1 */
        if (2 + this.tokenIndex < this.lineTokens.length) {
            this.nextPlusOneToken = this.lineTokens[2 + this.tokenIndex];
        } else {
            this.nextPlusOneToken = "";
        }

        if (1 + this.tokenIndex < this.lineTokens.length) {
            this.nextToken = this.lineTokens[1 + this.tokenIndex];
            if (this.nextToken === undefined) {
                this.nextToken = this.nextTokenLower = "";
            } else {
                this.nextTokenLower = this.lineLowerTokens[1 + this.tokenIndex];
            }
        } else {
            this.nextToken = this.nextTokenLower = "";
        }
    }

    public moveToNextToken(): boolean {
        if (1 + this.tokenIndex > this.lineTokens.length) {
            return true;
        }

        this.tokenIndex++;
        this.setupToken();
        return false;
    }

    public isTokenPresent(possibleToken: string): boolean {
        let possibleTokenLower = possibleToken.toLocaleLowerCase();
        let possibleTokenLowerDot = possibleTokenLower + ".";

        for (let c = 0; c < this.lineLowerTokens.length; c++) {
            if (this.lineLowerTokens[c] === possibleTokenLower) {
                return true;
            }
            if (this.lineLowerTokens[c] === possibleTokenLowerDot) {
                return true;
            }
        }

        return false;
    }
}

export class SharedSourceReferences {
    public filenames: Uri[];

    public targetReferences: Map<string, SourceReference[]>;
    public constantsOrVariablesReferences: Map<string, SourceReference[]>;
    public sharedConstantsOrVariables: Map<string, COBOLToken[]>;
    public sharedSections: Map<string, COBOLToken>;
    public sharedParagraphs: Map<string, COBOLToken>;
    public processingMap: Map<string, string>;

    constructor() {
        this.filenames = [];
        this.targetReferences = new Map<string, SourceReference[]>();
        this.constantsOrVariablesReferences = new Map<string, SourceReference[]>();
        this.sharedConstantsOrVariables = new Map<string, COBOLToken[]>();
        this.sharedSections = new Map<string, COBOLToken>();
        this.sharedParagraphs = new Map<string, COBOLToken>();
        this.processingMap = new Map<string, string>();
    }
}

export default class COBOLQuickParse {
    public filename: string;
    public lastModifiedTime: number;

    public tokensInOrder: COBOLToken[] = [];

    public sections: Map<string, COBOLToken>;
    public paragraphs: Map<string, COBOLToken>;
    public constantsOrVariables: Map<string, COBOLToken[]>;
    public callTargets: Map<string, COBOLToken>;
    public classes: Map<string, COBOLToken>;
    public methods: Map<string, COBOLToken>;
    public isCached: boolean;
    public copyBooksUsed: Map<string, string>;

    public parseReferences: boolean;
    public sourceReferences?: SharedSourceReferences;
    public sourceFileId: number;

    public cpPerformTargets: any | undefined;
    public cpConstantsOrVars: any | undefined;

    public ImplicitProgramId: string = "";

    skipToDot: boolean = false;
    addReferencesDuringSkipToTag: boolean = false;

    inProcedureDivision: boolean;
    inDeclaratives: boolean;
    pickFields: boolean;

    currentToken: COBOLToken;
    currentRegion: COBOLToken;
    currentDivision: COBOLToken;
    currentSection: COBOLToken;
    currentParagraph: COBOLToken;
    procedureDivision: COBOLToken;
    declaratives: COBOLToken;
    parseColumnBOnwards: boolean; // = this.getColumBParsing();
    current01Group: COBOLToken;
    currentLevel: COBOLToken;
    captureDivisions: boolean;
    programs: COBOLToken[];
    currentClass: COBOLToken;
    currentMethod: COBOLToken;
    currentFunctionId: COBOLToken;

    numberTokensInHeader: number;
    workingStorageRelatedTokens: number;
    procedureDivisionRelatedTokens: number;
    sectionsInToken: number;
    divisionsInToken: number;

    copybookNestedInSection: boolean;

    sourceLooksLikeCOBOL: boolean;

    configHandler: ICOBOLSettings;

    parserHintDirectory: string;

    parseHint_OnOpenFiles: string[] = [];
    parseHint_WorkingStorageFiles: string[] = [];
    parseHint_LocalStorageFiles: string[] = [];
    parseHint_ScreenSectionFiles: string[] = [];

    private processingMap: Map<string, string>;

    public constructor(sourceHandler: ISourceHandler, filename: string, configHandler: ICOBOLSettings, cacheDirectory: string, sourceReferences?: SharedSourceReferences) {
        let stat: fs.Stats = fs.statSync(filename);
        this.configHandler = configHandler;
        this.filename = path.normalize(filename);
        this.ImplicitProgramId = path.basename(filename, path.extname(filename));
        this.lastModifiedTime = stat.mtimeMs;
        this.inProcedureDivision = false;
        this.inDeclaratives = false;
        this.pickFields = false;
        this.currentDivision = COBOLToken.Null;
        this.procedureDivision = COBOLToken.Null;
        this.currentSection = COBOLToken.Null;
        this.currentParagraph = COBOLToken.Null;
        this.currentToken = COBOLToken.Null;
        this.currentClass = COBOLToken.Null;
        this.currentMethod = COBOLToken.Null;
        this.currentRegion = COBOLToken.Null;
        this.declaratives = COBOLToken.Null;
        this.current01Group = COBOLToken.Null;
        this.currentLevel = COBOLToken.Null;
        this.currentFunctionId = COBOLToken.Null;
        this.programs = [];
        this.captureDivisions = true;
        this.numberTokensInHeader = 0;
        this.workingStorageRelatedTokens = 0;
        this.procedureDivisionRelatedTokens = 0;
        this.sectionsInToken = 0;
        this.divisionsInToken = 0;
        this.copybookNestedInSection = configHandler.copybooks_nested;
        this.copyBooksUsed = new Map();
        this.isCached = false;
        this.sections = new Map<string, COBOLToken>();
        this.paragraphs = new Map<string, COBOLToken>();
        this.constantsOrVariables = new Map<string, COBOLToken[]>();
        this.callTargets = new Map<string, COBOLToken>();
        this.classes = new Map<string, COBOLToken>();
        this.methods = new Map<string, COBOLToken>();
        this.sourceLooksLikeCOBOL = false;
        this.parseColumnBOnwards = configHandler.ignorecolumn_b_onwards;
        this.parseReferences = sourceHandler !== null;
        this.sourceReferences = sourceReferences;
        this.cpPerformTargets = undefined;
        this.cpConstantsOrVars = undefined;
        this.parserHintDirectory = configHandler.parser_hint_directory;

        let prevToken: Token = Token.Blank;

        let hasCOBOLExtension = path.extname(filename).length > 0 ? true : false;

        if (sourceReferences !== undefined) {
            this.sourceFileId = sourceReferences.filenames.length;
            sourceReferences.filenames.push(sourceHandler.getUri());
            this.constantsOrVariables = sourceReferences.sharedConstantsOrVariables;
            this.paragraphs = sourceReferences.sharedParagraphs;
            this.sections = sourceReferences.sharedSections;
            this.processingMap = sourceReferences.processingMap;
        } else {
            this.sourceFileId = 0;
            this.processingMap = new Map<string, string>();
        }

        /* mark this has been processed (to help copy of self) */
        this.processingMap.set(this.filename, this.filename);

        /* if we have an extension, then don't do a relaxed parse to determiune if it is COBOL or not */
        let lineLimit = configHandler.pre_parse_line_limit;
        let maxLines = sourceHandler.getLineCount();
        if (maxLines > lineLimit) {
            maxLines = lineLimit;
        }

        let line = "";
        for (let l = 0; l < maxLines; l++) {
            try {
                line = sourceHandler.getLine(l).trimRight();

                // don't parse a empty line
                if (line.length > 0) {
                    if (prevToken.endsWithDot === false) {
                        prevToken = this.relaxedParseLineByLine(sourceHandler, l, prevToken, line);
                    }
                    else {
                        prevToken = this.relaxedParseLineByLine(sourceHandler, l, Token.Blank, line);
                    }
                }

            }
            catch (e) {
                logException("CobolQuickParse - Parse error : " + e, e);
            }
        }

        // Do we have some sections?
        if (this.sectionsInToken === 0 && this.divisionsInToken === 0) {
            /* if we have items that could be in a data division */

            if (this.procedureDivisionRelatedTokens !== 0 && this.procedureDivisionRelatedTokens > this.workingStorageRelatedTokens) {
                this.ImplicitProgramId = "";

                let fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Procedure Division", "Procedure", "Procedure Division (CopyBook)", this.currentDivision);
                this.currentDivision = fakeDivision;
                this.procedureDivision = fakeDivision;
                this.pickFields = false;
                this.inProcedureDivision = true;
                this.sourceLooksLikeCOBOL = true;
            }
            else if ((this.workingStorageRelatedTokens !== 0 && this.numberTokensInHeader !== 0)) {
                let fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Data Division", "Data", "Data Division (CopyBook)", this.currentDivision);
                this.currentDivision = fakeDivision;
                this.pickFields = true;
                this.inProcedureDivision = false;
                this.sourceLooksLikeCOBOL = true;
                this.ImplicitProgramId = "";
            }
        }

        /* if the source has an extension, then continue on reguardless */
        if (hasCOBOLExtension) {
            this.sourceLooksLikeCOBOL = true;
            /* otherwise, does it look like COBOL? */
        } else if (this.sectionsInToken !== 0 || this.divisionsInToken !== 0 || sourceHandler.getCommentCount() > 0) {
            this.sourceLooksLikeCOBOL = true;
        }

        /* leave early */
        if (this.sourceLooksLikeCOBOL === false) {
            return;
        }

        // prepare parser hint information
        this.setupParserHint();

        prevToken = Token.Blank;
        for (let l = 0; l < sourceHandler.getLineCount(); l++) {
            try {
                line = sourceHandler.getLine(l).trimRight();

                // don't parse a empty line
                if (line.length > 0) {
                    if (prevToken.endsWithDot === false) {
                        prevToken = this.parseLineByLine(sourceHandler, l, prevToken, line);
                    }
                    else {
                        prevToken = this.parseLineByLine(sourceHandler, l, Token.Blank, line);
                    }
                }
            }
            catch (e) {
                logException("CobolQuickParse - Parse error", e);
            }
        }

        if (this.programs.length !== 0) {
            for (let cp = 0; cp < this.programs.length; cp++) {
                let currentProgram = this.programs.pop();
                if (currentProgram !== undefined) {
                    currentProgram.endLine = sourceHandler.getLineCount();
                    currentProgram.endColumn = line.length;
                }
            }
        }

        if (this.currentDivision !== COBOLToken.Null) {
            this.currentDivision.endLine = sourceHandler.getLineCount();
            this.currentDivision.endColumn = line.length;
        }

        if (this.currentSection !== COBOLToken.Null) {
            this.currentSection.endLine = sourceHandler.getLineCount();
            this.currentSection.endColumn = line.length;
        }

        if (this.currentParagraph !== COBOLToken.Null) {
            this.currentParagraph.endLine = sourceHandler.getLineCount();
            this.currentParagraph.endColumn = line.length;
        }

        this.addinMissingEndlings(sourceHandler);

        if (this.ImplicitProgramId.length !== 0) {
            let ctoken = this.newCOBOLToken(COBOLTokenStyle.ImplicitProgramId, 0, "", this.ImplicitProgramId, this.ImplicitProgramId, undefined);
            this.callTargets.set(this.ImplicitProgramId, ctoken);
        }

        if (COBOLSettingsHelper.isCachingEnabled(configHandler) && this.sourceLooksLikeCOBOL === true && cacheDirectory.length > 0) {
            this.processAllCopyBooksInSourceFile(cacheDirectory, false);
        }
    }




    public processExternalCopybook(cacheDirectory: string, showError: boolean, sourceCopybook: string) {
        try {
            let copyBookfilename: string = expandLogicalCopyBookToFilenameOrEmpty(sourceCopybook);
            if (copyBookfilename.length !== 0) {
                if (this.processingMap.has(copyBookfilename) === false) {
                    this.processingMap.set(copyBookfilename, copyBookfilename);
                    if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, copyBookfilename)) {
                        //logMessage("   CopyBook: " + key + " => " + copyBookfilename);
                        let filefs_vb = new FileSourceHandler(copyBookfilename, false);
                        let qcp_vb = new COBOLQuickParse(filefs_vb, copyBookfilename, this.configHandler, cacheDirectory);
                        let qcp_symtable: COBOLSymbolTable = COBOLSymbolTableHelper.getCOBOLSymbolTable(qcp_vb);

                        COBOLSymbolTableHelper.saveToFile(cacheDirectory, qcp_symtable);
                    }
                }
            } else {
                if (showError) {
                    logMessage("   CopyBook: " + sourceCopybook + " (not found)");
                }
            }
        }
        catch (ex) {
            logException("processAllCopyBooksInSourceFile:", ex);
        }
    }

    public processAllCopyBooksInSourceFile(cacheDirectory: string, showError: boolean) {

        let filename = this.filename;

        if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, filename)) {
            let qcp_symtable: COBOLSymbolTable = COBOLSymbolTableHelper.getCOBOLSymbolTable(this);

            COBOLSymbolTableHelper.saveToFile(cacheDirectory, qcp_symtable);
        }

        /* iterater through all the known copybook references */
        for (let [key, value] of this.getcopyBooksUsed()) {
            try {
                this.processExternalCopybook(cacheDirectory, showError, key);
            }
            catch (fe) {
                logException("processOneFile", fe);
            }

        }
    }

    private newCOBOLToken(tokenType: COBOLTokenStyle, startLine: number, line: string, token: string,
        description: string, parentToken: COBOLToken | undefined,
        extraInformation: string = ""): COBOLToken {

        let ctoken = new COBOLToken(this.filename, tokenType, startLine, line, token, description, parentToken, this.inProcedureDivision, extraInformation);

        /* if we are in a paragraph update */
        if (this.currentParagraph !== COBOLToken.Null) {
            this.currentParagraph.endLine = startLine;
            if (ctoken.startColumn !== 0) {
                this.currentParagraph.endColumn = ctoken.startColumn - 1;
            }
        }

        // new divison
        if (tokenType === COBOLTokenStyle.Division && this.currentDivision !== COBOLToken.Null) {
            this.currentParagraph = COBOLToken.Null;

            this.currentDivision.endLine = startLine;
            if (ctoken.startColumn !== 0) {
                this.currentDivision.endColumn = ctoken.startColumn - 1;
            }

            if (this.currentSection !== COBOLToken.Null) {
                this.currentSection.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    this.currentSection.endColumn = ctoken.startColumn - 1;
                }
                this.currentSection = COBOLToken.Null;
            }
            this.tokensInOrder.push(ctoken);
            return ctoken;
        }

        // new section
        if (tokenType === COBOLTokenStyle.Section && this.currentSection !== COBOLToken.Null) {
            this.currentParagraph = COBOLToken.Null;
            this.currentSection.endLine = startLine;
            if (ctoken.startColumn !== 0) {
                this.currentSection.endColumn = ctoken.startColumn - 1;
            }
            this.tokensInOrder.push(ctoken);
            return ctoken;
        }

        // new paragraph
        if (tokenType === COBOLTokenStyle.Paragraph) {
            if (this.currentSection !== COBOLToken.Null) {
                this.currentSection.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    this.currentSection.endColumn = ctoken.startColumn - 1;
                }
            }

            this.currentParagraph = ctoken;

            if (this.currentDivision !== COBOLToken.Null) {
                this.currentDivision.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    this.currentDivision.endColumn = ctoken.startColumn - 1;
                }
            }
            this.tokensInOrder.push(ctoken);
            return ctoken;
        }

        this.tokensInOrder.push(ctoken);
        return ctoken;
    }

    private setupParserHint() {
        if (isDirectory(this.parserHintDirectory) === false) {
            this.parserHintDirectory = "";
        } else {
            try {
                let stat = fs.statSync(this.parserHintDirectory);
                if (stat.isDirectory() === false) {
                    this.parserHintDirectory = "";
                }

                let combinedFile = path.join(this.parserHintDirectory, path.basename(this.filename) + ".json");
                if (isFile(combinedFile)) {
                    let statJFile = fs.statSync(combinedFile);
                    if (statJFile.isFile()) {
                        logMessage("INFO: found  " + combinedFile);
                        let jfileContents = fs.readFileSync(combinedFile);
                        let json = JSON.parse(jfileContents.toString());
                        logMessage("json is " + json);
                        logMessage("from : " + jfileContents.toString());

                        if (json.WorkingStorage !== undefined) {
                            this.parseHint_WorkingStorageFiles = json.WorkingStorage;
                        }
                        if (json.LocalStorage !== undefined) {
                            this.parseHint_LocalStorageFiles = json.LocalStorage;
                        }
                        if (json.ScreenSection !== undefined) {
                            this.parseHint_ScreenSectionFiles = json.ScreenSection;
                        }
                        if (json.OnOpen !== undefined) {
                            this.parseHint_OnOpenFiles = json.OnOpen;
                        }
                    }
                }
            }
            catch (e) {
                logException("Exception during parserHint", e);
            }
        }
    }


    public static dumpMetaData(settings: ICOBOLSettings, cacheDirectory: string) {
        if (COBOLSettingsHelper.isCachingEnabled(settings) === false) {
            logMessage("Metadata is not enabled");
            return;
        }

        logMessage("Metadata Dump");

        logMessage(" cache folder   : " + cacheDirectory);
        logMessage(" cache_metadata : " + settings.cache_metadata + "\n");
        logMessage(" Last modified  : " + InMemoryGlobalSymbolCache.lastModifiedTime);
        logMessage(" Dirty flag     : " + InMemoryGlobalSymbolCache.isDirty);
        logMessage("");
        logMessage("Global symbols  : (made lowercase)");

        for (let [i, tag] of InMemoryGlobalSymbolCache.callableSymbols.entries()) {
            let fileSymbol: COBOLFileSymbol[] | undefined = InMemoryGlobalSymbolCache.callableSymbols.get(i);
            if (fileSymbol === undefined) {
                logMessage("  " + i + " => empty");
            } else {
                fileSymbol.forEach(function (value: COBOLFileSymbol) {
                    logMessage(String.Format(" {0} => {1} @ {2}", i.padEnd(40), value.filename, value.lnum));
                });
            }
        }

        logMessage("");
        logMessage("File symbols    :");

        for (var file of fs.readdirSync(cacheDirectory)) {
            if (file !== globalSymbolFilename && file !== fileSymbolFilename && file.endsWith(".sym")) {
                let symTable = COBOLSymbolTableHelper.getSymbolTable_direct(path.join(cacheDirectory, file));
                if (symTable !== undefined) {
                    logMessage(" " + symTable.fileName + " in " + file);
                    logMessage("   Label symbol count    : " + symTable.labelSymbols.size);
                    logMessage("   Variable symbol count : " + symTable.variableSymbols.size);
                }
            }
        }

        if (InMemoryGlobalFileCache.copybookFileSymbols.size !== 0) {
            logMessage("");
            logMessage("Global copybooks:");
            for (let [i, tag] of InMemoryGlobalFileCache.copybookFileSymbols.entries()) {
                logMessage(" " + i);
            }
        }
    }

    private static readonly literalRegex = /^[#a-zA-Z0-9][a-zA-Z0-9-_]*$/g;

    private isValidLiteral(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(COBOLQuickParse.literalRegex)) {
            return true;
        }

        return false;
    }

    private static readonly paragraphRegex = /^[a-zA-Z0-9][a-zA-Z0-9-_]*$/g;

    private isParagraph(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(COBOLQuickParse.paragraphRegex) === null) {
            return false;
        }

        /* paragraph can't be a variable or constant */
        if (this.constantsOrVariables.has(id.toLowerCase()) === true) {
            return false;
        }

        return true;
    }

    private isValidKeyword(keyword: string): boolean {
        return cobolKeywordDictionary.containsKey(keyword);
    }

    private isValidProcedureKeyword(keyword: string): boolean {
        return cobolProcedureKeywordDictionary.containsKey(keyword);
    }

    private isValidStorageKeyword(keyword: string): boolean {
        return cobolStorageKeywordDictionary.containsKey(keyword);
    }

    private isNumber(value: string): boolean {
        try {
            if (value.toString().length === 0) {
                return false;
            }
            return !isNaN(Number(value.toString()));
        }
        catch (e) {
            logException("isNumber(" + value + ")", e);
            return false;
        }
    }

    private trimLiteral(literal: string) {
        let literalTrimmed = literal.trim();

        if (literalTrimmed.length === 0) {
            return literalTrimmed;
        }

        /* remove ( */
        if (literalTrimmed[0] === "(") {
            literalTrimmed = literalTrimmed.substr(1, literalTrimmed.length - 1);
        }

        /* remove  */
        if (literalTrimmed.endsWith(")")) {
            literalTrimmed = literalTrimmed.substr(0, literalTrimmed.length - 1);
        }

        literalTrimmed = literalTrimmed.trim();
        if (literalTrimmed.length === 0) {
            return literalTrimmed;
        }

        /* remove quotes */
        if (literalTrimmed[0] === "\"" && literalTrimmed.endsWith("\"")) {
            return literalTrimmed.substr(1, literalTrimmed.length - 2);
        }
        /* remove quotes */
        if (literalTrimmed[0] === "\'" && literalTrimmed.endsWith("\'")) {
            return literalTrimmed.substr(1, literalTrimmed.length - 2);
        }

        /* remove end . */
        if (literalTrimmed.endsWith(".")) {
            return literalTrimmed.substr(0, literalTrimmed.length - 1);
        }

        return literalTrimmed;
    }

    private relaxedParseLineByLine(sourceHandler: ISourceHandler, lineNumber: number, prevToken: Token, line: string): Token {
        let token = new Token(line, prevToken);
        let tokenCountPerLine = 0;
        do {
            try {
                let endWithDot = false;

                let tcurrent: string = token.currentToken;
                let tcurrentLower: string = token.currentTokenLower;
                tokenCountPerLine++;

                if (tcurrent.endsWith(".")) {
                    tcurrent = tcurrent.substr(0, tcurrent.length - 1);
                    tcurrentLower = tcurrent.toLowerCase();
                    endWithDot = true;
                    token.endsWithDot = endWithDot;
                } else {
                    token.endsWithDot = false;
                }

                if (tokenCountPerLine === 1) {
                    let tokenAsNumber = Number.parseInt(tcurrent);
                    if (tokenAsNumber !== undefined && (!isNaN(tokenAsNumber))) {
                        this.numberTokensInHeader++;
                        continue;
                    }
                }
                switch (tcurrentLower) {
                    case "section":
                        if (token.prevToken.length !== 0) {
                            switch(token.prevTokenLower) {
                                case "working-storage" : this.sectionsInToken++;
                                case "file" : this.sectionsInToken++;
                                case "linkage" : this.sectionsInToken++;
                                case "screen" : this.sectionsInToken++;
                            }
                        }
                        break;
                    case "division":
                        switch (token.prevTokenLower) {
                            case "identification": this.divisionsInToken++; break;
                            case "procedure": this.divisionsInToken++; break;
                        }
                        break;
                    default:
                        if (this.isValidProcedureKeyword(tcurrentLower)) {
                            this.procedureDivisionRelatedTokens++;
                        }

                        if (this.isValidStorageKeyword(tcurrentLower)) {
                            this.workingStorageRelatedTokens++;
                        }

                        break;
                }
                // continue now
                if (tcurrent.length === 0) {
                    continue;
                }
            }
            catch (e) {
                logException("Cobolquickparse relaxedParseLineByLine line error: ", e);
            }
        }
        while (token.moveToNextToken() === false);

        return token;
    }

    private addReference(referencesMap: Map<string, SourceReference[]>, lowerCaseVariable: string, line: number, column: number) {
        if (referencesMap.has(lowerCaseVariable)) {
            let sourceRefs: SourceReference[] | undefined = referencesMap.get(lowerCaseVariable);
            if (sourceRefs !== undefined) {
                sourceRefs.push(new SourceReference(this.sourceFileId, line, column));
                return;
            }
        }

        let sourceRefs: SourceReference[] = [];
        sourceRefs.push(new SourceReference(this.sourceFileId, line, column));
        referencesMap.set(lowerCaseVariable, sourceRefs);
    }

    private addVariableOrConstant(lowerCaseVariable: string, token: COBOLToken) {
        if (this.constantsOrVariables.has(lowerCaseVariable)) {
            let tokens: COBOLToken[] | undefined = this.constantsOrVariables.get(lowerCaseVariable);
            if (tokens !== undefined) {
                tokens.push(token);
                return;
            }
        }

        let tokens: COBOLToken[] = [];
        tokens.push(token);
        this.constantsOrVariables.set(lowerCaseVariable, tokens);
    }

    private parseLineByLine(sourceHandler: ISourceHandler, lineNumber: number, prevToken: Token, line: string): Token {
        let token = new Token(line, prevToken);

        do {
            try {
                let endWithDot = false;

                let tcurrent: string = token.currentToken;
                let tcurrentLower: string = token.currentTokenLower;

                // continue now
                if (tcurrent.length === 0) {
                    continue;
                }

                // HACK for "set x to entry"
                if (token.prevTokenLower === "to" && tcurrentLower === "entry") {
                    token.moveToNextToken();
                    continue;
                }

                if (tcurrent.endsWith(",")) {
                    tcurrent = tcurrent.substr(0, tcurrent.length - 1);
                    tcurrentLower = tcurrent.toLowerCase();
                } else if (tcurrent.endsWith(".")) {
                    tcurrent = tcurrent.substr(0, tcurrent.length - 1);
                    tcurrentLower = tcurrent.toLowerCase();
                    endWithDot = true;
                    token.endsWithDot = endWithDot;
                } else {
                    token.endsWithDot = false;
                }

                // if skiptodot and not the end of the statement.. swallow
                if (this.skipToDot && endWithDot === false) {
                    if (this.addReferencesDuringSkipToTag) {
                        if ((this.isValidKeyword(tcurrentLower) === false) && (this.isValidLiteral(tcurrentLower))) {
                            let trimToken = this.trimLiteral(tcurrent);
                            if (this.sourceReferences !== undefined) {
                                // no forward validation can be done, as this is a one pass scanner
                                this.addReference(this.sourceReferences.targetReferences, trimToken.toLocaleLowerCase(), lineNumber, token.currentCol);
                            }
                        }
                    }
                    continue;
                }

                // reset
                if (this.skipToDot && endWithDot === true) {
                    this.skipToDot = false;
                    this.addReferencesDuringSkipToTag = false;
                }


                const current: string = tcurrent;
                const currentLower: string = tcurrentLower;
                const nextToken = token.nextToken;
                const nextTokenLower = token.nextTokenLower;
                const prevToken = this.trimLiteral(token.prevToken);
                const prevTokenLower = this.trimLiteral(token.prevTokenLower);
                const nextPlusOneToken = token.nextPlusOneToken;

                const prevPlusCurrent = token.prevToken + " " + current;

                if (currentLower === "exec") {
                    this.currentToken = this.newCOBOLToken(COBOLTokenStyle.Exec, lineNumber, line, prevToken, "", this.currentDivision);
                    continue;
                }

                /* finish processing end-exec */
                if (currentLower === "end-exec") {
                    this.currentToken = COBOLToken.Null;
                    endWithDot = true;
                    token.endsWithDot = endWithDot;
                    continue;
                }

                /* skip everything in between exec .. end-exec */
                if (this.currentToken.tokenType === COBOLTokenStyle.Exec) {
                    continue;
                }

                if (prevTokenLower === "end" && currentLower === "declaratives") {
                    this.declaratives.endLine = lineNumber;
                    this.declaratives.endColumn = line.indexOf(tcurrent);
                    this.tokensInOrder.push(this.declaratives);
                    this.inDeclaratives = false;
                    this.declaratives = COBOLToken.Null;
                    continue;
                }

                // only include sections in the declaratives area
                if (this.inDeclaratives) {
                    if (currentLower === 'section') {
                        this.newCOBOLToken(COBOLTokenStyle.DeclarativesSection, lineNumber, line, prevToken, prevToken, this.currentDivision);
                    }
                    continue;
                }
                // handle sections)
                if (this.currentClass === COBOLToken.Null && prevToken.length !== 0 && currentLower === "section" && (prevTokenLower !== 'exit')) {
                    if (prevTokenLower === "declare") {
                        continue;
                    }

                    // So we need to insert a fake data division?
                    if (this.currentDivision === COBOLToken.Null) {
                        if (prevTokenLower === 'file' ||
                            prevTokenLower === 'working-storage' ||
                            prevTokenLower === 'local-storage' ||
                            prevTokenLower === 'screen' ||
                            prevTokenLower === 'linkage') {

                            this.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Data Division", "Data", "Data Division (Optional)", this.currentDivision);
                        }
                    }

                    this.currentSection = this.newCOBOLToken(COBOLTokenStyle.Section, lineNumber, line, prevToken, prevPlusCurrent, this.currentDivision);
                    this.sections.set(prevTokenLower, this.currentSection);
                    this.current01Group = COBOLToken.Null;
                    this.currentLevel = COBOLToken.Null;

                    if (prevTokenLower === "working-storage" || prevTokenLower === "linkage" ||
                        prevTokenLower === "local-storage" || prevTokenLower === "file-control" ||
                        prevTokenLower === 'file' || prevTokenLower === "screen") {
                        this.pickFields = true;
                        this.inProcedureDivision = false;
                        sourceHandler.setDumpAreaA(false);
                        sourceHandler.setDumpAreaBOnwards(!this.parseColumnBOnwards);
                    }

                    continue;
                }

                // handle divisions
                if (this.captureDivisions && prevTokenLower.length !== 0 && currentLower === "division") {
                    this.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, line, prevToken, prevPlusCurrent, COBOLToken.Null);

                    if (prevTokenLower === "procedure") {
                        this.inProcedureDivision = true;
                        this.pickFields = false;
                        this.procedureDivision = this.currentDivision;
                        sourceHandler.setDumpAreaA(true);
                        sourceHandler.setDumpAreaBOnwards(false);
                    }

                    continue;
                }

                // handle entries
                if (prevTokenLower === "entry" && current.length !== 0) {
                    let trimmedCurrent = this.trimLiteral(current);
                    let ctoken = this.newCOBOLToken(COBOLTokenStyle.EntryPoint, lineNumber, line, trimmedCurrent, prevPlusCurrent, this.currentDivision);
                    this.callTargets.set(trimmedCurrent, ctoken);
                    continue;
                }

                // handle program-id
                if (prevTokenLower === "program-id" && current.length !== 0) {
                    let trimmedCurrent = this.trimLiteral(current);
                    let ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, line, trimmedCurrent, prevPlusCurrent, this.currentDivision);
                    this.programs.push(ctoken);
                    if (trimmedCurrent.indexOf(" ") !== -1 && token.isTokenPresent("external") === false) {
                        this.callTargets.set(trimmedCurrent, ctoken);
                    }
                    this.ImplicitProgramId = "";        /* don't need it */

                    // So we don't have any division?
                    // if (this.currentDivision === COBOLToken.Null) {
                    //     this.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Identification Division", "Identification","Identification Division (Optional)", this.currentDivision);
                    // }
                    continue;
                }

                // handle class-id
                if (prevTokenLower === "class-id" && current.length !== 0) {
                    let trimmedCurrent = this.trimLiteral(current);
                    this.currentClass = this.newCOBOLToken(COBOLTokenStyle.ClassId, lineNumber, line, trimmedCurrent, prevPlusCurrent, this.currentDivision);
                    this.captureDivisions = false;
                    this.currentMethod = COBOLToken.Null;
                    this.pickFields = true;
                    this.classes.set(trimmedCurrent, this.currentClass);

                    continue;
                }

                // handle "end class, enum, valuetype"
                if (this.currentClass !== COBOLToken.Null && prevTokenLower === "end" &&
                    (currentLower === "class" || currentLower === "enum" || currentLower === "valuetype")) {
                    this.currentClass.endLine = lineNumber;
                    this.currentClass.endColumn = line.toLocaleLowerCase().indexOf(currentLower) + currentLower.length;

                    this.currentClass = COBOLToken.Null;
                    this.captureDivisions = true;
                    this.currentMethod = COBOLToken.Null;
                    this.pickFields = false;
                    continue;
                }

                // handle enum-id
                if (prevTokenLower === "enum-id" && current.length !== 0) {
                    this.currentClass = this.newCOBOLToken(COBOLTokenStyle.EnumId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, COBOLToken.Null);

                    this.captureDivisions = false;
                    this.currentMethod = COBOLToken.Null;
                    this.pickFields = true;
                    continue;
                }

                // handle interface-id
                if (prevTokenLower === "interface-id" && current.length !== 0) {
                    this.currentClass = this.newCOBOLToken(COBOLTokenStyle.InterfaceId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);

                    this.pickFields = true;
                    this.captureDivisions = false;
                    this.currentMethod = COBOLToken.Null;
                    continue;
                }

                // handle valuetype-id
                if (prevTokenLower === "valuetype-id" && current.length !== 0) {
                    this.currentClass = this.newCOBOLToken(COBOLTokenStyle.ValueTypeId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);

                    this.pickFields = true;
                    this.captureDivisions = false;
                    this.currentMethod = COBOLToken.Null;
                    continue;
                }

                // handle function-id
                if (prevTokenLower === "function-id" && current.length !== 0) {
                    this.currentFunctionId = this.newCOBOLToken(COBOLTokenStyle.FunctionId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);
                    this.captureDivisions =
                        this.pickFields = true;
                    continue;
                }

                // handle method-id
                if (prevTokenLower === "method-id" && current.length !== 0) {
                    let currentLowerTrim = this.trimLiteral(currentLower);
                    let style = currentLowerTrim === "new" ? COBOLTokenStyle.Constructor : COBOLTokenStyle.MethodId;

                    if (nextTokenLower === "property") {
                        let trimmedProperty = this.trimLiteral(nextPlusOneToken);
                        this.currentMethod = this.newCOBOLToken(COBOLTokenStyle.Property, lineNumber, line, trimmedProperty, nextToken + " " + nextPlusOneToken, this.currentDivision);
                        this.methods.set(trimmedProperty, this.currentMethod);
                    } else {
                        let trimmedCurrent = this.trimLiteral(current);
                        this.currentMethod = this.newCOBOLToken(style, lineNumber, line, trimmedCurrent, prevPlusCurrent, this.currentDivision);
                        this.methods.set(trimmedCurrent, this.currentMethod);
                    }

                    this.pickFields = true;
                    this.captureDivisions = false;
                    continue;
                }

                // handle "end method"
                if (this.currentMethod !== COBOLToken.Null && prevTokenLower === "end" && currentLower === "method") {
                    this.currentMethod.endLine = lineNumber;
                    this.currentMethod.endColumn = line.toLocaleLowerCase().indexOf(currentLower) + currentLower.length;

                    this.currentMethod = COBOLToken.Null;
                    this.pickFields = false;
                    continue;
                }

                if (this.programs.length !== 0 && prevTokenLower === "end" && currentLower === "program") {
                    let currentProgram: COBOLToken | undefined = this.programs.pop();
                    if (currentProgram !== undefined) {
                        currentProgram.endLine = lineNumber;
                        currentProgram.endColumn = line.length;
                    }

                    if (this.currentDivision !== COBOLToken.Null) {
                        this.currentDivision.endLine = lineNumber;
                        this.currentDivision.endColumn = line.length;
                    }

                    if (this.currentSection !== COBOLToken.Null) {
                        this.currentSection.endLine = lineNumber;
                        this.currentSection.endColumn = line.length;
                    }

                    if (this.currentParagraph !== COBOLToken.Null) {
                        this.currentParagraph.endLine = lineNumber;
                        this.currentParagraph.endColumn = line.length;
                    }

                    this.currentDivision = COBOLToken.Null;
                    this.currentSection = COBOLToken.Null;
                    this.currentParagraph = COBOLToken.Null;
                    this.pickFields = false;
                    continue;
                }

                // handle "end function"
                if (this.currentFunctionId !== COBOLToken.Null && prevTokenLower === "end" && currentLower === "function") {
                    this.currentFunctionId.endLine = lineNumber;
                    this.currentFunctionId.endColumn = line.toLocaleLowerCase().indexOf(currentLower) + currentLower.length;
                    this.newCOBOLToken(COBOLTokenStyle.EndFunctionId, lineNumber, line, prevToken, current, this.currentDivision);

                    this.pickFields = false;
                    this.inProcedureDivision = false;

                    if (this.currentDivision !== COBOLToken.Null) {
                        this.currentDivision.endLine = lineNumber;
                        this.currentDivision.endColumn = 0;
                    }

                    if (this.currentSection !== COBOLToken.Null) {
                        this.currentSection.endLine = lineNumber;
                        this.currentSection.endColumn = 0;
                    }
                    this.currentDivision = COBOLToken.Null;
                    this.currentSection = COBOLToken.Null;
                    this.currentParagraph = COBOLToken.Null;
                    this.procedureDivision = COBOLToken.Null;
                    continue;
                }


                if (currentLower === "declaratives") {
                    this.declaratives = this.newCOBOLToken(COBOLTokenStyle.Declaratives, lineNumber, line, prevToken, current, this.currentDivision);
                    this.inDeclaratives = true;
                    this.tokensInOrder.pop();       /* only interested it at the end */
                    continue;
                }

                // copybook handling
                if (prevTokenLower === "copy" && current.length !== 0) {
                    let trimmedCopyBook = this.trimLiteral(current);
                    let newCopybook: boolean = false;
                    if (this.copyBooksUsed.has(trimmedCopyBook) === false) {
                        this.copyBooksUsed.set(trimmedCopyBook, current);
                        newCopybook = true;
                    }

                    if (this.copybookNestedInSection) {
                        if (this.currentSection !== COBOLToken.Null) {
                            this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, prevPlusCurrent, prevPlusCurrent, this.currentSection);
                        } else {
                            this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, prevPlusCurrent, prevPlusCurrent, this.currentDivision);
                        }
                    } else {
                        this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, prevPlusCurrent, prevPlusCurrent, this.currentDivision);
                    }

                    if (this.sourceReferences !== undefined && newCopybook) {
                        let fileName = expandLogicalCopyBookToFilenameOrEmpty(trimmedCopyBook);
                        if (fileName.length > 0) {
                            let qfile = new FileSourceHandler(fileName, false);
                            let qps = new COBOLQuickParse(qfile, fileName, this.configHandler, "", this.sourceReferences);
                        }
                    }
                    continue;
                }

                // we are in the procedure division
                if (this.captureDivisions && this.currentDivision !== COBOLToken.Null &&
                    this.currentDivision === this.procedureDivision && endWithDot) {
                    if (!this.isValidKeyword(prevTokenLower) && !this.isValidKeyword(currentLower)) {
                        let beforeCurrent = line.substr(0, token.currentCol - 1).trim();
                        if (beforeCurrent.length === 0) {
                            let c = tcurrent;
                            if (c.length !== 0) {
                                if (this.isParagraph(c)) {
                                    if (this.currentSection !== COBOLToken.Null) {
                                        let newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, c, c, this.currentSection);
                                        this.paragraphs.set(newToken.tokenNameLower, newToken);
                                    } else {
                                        let newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, c, c, this.currentDivision);
                                        this.paragraphs.set(newToken.tokenNameLower, newToken);
                                    }
                                }
                            }
                        }
                    }
                }

                if (this.currentSection.tokenNameLower === 'input-output') {
                    if (prevTokenLower === 'fd' || prevTokenLower === 'select') {
                        this.pickFields = true;
                    }
                }

                // are we in the working-storage section?
                if (this.pickFields) {
                    /* only interesting in things that are after a number */
                    if (this.isNumber(prevToken) && !this.isNumber(current)) {
                        if (!this.isValidKeyword(prevTokenLower) && !this.isValidKeyword(currentLower)) {
                            let trimToken = this.trimLiteral(current);
                            if (this.isValidLiteral(currentLower)) {
                                let style = prevToken === "78" ? COBOLTokenStyle.Constant : COBOLTokenStyle.Variable;
                                let extraInfo = prevToken;
                                if (prevToken === '01' || prevToken === '1') {
                                    if (nextTokenLower.length === 0) {
                                        extraInfo += "-GROUP";
                                    }
                                    else if (this.currentSection.tokenNameLower === 'report') {
                                        extraInfo += "-GROUP";
                                    }

                                    if (token.isTokenPresent("constant")) {
                                        style = COBOLTokenStyle.Constant;
                                    }
                                }
                                let ctoken = this.newCOBOLToken(style, lineNumber, line, trimToken, trimToken, this.currentDivision, extraInfo);
                                this.addVariableOrConstant(currentLower, ctoken);

                                // place the 88 under the 01 item
                                if (this.currentLevel !== COBOLToken.Null && prevToken === '88') {
                                    this.currentLevel.endLine = ctoken.startLine;
                                    this.currentLevel.endColumn = ctoken.startColumn + ctoken.tokenName.length;
                                }

                                if (prevToken !== '88') {
                                    this.currentLevel = ctoken;
                                }

                                if (prevToken === '01' || prevToken === '1' ||
                                    prevToken === '66' || prevToken === '77' || prevToken === '78') {
                                    if (nextTokenLower.length === 0 ||
                                        (this.currentSection.tokenNameLower === "report" && nextTokenLower === "type")) {
                                        this.current01Group = ctoken;
                                    } else {
                                        this.current01Group = COBOLToken.Null;
                                    }
                                }

                                if (this.current01Group !== COBOLToken.Null) {
                                    this.current01Group.endLine = ctoken.startLine;
                                    this.current01Group.endColumn = ctoken.startColumn + ctoken.tokenName.length;
                                }

                                /* if spans multiple lines, skip to dot */
                                if (endWithDot === false) {
                                    this.skipToDot = true;
                                }
                            }
                        }
                        continue;
                    }

                    if ((prevTokenLower === "fd"
                        || prevTokenLower === "sd"
                        || prevTokenLower === "rd"
                        || prevTokenLower === "select")
                        && !this.isValidKeyword(currentLower)) {
                        let trimToken = this.trimLiteral(current);

                        if (this.isValidLiteral(currentLower)) {
                            let variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, trimToken, trimToken, this.currentDivision, prevTokenLower);
                            this.addVariableOrConstant(currentLower, variableToken);
                        }

                        if (prevTokenLower === "rd" || prevTokenLower === 'select') {
                            if (prevTokenLower === 'select') {
                                this.addReferencesDuringSkipToTag = true;
                            }
                            this.skipToDot = true;
                        }
                        continue;
                    }

                    if (prevTokenLower === "indexed" && currentLower === "by" && nextToken.length > 0) {
                        if (this.isValidKeyword(nextTokenLower) === false) {
                            let trimmedNextToken = this.trimLiteral(nextToken);
                            let variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, trimmedNextToken, trimmedNextToken, this.currentDivision);
                            this.addVariableOrConstant(trimmedNextToken.toLowerCase(), variableToken);
                        }
                    }
                }

                /* add reference when perform is used */
                if (this.parseReferences && this.sourceReferences !== undefined) {
                    if (this.inProcedureDivision) {
                        let trimmedCurrentLower = this.trimLiteral(currentLower);
                        if (this.isNumber(trimmedCurrentLower) === true) {
                            continue;
                        }

                        if (prevTokenLower === 'perform' || prevTokenLower === "to" || prevTokenLower === "goto") {
                            /* go nn, could be "move xx to nn" or "go to nn" */
                            if (this.constantsOrVariables.has(trimmedCurrentLower) === false && this.isValidKeyword(trimmedCurrentLower) === false) {
                                this.addReference(this.sourceReferences.targetReferences, trimmedCurrentLower, lineNumber, token.currentCol);
                                continue;
                            }
                        }

                        /* is this a reference to a variable? */
                        if (this.constantsOrVariables.has(trimmedCurrentLower) === true) {
                            this.addReference(this.sourceReferences.constantsOrVariablesReferences, trimmedCurrentLower, lineNumber, token.currentCol);
                            continue;
                        }
                    }
                }
            }
            catch (e) {
                logException("Cobolquickparse line error: ", e);
            }
        }
        while (token.moveToNextToken() === false);

        return token;
    }

    public getcopyBooksUsed(): Map<string, string> {
        return this.copyBooksUsed;
    }

    private addinMissingEndlings(sourceHandler: ISourceHandler) {
        for (let i = 0; i < this.tokensInOrder.length; i++) {
            let token = this.tokensInOrder[i];

            if (token.endLine === 0) {
                token.endLine = token.startLine;
                token.endColumn = token.startColumn + token.tokenName.length;
            }
        }
    }
}

export class COBOLFileSymbol {
    public filename: string | undefined;
    public lnum: number | undefined;

    public constructor(symbol?: string, lineNumber?: number) {
        this.filename = symbol;
        this.lnum = lineNumber;
    }

    static fromJSON(d: Object): COBOLFileSymbol {
        return Object.assign(new COBOLFileSymbol(), d);
    }
}

export class COBOLSymbol {
    public symbol: string | undefined;
    public lnum: number | undefined;

    public constructor(symbol?: string, lineNumber?: number) {
        this.symbol = symbol;
        this.lnum = lineNumber;
    }

    static fromJSON(d: Object): COBOLSymbol {
        return Object.assign(new COBOLSymbol(), d);
    }
}

// JSON callbacks to Map to something that can be serialised
function replacer(this: any, key: any, value: any) {
    const originalObject = this[key];
    if (originalObject instanceof Map) {
        return {
            dataType: 'Map',
            value: Array.from(originalObject.entries()), // or with spread: value: [...originalObject]
        };
    } else {
        return value;
    }
}

function reviver(key: any, value: any) {
    if (typeof value === 'object' && value !== null) {
        if (value.dataType === 'Map') {
            return new Map<string, COBOLSymbol>(value.value);
        }
    }
    return value;
}

export class COBOLGlobalFileTable {
    public lastModifiedTime: number = 0;
    public isDirty: boolean = false;
    public callableFileSymbols: Map<string, COBOLFileSymbol[]>;
    public copybookFileSymbols: Map<string, COBOLFileSymbol[]>;

    public constructor() {
        this.callableFileSymbols = new Map<string, COBOLFileSymbol[]>();
        this.copybookFileSymbols = new Map<string, COBOLFileSymbol[]>();
    }
}

export class COBOLGlobalSymbolTable {
    public lastModifiedTime: number = 0;
    public callableSymbols: Map<string, COBOLFileSymbol[]>;
    public isDirty: boolean = false;
    public classSymbols: Map<string, COBOLFileSymbol[]>;
    public methodSymbols: Map<string, COBOLFileSymbol[]>;

    public constructor() {
        this.callableSymbols = new Map<string, COBOLFileSymbol[]>();
        this.classSymbols = new Map<string, COBOLFileSymbol[]>();
        this.methodSymbols = new Map<string, COBOLFileSymbol[]>();
    }

    static fromJSON(d: Object): COBOLGlobalSymbolTable {
        return Object.assign(new COBOLGlobalSymbolTable(), d);
    }
}

export class COBOLSymbolTable {
    public lastModifiedTime: number = 0;
    public fileName: string = "";

    public variableSymbols: Map<string, COBOLSymbol>;
    public labelSymbols: Map<string, COBOLSymbol>;

    public constructor() {
        this.variableSymbols = new Map<string, COBOLSymbol>();
        this.labelSymbols = new Map<string, COBOLSymbol>();
    }

    static fromJSON(d: Object): COBOLSymbolTable {
        return Object.assign(new COBOLSymbolTable(), d);
    }
}

export class COBOLSymbolTableHelper {
    public static getCOBOLSymbolTable(qp: COBOLQuickParse): COBOLSymbolTable {
        let st = new COBOLSymbolTable();
        st.fileName = qp.filename;
        st.lastModifiedTime = qp.lastModifiedTime;

        for (let [key, value] of qp.copyBooksUsed) {
            let fileName = expandLogicalCopyBookToFilenameOrEmpty(key);
            InMemoryGlobalCachesHelper.addCopyBookFilename(fileName);
        }

        for (let i = 0; i < qp.tokensInOrder.length; i++) {
            let token = qp.tokensInOrder[i];
            switch (token.tokenType) {
                case COBOLTokenStyle.Constant:
                    st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.Variable:
                    st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.Paragraph:
                    st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.Section:
                    st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    break;
                case COBOLTokenStyle.ImplicitProgramId:
                    InMemoryGlobalCachesHelper.addSymbol(st.fileName, token.tokenNameLower, token.startLine);
                    break;
                case COBOLTokenStyle.ProgramId:
                    InMemoryGlobalCachesHelper.addSymbol(st.fileName, token.tokenNameLower, token.startLine);
                    break;
                case COBOLTokenStyle.EntryPoint:
                    InMemoryGlobalCachesHelper.addSymbol(st.fileName, token.tokenNameLower, token.startLine);
                    break;
                case COBOLTokenStyle.InterfaceId:
                    InMemoryGlobalCachesHelper.addClassSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
                case COBOLTokenStyle.EnumId:
                    InMemoryGlobalCachesHelper.addClassSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
                case COBOLTokenStyle.ClassId:
                    InMemoryGlobalCachesHelper.addClassSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
                case COBOLTokenStyle.MethodId:
                    InMemoryGlobalCachesHelper.addMethodSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
            }
        }
        //logCOBOLChannelLine("- Creating symbol table for " + st.fileName + ", symbols found: " + qp.tokensInOrder.length + " -> " + (performance.now() - startTime).toFixed(2));
        return st;
    }


    private static getHashForFilename(filename: string) {
        let hash: Hash = crypto.createHash('sha256');
        hash.update(filename);
        return hash.digest('hex');
    }

    public static saveToFile(cacheDirectory: string, st: COBOLSymbolTable) {
        let fn = path.join(cacheDirectory, this.getHashForFilename(st.fileName) + ".sym");

        fs.writeFileSync(fn, lzjs.compress(JSON.stringify(st, replacer)));
    }

    public static cacheUpdateRequired(cacheDirectory: string, nfilename: string): boolean {
        let filename = path.normalize(nfilename);

        // check memory first
        if (InMemorySymbolCache.has(filename)) {
            let cachedTable: COBOLSymbolTable | undefined = InMemorySymbolCache.get(filename);
            if (cachedTable !== undefined) {

                /* is the cache table still valid? */
                let stat4src = fs.statSync(filename);
                if (stat4src.mtimeMs === cachedTable.lastModifiedTime) {
                    return false;
                }
                return true;
            }
        }

        let fn: string = path.join(cacheDirectory, this.getHashForFilename(filename) + ".sym");
        if (isFile(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            let stat4src = fs.statSync(filename);
            if (stat4cache.mtimeMs < stat4src.mtimeMs) {
                return true;
            }
            return false;
        }

        return true;
    }

    public static getSymbolTableGivenFile(cacheDirectory: string, nfilename: string): COBOLSymbolTable | undefined {
        let filename = path.normalize(nfilename);
        if (InMemorySymbolCache.has(filename)) {
            let cachedTable: COBOLSymbolTable | undefined = InMemorySymbolCache.get(filename);
            if (cachedTable !== undefined) {

                /* is the cache table still valid? */
                let stat4src = fs.statSync(filename);
                if (stat4src.mtimeMs === cachedTable.lastModifiedTime) {
                    return cachedTable;
                }
                InMemorySymbolCache.delete(filename);       /* drop the invalid cache */
            }
        }

        let fn: string = path.join(cacheDirectory, this.getHashForFilename(filename) + ".sym");
        if (isFile(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            let stat4src = fs.statSync(filename);
            if (stat4cache.mtimeMs < stat4src.mtimeMs) {
                // never return a out of date cache
                fs.unlinkSync(fn);
                return undefined;
            }
            let str: string = fs.readFileSync(fn).toString();
            let cachableTable = JSON.parse(lzjs.decompress(str), reviver);
            InMemorySymbolCache.set(filename, cachableTable);
            return cachableTable;
        }
        return undefined;
    }

    public static getSymbolTable_direct(nfilename: string): COBOLSymbolTable | undefined {
        let str: string = fs.readFileSync(nfilename).toString();
        return JSON.parse(lzjs.decompress(str), reviver);
    }

}

export class InMemoryGlobalCachesHelper {

    public static loadInMemoryGlobalSymbolCaches(cacheDirectory: string) {
        //let symbolDir = COBOLSymbolTableHelper.getCacheDirectory();
        let fn: string = path.join(cacheDirectory, globalSymbolFilename);
        if (isFile(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            if (stat4cache.mtimeMs !== InMemoryGlobalSymbolCache.lastModifiedTime) {
                try {

                    let str: string = fs.readFileSync(fn).toString();
                    let cachableTable: COBOLGlobalSymbolTable = JSON.parse(lzjs.decompress(str), reviver);
                    InMemoryGlobalSymbolCache.callableSymbols = cachableTable.callableSymbols;
                    InMemoryGlobalSymbolCache.lastModifiedTime = stat4cache.mtimeMs;
                    InMemoryGlobalSymbolCache.isDirty = false;
                    return true;
                }
                catch (e) {
                    fs.unlinkSync(fn);
                    return false;
                }
            }
            return false;
        }
    }

    public static loadInMemoryGlobalFileCache(cacheDirectory: string) {
        let fn: string = path.join(cacheDirectory, fileSymbolFilename);

        if (isFile(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            if (stat4cache.mtimeMs !== InMemoryGlobalFileCache.lastModifiedTime) {
                try {

                    let str: string = fs.readFileSync(fn).toString();
                    let cachableTable: COBOLGlobalFileTable = JSON.parse(lzjs.decompress(str), reviver);
                    InMemoryGlobalFileCache.callableFileSymbols = cachableTable.callableFileSymbols;
                    InMemoryGlobalFileCache.copybookFileSymbols = cachableTable.copybookFileSymbols;
                    InMemoryGlobalFileCache.lastModifiedTime = stat4cache.mtimeMs;
                    InMemoryGlobalFileCache.isDirty = false;
                    return true;
                }
                catch (e) {
                    fs.unlinkSync(fn);
                    return false;
                }
            }
            return false;
        }

    }

    public static saveInMemoryGlobalCaches(cacheDirectory: string) {

        if (InMemoryGlobalSymbolCache.isDirty) {
            let fnGlobalSymbolFilename: string = path.join(cacheDirectory, globalSymbolFilename);
            fs.writeFileSync(fnGlobalSymbolFilename, lzjs.compress(JSON.stringify(InMemoryGlobalSymbolCache, replacer)));
            InMemoryGlobalSymbolCache.isDirty = false;
        }

        if (InMemoryGlobalFileCache.isDirty) {
            let fnFileSymbolFilename: string = path.join(cacheDirectory, fileSymbolFilename);
            fs.writeFileSync(fnFileSymbolFilename, lzjs.compress(JSON.stringify(InMemoryGlobalFileCache, replacer)));
            InMemoryGlobalFileCache.isDirty = false;
        }
    }

    private static addSymbolToCache(srcfilename: string, symbolUnchanged: string, lineNumber: number, symbolsCache: Map<string, COBOLFileSymbol[]>) {
        let symbol = symbolUnchanged.toLowerCase();
        if (symbolsCache.has(symbol)) {
            let symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(symbol);

            /* search the list of COBOLFileSymbols */
            if (symbolList !== undefined) {
                let found: boolean = false;
                for (let i = 0; i < symbolList.length; i++) {
                    if (symbolList[i].filename === srcfilename && symbolList[i].lnum === lineNumber) {
                        found = true;
                        break;
                    }
                }
                // not found?
                if (found === false) {
                    symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
                    InMemoryGlobalSymbolCache.isDirty = true;
                }
            }
        }
        let symbolList = [];
        symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
        symbolsCache.set(symbol, symbolList);
        InMemoryGlobalSymbolCache.isDirty = true;
        return;
    }

    private static addSymbolFileToCache(srcfilename: string, symbolsCache: Map<string, COBOLFileSymbol[]>) {
        let symbol: string = srcfilename, lineNumber: number = 0;
        if (symbolsCache.has(symbol)) {
            let symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(symbol);

            /* search the list of COBOLFileSymbols */
            if (symbolList !== undefined) {
                let found: boolean = false;
                for (let i = 0; i < symbolList.length; i++) {
                    if (symbolList[i].filename === srcfilename && symbolList[i].lnum === lineNumber) {
                        found = true;
                        break;
                    }
                }
                // not found?
                if (found === false) {
                    symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
                    InMemoryGlobalFileCache.isDirty = true;
                }
            }
        }
        let symbolList = [];
        symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
        symbolsCache.set(symbol, symbolList);
        InMemoryGlobalFileCache.isDirty = true;
        return;
    }

    public static addClassSymbol(srcfilename: string, symbolUnchanged: string, lineNumber: number) {
        let symbolsCache = InMemoryGlobalSymbolCache.classSymbols;

        InMemoryGlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, symbolsCache);
    }

    public static addMethodSymbol(srcfilename: string, symbolUnchanged: string, lineNumber: number) {
        let symbolsCache = InMemoryGlobalSymbolCache.methodSymbols;
        InMemoryGlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, symbolsCache);
    }

    public static addSymbol(srcfilename: string, symbolUnchanged: string, lineNumber: number) {
        let symbolsCache = InMemoryGlobalSymbolCache.callableSymbols;
        InMemoryGlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, symbolsCache);
    }

    public static addCopyBookFilename(srcfilename: string) {
        let symbolsCache = InMemoryGlobalFileCache.copybookFileSymbols;
        InMemoryGlobalCachesHelper.addSymbolFileToCache(srcfilename, symbolsCache);
    }

    public static getGlobalSymbolCache(): COBOLGlobalSymbolTable {
        return InMemoryGlobalSymbolCache;
    }
}

const globalSymbolFilename: string = "globalsymbols.sym";
const fileSymbolFilename: string = "filesymbols.sym";

const InMemorySymbolCache: Map<string, COBOLSymbolTable> = new Map<string, COBOLSymbolTable>();
const InMemoryGlobalSymbolCache: COBOLGlobalSymbolTable = new COBOLGlobalSymbolTable();
export const InMemoryGlobalFileCache: COBOLGlobalFileTable = new COBOLGlobalFileTable();
