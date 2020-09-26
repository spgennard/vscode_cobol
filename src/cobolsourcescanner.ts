/* eslint-disable @typescript-eslint/no-explicit-any */
import ISourceHandler, { ICommentCallback } from "./isourcehandler";
import { cobolKeywordDictionary, cobolProcedureKeywordDictionary, cobolStorageKeywordDictionary } from "./keywords/cobolKeywords";

import { FileSourceHandler } from "./filesourcehandler";

import * as fs from 'fs';
import * as path from 'path';

import { logMessage, logException } from "./extension";

import { expandLogicalCopyBookToFilenameOrEmpty } from "./opencopybook";
import { ICOBOLSettings } from "./iconfiguration";
import { Uri, window } from "vscode";
import { getCOBOLSourceFormat, ESourceFormat } from "./margindecorations";
import { CobolLinterProvider } from "./cobollinter";
import { clearCOBOLCache } from "./vscobolscanner";
import { COBOLSymbolTableHelper, COBOLSymbolTable, InMemoryGlobalSymbolCache,  COBOLFileSymbol } from "./cobolglobalcache";

export enum COBOLTokenStyle {
    CopyBook = "Copybook",
    CopyBookIn = "CopybookIn",
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

export enum CobolDocStyle {
    unknown = "unknown",
    MSDN = "MSDN",
    COBOLDOC = "COBOLDOC",
    ISCOBOL = "ISCOBOL",
    FUJITSU = "FUJITSU"
}

export enum CobolTagStyle {
    unknown = "unknown",
    FREE = "FREE",
    MICROFOCUS = "MICROFOCUS"
}

export function camelize(text: string): string {
    let ret = "";
    let uppercaseNext = true;
    for (let c = 0; c < text.length; c++) {
        const ch = text[c];
        if (uppercaseNext) {
            ret += ch.toUpperCase();
            uppercaseNext = false;
        } else {
            if (ch === '-' || ch === '_') {
                uppercaseNext = true;
            }

            ret += ch.toLocaleLowerCase();
        }
    }

    return ret;
}

export function splitArgument(input: string, splitBrackets: boolean): string[] {
    const ret: string[] = [];
    let inQuote = false;
    let inQuoteSingle = false;
    const lineLength = input.length;
    let cArg = "";

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

        if (splitBrackets) {
            if (c === '(' || c === ')') {
                ret.push(cArg);
                cArg = "" + c;
                ret.push(cArg);
                cArg = "";
                continue;
            }

        }
        cArg += c;
    }

    if (cArg.length !== 0) {
        ret.push(cArg);
    }

    return ret;
}

export class COBOLToken {
    public ignoreInOutlineView: boolean;
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
    public inSection: COBOLToken;

    static Null: COBOLToken = new COBOLToken("", COBOLTokenStyle.Null, -1, "", "", "", undefined, false, "");

    public getEndDelimiterToken(): COBOLToken {
        return new COBOLToken(this.filename, COBOLTokenStyle.EndDelimiter, this.startLine, "", this.tokenName, this.description, this.parentToken, this.inProcedureDivision, "");
    }

    public constructor(filename: string, tokenType: COBOLTokenStyle, startLine: number, line: string, token: string, description: string,
        parentToken: COBOLToken | undefined, inProcedureDivision: boolean, extraInformation: string) {
        this.ignoreInOutlineView = false;
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
        this.inSection = COBOLToken.Null;

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
    public line = "";

    private lineTokens: string[] = [];
    private lineLowerTokens: string[] = [];
    private tokenIndex = 0;

    public currentToken = "";
    public prevToken = "";
    public nextToken = "";
    public nextPlusOneToken = "";  // only used for method-id. get property xxx

    public currentTokenLower = "";
    public prevTokenLower = "";
    public nextTokenLower = "";

    public currentCol = 0;
    public prevCol = 0;
    public rollingColumn = 0;

    public endsWithDot = false;

    public constructor(line: string, previousToken?: Token) {
        this.line = line;
        this.setupLine();

        if (previousToken !== undefined) {
            // wire in previous token into this token
            if (previousToken.lineTokens.length > 0) {
                const lastToken = previousToken.lineTokens[previousToken.lineTokens.length - 1];
                this.prevCol = previousToken.line.indexOf(lastToken);
                this.prevToken = lastToken;
                this.prevTokenLower = lastToken.toLowerCase();
            }
        }
    }

    public static Blank = new Token("", undefined);

    private setupLine() {
        this.lineTokens = splitArgument(this.line, false);
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
        const possibleTokenLower = possibleToken.toLocaleLowerCase();
        const possibleTokenLowerDot = possibleTokenLower + ".";

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
    public copyBooksUsed: Map<string, COBOLToken>;
    public state: ParseState;
    public tokensInOrder: COBOLToken[];

    public ignoreUnusedSymbol: Map<string, string>;

    public topLevel: boolean;

    constructor(topLevel: boolean) {
        this.filenames = [];
        this.targetReferences = new Map<string, SourceReference[]>();
        this.constantsOrVariablesReferences = new Map<string, SourceReference[]>();
        this.sharedConstantsOrVariables = new Map<string, COBOLToken[]>();
        this.sharedSections = new Map<string, COBOLToken>();
        this.sharedParagraphs = new Map<string, COBOLToken>();
        this.copyBooksUsed = new Map<string, COBOLToken>();
        this.state = new ParseState();
        this.tokensInOrder = [];
        this.topLevel = topLevel;
        this.ignoreUnusedSymbol = new Map<string, string>();
    }
}

export enum UsingState {
    BY_VALUE,
    BY_REF,
    BY_CONTENT,
    RETURNING
}

export class COBOLParameter {
    using: UsingState;
    name: string;

    constructor(u: UsingState, n: string) {
        this.using = u;
        this.name = n;
    }
}

class ParseState {
    currentToken: COBOLToken;
    currentRegion: COBOLToken;
    currentDivision: COBOLToken;
    currentSection: COBOLToken;
    currentParagraph: COBOLToken;
    currentClass: COBOLToken;
    currentMethod: COBOLToken;
    currentFunctionId: COBOLToken;
    current01Group: COBOLToken;
    currentLevel: COBOLToken;
    currentProgramTarget: CallTargetInformation;

    copyBooksUsed: Map<string, COBOLToken>;

    procedureDivision: COBOLToken;
    declaratives: COBOLToken;
    captureDivisions: boolean;
    programs: COBOLToken[];
    pickFields: boolean;
    pickUpUsing: boolean;
    skipToDot: boolean;

    inProcedureDivision: boolean;
    inDeclaratives: boolean;

    ignoreInOutlineView: boolean;

    addReferencesDuringSkipToTag: boolean;

    using: UsingState;

    parameters: COBOLParameter[];

    entryPointCount: number;

    constructor() {
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
        this.currentProgramTarget = new CallTargetInformation(COBOLToken.Null, false, []);
        this.programs = [];
        this.captureDivisions = true;
        this.copyBooksUsed = new Map<string, COBOLToken>();
        this.pickFields = false;
        this.inProcedureDivision = false;
        this.inDeclaratives = false;
        this.ignoreInOutlineView = false;
        this.skipToDot = false;
        this.addReferencesDuringSkipToTag = false;
        this.pickUpUsing = false;
        this.using = UsingState.BY_REF;
        this.parameters = [];
        this.entryPointCount = 0;
    }
}

class PreParseState {
    numberTokensInHeader: number;
    workingStorageRelatedTokens: number;
    procedureDivisionRelatedTokens: number;
    sectionsInToken: number;
    divisionsInToken: number;
    leaveEarly: boolean;

    constructor() {
        this.numberTokensInHeader = 0;
        this.workingStorageRelatedTokens = 0;
        this.procedureDivisionRelatedTokens = 0;
        this.sectionsInToken = 0;
        this.divisionsInToken = 0;
        this.leaveEarly = false;
    }
}

export class CallTargetInformation {

    public Token: COBOLToken;
    public IsEntryPoint: boolean;
    public CallParameters: COBOLParameter[];

    constructor(token: COBOLToken, isEntryPoint: boolean, params: COBOLParameter[]) {
        this.Token = token;
        this.IsEntryPoint = isEntryPoint;
        this.CallParameters = params;
    }
}

export interface ICOBOLSourceScanner {
    filename: string;
    lastModifiedTime: number;
    copyBooksUsed: Map<string, COBOLToken>;
    tokensInOrder: COBOLToken[];
}

export default class COBOLSourceScanner implements ICommentCallback, ICOBOLSourceScanner {
    public filename: string;
    public lastModifiedTime = 0;

    public tokensInOrder: COBOLToken[] = [];

    public sections: Map<string, COBOLToken>;
    public paragraphs: Map<string, COBOLToken>;
    public constantsOrVariables: Map<string, COBOLToken[]>;
    public callTargets: Map<string, CallTargetInformation>;
    public functionTargets: Map<string, CallTargetInformation>;
    public classes: Map<string, COBOLToken>;
    public methods: Map<string, COBOLToken>;
    public isCached: boolean;
    public copyBooksUsed: Map<string, COBOLToken>;
    public diagWarnings: Map<string, COBOLFileSymbol>;

    public parseReferences: boolean;
    public sourceReferences: SharedSourceReferences;
    public sourceFileId: number;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public cpPerformTargets: any | undefined = undefined;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public cpConstantsOrVars: any | undefined = undefined;

    public ImplicitProgramId = "";

    public sourceFormat: ESourceFormat = ESourceFormat.unknown;

    public sourceIsCopybook = false;

    public commentDocStyle: CobolDocStyle = CobolDocStyle.unknown;
    public commentTagStyle: CobolTagStyle = CobolTagStyle.unknown;

    public readonly parse_copybooks_for_references: boolean;

    readonly copybookNestedInSection: boolean;

    readonly configHandler: ICOBOLSettings;

    parseHint_OnOpenFiles: string[] = [];
    parseHint_WorkingStorageFiles: string[] = [];
    parseHint_LocalStorageFiles: string[] = [];
    parseHint_ScreenSectionFiles: string[] = [];


    public constructor(sourceHandler: ISourceHandler, configHandler: ICOBOLSettings,
        cacheDirectory: string, sourceReferences: SharedSourceReferences = new SharedSourceReferences(true),
        parse_copybooks_for_references: boolean = configHandler.parse_copybooks_for_references) {
        const filename = sourceHandler.getFilename();

        this.configHandler = configHandler;
        this.filename = path.normalize(filename);
        this.ImplicitProgramId = path.basename(filename, path.extname(filename));

        this.parse_copybooks_for_references = parse_copybooks_for_references;

        this.copybookNestedInSection = configHandler.copybooks_nested;
        this.copyBooksUsed = new Map<string, COBOLToken>();
        this.isCached = false;
        this.sections = new Map<string, COBOLToken>();
        this.paragraphs = new Map<string, COBOLToken>();
        this.constantsOrVariables = new Map<string, COBOLToken[]>();
        this.callTargets = new Map<string, CallTargetInformation>();
        this.functionTargets = new Map<string, CallTargetInformation>();
        this.classes = new Map<string, COBOLToken>();
        this.methods = new Map<string, COBOLToken>();
        this.diagWarnings = new Map<string, COBOLFileSymbol>();
        this.parseReferences = sourceHandler !== null;
        this.cpPerformTargets = undefined;
        this.cpConstantsOrVars = undefined;

        let sourceLooksLikeCOBOL = false;
        let prevToken: Token = Token.Blank;

        const hasCOBOLExtension = path.extname(filename).length > 0 ? true : false;
        this.sourceReferences = sourceReferences;

        this.sourceFileId = 0;
        this.sourceFileId = sourceReferences.filenames.length;
        sourceReferences.filenames.push(sourceHandler.getUri());

        this.constantsOrVariables = sourceReferences.sharedConstantsOrVariables;
        this.paragraphs = sourceReferences.sharedParagraphs;
        this.sections = sourceReferences.sharedSections;
        this.tokensInOrder = sourceReferences.tokensInOrder;
        this.copyBooksUsed = sourceReferences.copyBooksUsed;

        // set the source handler for the commentspackage
        sourceHandler.setCommentCallback(this);

        const state: ParseState = this.sourceReferences.state;

        /* mark this has been processed (to help copy of self) */
        state.copyBooksUsed.set(this.filename, COBOLToken.Null);

        if (this.sourceReferences.topLevel) {
            const stat: fs.Stats = fs.statSync(this.filename);
            this.lastModifiedTime = stat.mtimeMs;

            /* if we have an extension, then don't do a relaxed parse to determiune if it is COBOL or not */
            const lineLimit = configHandler.pre_parse_line_limit;
            const maxLinesInFile = sourceHandler.getLineCount();
            let maxLines = maxLinesInFile;
            if (maxLines > lineLimit) {
                maxLines = lineLimit;
            }

            let line: string | undefined = undefined;
            const preParseState: PreParseState = new PreParseState();

            for (let l = 0; l < maxLines + sourceHandler.getCommentCount(); l++) {
                if (l > maxLinesInFile) {
                    break;
                }

                try {
                    line = sourceHandler.getLine(l);
                    if (line === undefined) {
                        break; // eof
                    }

                    line = line.trimRight();

                    // don't parse a empty line
                    if (line.length > 0) {
                        if (prevToken.endsWithDot === false) {
                            prevToken = this.relaxedParseLineByLine(prevToken, line, preParseState);
                        }
                        else {
                            prevToken = this.relaxedParseLineByLine(Token.Blank, line, preParseState);
                        }
                    } else {
                        maxLines++;     // increase the max lines, as this line is
                    }

                    if (preParseState.leaveEarly) {
                        break;
                    }

                }
                catch (e) {
                    logException("CobolQuickParse - Parse error : " + e, e);
                }
            }

            // Do we have some sections?
            if (preParseState.sectionsInToken === 0 && preParseState.divisionsInToken === 0) {
                /* if we have items that could be in a data division */

                if (preParseState.procedureDivisionRelatedTokens !== 0 && preParseState.procedureDivisionRelatedTokens > preParseState.workingStorageRelatedTokens) {
                    this.ImplicitProgramId = "";

                    const fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Procedure Division", "Procedure", "Procedure Division (CopyBook)", state.currentDivision);
                    state.currentDivision = fakeDivision;
                    state.procedureDivision = fakeDivision;
                    state.pickFields = false;
                    state.inProcedureDivision = true;
                    sourceLooksLikeCOBOL = true;
                    fakeDivision.ignoreInOutlineView = true;
                    this.sourceIsCopybook = true;
                }
                else if ((preParseState.workingStorageRelatedTokens !== 0 && preParseState.numberTokensInHeader !== 0)) {
                    const fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Data Division", "Data", "Data Division (CopyBook)", state.currentDivision);
                    state.currentDivision = fakeDivision;
                    state.pickFields = true;
                    state.inProcedureDivision = false;
                    sourceLooksLikeCOBOL = true;
                    this.ImplicitProgramId = "";
                    fakeDivision.ignoreInOutlineView = true;
                    this.sourceIsCopybook = true;
                }
            }

            /* if the source has an extension, then continue on reguardless */
            if (hasCOBOLExtension) {
                sourceLooksLikeCOBOL = true;
                /* otherwise, does it look like COBOL? */
            } else if (preParseState.sectionsInToken !== 0 || preParseState.divisionsInToken !== 0 || sourceHandler.getCommentCount() > 0) {
                sourceLooksLikeCOBOL = true;
            }

            /* leave early */
            if (sourceLooksLikeCOBOL === false) {
                logMessage(` Unabled to determine if ${filename} is COBOL after scanning ${maxLines} lines`);
                return;
            }

        } else {
            sourceLooksLikeCOBOL = true;
        }
        sourceHandler.resetCommentCount();

        this.sourceFormat = getCOBOLSourceFormat(sourceHandler, configHandler);
        switch (this.sourceFormat) {
            case ESourceFormat.free: sourceHandler.setDumpAreaBOnwards(false);
                break;
            case ESourceFormat.variable: sourceHandler.setDumpAreaBOnwards(false);
                break;
            case ESourceFormat.fixed: sourceHandler.setDumpAreaBOnwards(true);
                break;
        }

        let line: string | undefined = undefined;
        prevToken = Token.Blank;
        sourceHandler.resetCommentCount();

        for (let l = 0; l < sourceHandler.getLineCount(); l++) {
            try {
                line = sourceHandler.getLine(l);

                if (line === undefined) {
                    break;
                }

                line = line.trimRight();

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

        if (this.sourceReferences.topLevel) {
            const lineLength = line !== undefined ? line.length : 0;
            if (state.programs.length !== 0) {
                for (let cp = 0; cp < state.programs.length; cp++) {
                    const currentProgram = state.programs.pop();
                    if (currentProgram !== undefined) {
                        currentProgram.endLine = sourceHandler.getLineCount();
                        currentProgram.endColumn = lineLength;
                    }
                }
            }

            if (state.currentDivision !== COBOLToken.Null) {
                state.currentDivision.endLine = sourceHandler.getLineCount();
                state.currentDivision.endColumn = lineLength;
            }

            if (state.currentSection !== COBOLToken.Null) {
                state.currentSection.endLine = sourceHandler.getLineCount();
                state.currentSection.endColumn = lineLength;
            }

            if (state.currentParagraph !== COBOLToken.Null) {
                state.currentParagraph.endLine = sourceHandler.getLineCount();
                state.currentParagraph.endColumn = lineLength;
            }

            this.addinMissingEndlings();

            if (this.ImplicitProgramId.length !== 0) {
                const ctoken = this.newCOBOLToken(COBOLTokenStyle.ImplicitProgramId, 0, "", this.ImplicitProgramId, this.ImplicitProgramId, undefined);
                state.currentProgramTarget.Token = ctoken;
                this.callTargets.set(this.ImplicitProgramId, state.currentProgramTarget);
            }

            if (cacheDirectory !== null && cacheDirectory.length > 0) {
                if (this.configHandler.parse_copybooks_for_references && !this.sourceReferences.topLevel) {
                    logMessage(` Skipping ${filename} as it is not a top level reference`);
                } else {
                    if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, filename)) {
                        const qcp_symtable: COBOLSymbolTable = COBOLSymbolTableHelper.getCOBOLSymbolTable(this);
                        COBOLSymbolTableHelper.saveToFile(cacheDirectory, qcp_symtable);
                    }
                }
            }
        }
    }


    private newCOBOLToken(tokenType: COBOLTokenStyle, startLine: number, line: string, token: string,
        description: string, parentToken: COBOLToken | undefined,
        extraInformation = ""): COBOLToken {

        const state: ParseState = this.sourceReferences.state;
        const ctoken = new COBOLToken(this.filename, tokenType, startLine, line, token, description, parentToken, state.inProcedureDivision, extraInformation);
        ctoken.ignoreInOutlineView = state.ignoreInOutlineView;
        ctoken.inSection = this.sourceReferences.state.currentSection;

        if (ctoken.ignoreInOutlineView) {
            this.tokensInOrder.push(ctoken);
            return ctoken;
        }

        /* if we are in a paragraph update */
        if (state.currentParagraph !== COBOLToken.Null) {
            state.currentParagraph.endLine = startLine;
            if (ctoken.startColumn !== 0) {
                state.currentParagraph.endColumn = ctoken.startColumn - 1;
            }
        }

        // new divison
        if (tokenType === COBOLTokenStyle.Division && state.currentDivision !== COBOLToken.Null) {
            state.currentParagraph = COBOLToken.Null;

            state.currentDivision.endLine = startLine;
            if (ctoken.startColumn !== 0) {
                state.currentDivision.endColumn = ctoken.startColumn - 1;
            }

            if (state.currentSection !== COBOLToken.Null) {
                state.currentSection.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    state.currentSection.endColumn = ctoken.startColumn - 1;
                }
                state.currentSection = COBOLToken.Null;
            }
            this.tokensInOrder.push(ctoken);
            return ctoken;
        }

        // new section
        if (tokenType === COBOLTokenStyle.Section && state.currentSection !== COBOLToken.Null) {
            state.currentParagraph = COBOLToken.Null;
            state.currentSection.endLine = startLine;
            if (ctoken.startColumn !== 0) {
                state.currentSection.endColumn = ctoken.startColumn - 1;
            }
            this.tokensInOrder.push(ctoken);
            return ctoken;
        }

        // new paragraph
        if (tokenType === COBOLTokenStyle.Paragraph) {
            if (state.currentSection !== COBOLToken.Null) {
                state.currentSection.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    state.currentSection.endColumn = ctoken.startColumn - 1;
                }
            }

            state.currentParagraph = ctoken;

            if (state.currentDivision !== COBOLToken.Null) {
                state.currentDivision.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    state.currentDivision.endColumn = ctoken.startColumn - 1;
                }
            }
            this.tokensInOrder.push(ctoken);
            return ctoken;
        }

        this.tokensInOrder.push(ctoken);
        return ctoken;
    }

    public static clearMetaData(settings: ICOBOLSettings, cacheDirectory: string): void {
        window.showQuickPick(["Yes", "No"], { placeHolder: "Are you sure you want to clear the metadata?" }).then(function (data) {
            if (data === 'Yes') {
                clearCOBOLCache();
                InMemoryGlobalSymbolCache.callableSymbols.clear();
                InMemoryGlobalSymbolCache.classSymbols.clear();
                InMemoryGlobalSymbolCache.isDirty = false;
                for (const file of fs.readdirSync(cacheDirectory)) {
                    if (file.endsWith(".sym")) {
                        const fileName = path.join(cacheDirectory, file);
                        fs.unlinkSync(fileName);
                    }
                }
                logMessage("Metadata cache cleared");
            }
        });
    }

    private static readonly literalRegex = /^[#a-zA-Z0-9][a-zA-Z0-9-_]*$/g;

    private isValidLiteral(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(COBOLSourceScanner.literalRegex)) {
            return true;
        }

        return false;
    }

    private static readonly paragraphRegex = /^[a-zA-Z0-9][a-zA-Z0-9-_]*$/g;

    private isParagraph(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(COBOLSourceScanner.paragraphRegex) === null) {
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
        if (literalTrimmed[0] === "'" && literalTrimmed.endsWith("'")) {
            return literalTrimmed.substr(1, literalTrimmed.length - 2);
        }

        /* remove end . */
        if (literalTrimmed.endsWith(".")) {
            return literalTrimmed.substr(0, literalTrimmed.length - 1);
        }

        return literalTrimmed;
    }

    private relaxedParseLineByLine(prevToken: Token, line: string, state: PreParseState): Token {
        const token = new Token(line, prevToken);
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
                    const tokenAsNumber = Number.parseInt(tcurrent);
                    if (tokenAsNumber !== undefined && (!isNaN(tokenAsNumber))) {
                        state.numberTokensInHeader++;
                        continue;
                    }
                }
                switch (tcurrentLower) {
                    case "section":
                        if (token.prevToken.length !== 0) {
                            switch (token.prevTokenLower) {
                                case "working-storage": state.sectionsInToken++;
                                    state.leaveEarly = true;
                                    break;
                                case "file": state.sectionsInToken++;
                                    state.leaveEarly = true;
                                    break;
                                case "linkage": state.sectionsInToken++;
                                    state.leaveEarly = true;
                                    break;
                                case "screen": state.sectionsInToken++;
                                    state.leaveEarly = true;
                                    break;
                            }
                        }
                        break;
                    case "program-id":
                        state.divisionsInToken++;       // not really a division but that's okay because it looks like a program
                        state.leaveEarly = true;
                        break;
                    case "division":
                        switch (token.prevTokenLower) {
                            case "identification":
                                state.divisionsInToken++;
                                state.leaveEarly = true;
                                break;
                            case "procedure": state.divisionsInToken++;
                                state.leaveEarly = true;
                                break;
                        }
                        break;
                    default:
                        if (this.isValidProcedureKeyword(tcurrentLower)) {
                            state.procedureDivisionRelatedTokens++;
                        }

                        if (this.isValidStorageKeyword(tcurrentLower)) {
                            state.workingStorageRelatedTokens++;
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
            const sourceRefs: SourceReference[] | undefined = referencesMap.get(lowerCaseVariable);
            if (sourceRefs !== undefined) {
                sourceRefs.push(new SourceReference(this.sourceFileId, line, column));
                return;
            }
        }

        const sourceRefs: SourceReference[] = [];
        sourceRefs.push(new SourceReference(this.sourceFileId, line, column));
        referencesMap.set(lowerCaseVariable, sourceRefs);
    }

    private addVariableOrConstant(lowerCaseVariable: string, token: COBOLToken) {
        if (this.constantsOrVariables.has(lowerCaseVariable)) {
            const tokens: COBOLToken[] | undefined = this.constantsOrVariables.get(lowerCaseVariable);
            if (tokens !== undefined) {
                tokens.push(token);
                return;
            }
        }

        const tokens: COBOLToken[] = [];
        tokens.push(token);
        this.constantsOrVariables.set(lowerCaseVariable, tokens);
    }

    private parseLineByLine(sourceHandler: ISourceHandler, lineNumber: number, prevToken: Token, line: string): Token {
        const token = new Token(line, prevToken);

        const state: ParseState = this.sourceReferences.state;

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

                // if pickUpUsing
                if (state.pickUpUsing) {
                    if (endWithDot) {
                        state.pickUpUsing = false;
                    }

                    switch (tcurrentLower) {
                        case "using":
                            state.using = UsingState.BY_REF;
                            break;
                        case "by":
                            break;
                        case "reference":
                            state.using = UsingState.BY_REF;
                            break;
                        case "value":
                            state.using = UsingState.BY_VALUE;
                            break;
                        case "returning":
                            state.using = UsingState.RETURNING;
                            break;
                        default:
                            if (this.sourceReferences !== undefined) {
                                // no forward validation can be done, as this is a one pass scanner
                                this.addReference(this.sourceReferences.targetReferences, tcurrentLower, lineNumber, token.currentCol);
                            }
                            state.parameters.push(new COBOLParameter(state.using, tcurrent));
                        // logMessage(`INFO: using parameter : ${tcurrent}`);
                    }
                    if (endWithDot) {
                        state.currentProgramTarget.CallParameters = state.parameters;
                    }


                    continue;
                }

                // if skiptodot and not the end of the statement.. swallow
                if (state.skipToDot && endWithDot === false) {
                    if (state.addReferencesDuringSkipToTag) {
                        const trimToken = this.trimLiteral(tcurrentLower);
                        if ((this.isValidKeyword(tcurrentLower) === false) && (this.isValidLiteral(tcurrentLower))) {
                            if (this.sourceReferences !== undefined) {
                                // no forward validation can be done, as this is a one pass scanner
                                this.addReference(this.sourceReferences.targetReferences, trimToken, lineNumber, token.currentCol);
                            }
                        }
                        if (token.prevTokenLower === 'to' && this.isValidKeyword(tcurrentLower) === false) {
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, trimToken, trimToken, state.currentDivision, token.prevToken);
                            this.addVariableOrConstant(trimToken, variableToken);
                        }
                    }
                    continue;
                }

                // reset
                if (state.skipToDot && endWithDot === true) {
                    state.skipToDot = false;
                    state.addReferencesDuringSkipToTag = false;
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
                    state.currentToken = this.newCOBOLToken(COBOLTokenStyle.Exec, lineNumber, line, prevToken, "", state.currentDivision);
                    continue;
                }

                /* finish processing end-exec */
                if (currentLower === "end-exec") {
                    state.currentToken = COBOLToken.Null;
                    endWithDot = true;
                    token.endsWithDot = endWithDot;
                    continue;
                }

                /* skip everything in between exec .. end-exec */
                if (state.currentToken.tokenType === COBOLTokenStyle.Exec) {
                    continue;
                }

                if (prevTokenLower === "end" && currentLower === "declaratives") {
                    state.declaratives.endLine = lineNumber;
                    state.declaratives.endColumn = line.indexOf(tcurrent);
                    this.tokensInOrder.push(state.declaratives);
                    state.inDeclaratives = false;
                    state.declaratives = COBOLToken.Null;
                    continue;
                }

                // only include sections in the declaratives area
                if (state.inDeclaratives) {
                    if (currentLower === 'section') {
                        this.newCOBOLToken(COBOLTokenStyle.DeclarativesSection, lineNumber, line, prevToken, prevToken, state.currentDivision);
                    }
                    continue;
                }
                // handle sections)
                if (state.currentClass === COBOLToken.Null && prevToken.length !== 0 && currentLower === "section" && (prevTokenLower !== 'exit')) {
                    if (prevTokenLower === "declare") {
                        continue;
                    }

                    // So we need to insert a fake data division?
                    if (state.currentDivision === COBOLToken.Null) {
                        if (prevTokenLower === 'file' ||
                            prevTokenLower === 'working-storage' ||
                            prevTokenLower === 'local-storage' ||
                            prevTokenLower === 'screen' ||
                            prevTokenLower === 'linkage') {

                            if (this.ImplicitProgramId.length !== 0) {
                                const trimmedCurrent = this.trimLiteral(this.ImplicitProgramId);
                                const ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, "program-id. " + this.ImplicitProgramId, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                                state.programs.push(ctoken);
                                ctoken.ignoreInOutlineView = true;
                                this.ImplicitProgramId = "";        /* don't need it */
                            }

                            state.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Data Division", "Data", "Data Division (Optional)", state.currentDivision);
                            state.currentDivision.ignoreInOutlineView = true;
                        }
                    }

                    state.currentSection = this.newCOBOLToken(COBOLTokenStyle.Section, lineNumber, line, prevToken, prevPlusCurrent, state.currentDivision);
                    this.sections.set(prevTokenLower, state.currentSection);
                    state.current01Group = COBOLToken.Null;
                    state.currentLevel = COBOLToken.Null;

                    if (prevTokenLower === "working-storage" || prevTokenLower === "linkage" ||
                        prevTokenLower === "local-storage" || prevTokenLower === "file-control" ||
                        prevTokenLower === 'file' || prevTokenLower === "screen") {
                        state.pickFields = true;
                        state.inProcedureDivision = false;
                        // sourceHandler.setDumpAreaA(false);
                        // sourceHandler.setDumpAreaBOnwards(false);
                    }

                    continue;
                }

                // handle divisions
                if (state.captureDivisions && prevTokenLower.length !== 0 && currentLower === "division") {
                    state.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, line, prevToken, prevPlusCurrent, COBOLToken.Null);

                    if (prevTokenLower === "procedure") {
                        state.inProcedureDivision = true;
                        state.pickFields = false;
                        state.procedureDivision = state.currentDivision;
                        if (endWithDot === false) {
                            state.pickUpUsing = true;
                        }
                        sourceHandler.setDumpAreaA(true);
                        sourceHandler.setDumpAreaBOnwards(false);
                    }

                    continue;
                }

                // handle entries
                if (prevTokenLower === "entry" && current.length !== 0) {
                    const trimmedCurrent = this.trimLiteral(current);
                    const ctoken = this.newCOBOLToken(COBOLTokenStyle.EntryPoint, lineNumber, line, trimmedCurrent, prevPlusCurrent, state.currentDivision);

                    state.entryPointCount++;
                    state.parameters = [];
                    state.currentProgramTarget = new CallTargetInformation(ctoken, true, []);
                    this.callTargets.set(trimmedCurrent, state.currentProgramTarget);
                    state.pickUpUsing = true;
                    continue;
                }

                // handle program-id
                if (prevTokenLower === "program-id" && current.length !== 0) {
                    const trimmedCurrent = this.trimLiteral(current);
                    const ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, line, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                    state.programs.push(ctoken);
                    if (trimmedCurrent.indexOf(" ") === -1 && token.isTokenPresent("external") === false) {
                        state.parameters = [];
                        state.currentProgramTarget = new CallTargetInformation(ctoken, false, []);
                        this.callTargets.set(trimmedCurrent, state.currentProgramTarget);
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
                    const trimmedCurrent = this.trimLiteral(current);
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.ClassId, lineNumber, line, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                    state.captureDivisions = false;
                    state.currentMethod = COBOLToken.Null;
                    state.pickFields = true;
                    this.classes.set(trimmedCurrent, state.currentClass);

                    continue;
                }

                // handle "end class, enum, valuetype"
                if (state.currentClass !== COBOLToken.Null && prevTokenLower === "end" &&
                    (currentLower === "class" || currentLower === "enum" || currentLower === "valuetype")) {
                    state.currentClass.endLine = lineNumber;
                    state.currentClass.endColumn = line.toLocaleLowerCase().indexOf(currentLower) + currentLower.length;

                    state.currentClass = COBOLToken.Null;
                    state.captureDivisions = true;
                    state.currentMethod = COBOLToken.Null;
                    state.pickFields = false;
                    state.inProcedureDivision = false;
                    continue;
                }

                // handle enum-id
                if (prevTokenLower === "enum-id" && current.length !== 0) {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.EnumId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, COBOLToken.Null);

                    state.captureDivisions = false;
                    state.currentMethod = COBOLToken.Null;
                    state.pickFields = true;
                    continue;
                }

                // handle interface-id
                if (prevTokenLower === "interface-id" && current.length !== 0) {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.InterfaceId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, state.currentDivision);

                    state.pickFields = true;
                    state.captureDivisions = false;
                    state.currentMethod = COBOLToken.Null;
                    continue;
                }

                // handle valuetype-id
                if (prevTokenLower === "valuetype-id" && current.length !== 0) {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.ValueTypeId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, state.currentDivision);

                    state.pickFields = true;
                    state.captureDivisions = false;
                    state.currentMethod = COBOLToken.Null;
                    continue;
                }

                // handle function-id
                if (prevTokenLower === "function-id" && current.length !== 0) {
                    const trimmedCurrent = this.trimLiteral(current);
                    state.currentFunctionId = this.newCOBOLToken(COBOLTokenStyle.FunctionId, lineNumber, line, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                    state.captureDivisions = true;
                    state.pickFields = true;
                    state.parameters = [];
                    state.currentProgramTarget = new CallTargetInformation(state.currentFunctionId, false, []);
                    this.functionTargets.set(trimmedCurrent, state.currentProgramTarget);

                    continue;
                }

                // handle method-id
                if (prevTokenLower === "method-id" && current.length !== 0) {
                    const currentLowerTrim = this.trimLiteral(currentLower);
                    const style = currentLowerTrim === "new" ? COBOLTokenStyle.Constructor : COBOLTokenStyle.MethodId;

                    if (nextTokenLower === "property") {
                        const trimmedProperty = this.trimLiteral(nextPlusOneToken);
                        state.currentMethod = this.newCOBOLToken(COBOLTokenStyle.Property, lineNumber, line, trimmedProperty, nextToken + " " + nextPlusOneToken, state.currentDivision);
                        this.methods.set(trimmedProperty, state.currentMethod);
                    } else {
                        const trimmedCurrent = this.trimLiteral(current);
                        state.currentMethod = this.newCOBOLToken(style, lineNumber, line, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                        this.methods.set(trimmedCurrent, state.currentMethod);
                    }

                    state.pickFields = true;
                    state.captureDivisions = false;
                    continue;
                }

                // handle "end method"
                if (state.currentMethod !== COBOLToken.Null && prevTokenLower === "end" && currentLower === "method") {
                    state.currentMethod.endLine = lineNumber;
                    state.currentMethod.endColumn = line.toLocaleLowerCase().indexOf(currentLower) + currentLower.length;

                    state.currentMethod = COBOLToken.Null;
                    state.pickFields = false;
                    continue;
                }

                // handle "end program"
                if (state.programs.length !== 0 && prevTokenLower === "end" && currentLower === "program") {
                    const currentProgram: COBOLToken | undefined = state.programs.pop();
                    if (currentProgram !== undefined) {
                        currentProgram.endLine = lineNumber;
                        currentProgram.endColumn = line.length;
                    }

                    if (state.currentDivision !== COBOLToken.Null) {
                        state.currentDivision.endLine = lineNumber;
                        state.currentDivision.endColumn = line.length;
                    }

                    if (state.currentSection !== COBOLToken.Null) {
                        state.currentSection.endLine = lineNumber;
                        state.currentSection.endColumn = line.length;
                    }

                    if (state.currentParagraph !== COBOLToken.Null) {
                        state.currentParagraph.endLine = lineNumber;
                        state.currentParagraph.endColumn = line.length;
                    }

                    state.currentDivision = COBOLToken.Null;
                    state.currentSection = COBOLToken.Null;
                    state.currentParagraph = COBOLToken.Null;
                    state.inProcedureDivision = false;
                    state.pickFields = false;
                    continue;
                }

                // handle "end function"
                if (state.currentFunctionId !== COBOLToken.Null && prevTokenLower === "end" && currentLower === "function") {
                    state.currentFunctionId.endLine = lineNumber;
                    state.currentFunctionId.endColumn = line.toLocaleLowerCase().indexOf(currentLower) + currentLower.length;
                    this.newCOBOLToken(COBOLTokenStyle.EndFunctionId, lineNumber, line, prevToken, current, state.currentDivision);

                    state.pickFields = false;
                    state.inProcedureDivision = false;

                    if (state.currentDivision !== COBOLToken.Null) {
                        state.currentDivision.endLine = lineNumber;
                        state.currentDivision.endColumn = 0;
                    }

                    if (state.currentSection !== COBOLToken.Null) {
                        state.currentSection.endLine = lineNumber;
                        state.currentSection.endColumn = 0;
                    }
                    state.currentDivision = COBOLToken.Null;
                    state.currentSection = COBOLToken.Null;
                    state.currentParagraph = COBOLToken.Null;
                    state.procedureDivision = COBOLToken.Null;
                    continue;
                }


                if (currentLower === "declaratives") {
                    state.declaratives = this.newCOBOLToken(COBOLTokenStyle.Declaratives, lineNumber, line, prevToken, current, state.currentDivision);
                    state.inDeclaratives = true;
                    this.tokensInOrder.pop();       /* only interested it at the end */
                    continue;
                }

                // copybook handling
                if (prevTokenLower === "copy" && current.length !== 0) {
                    const trimmedCopyBook = this.trimLiteral(current);
                    // let newCopybook: boolean = false;

                    let copyToken: COBOLToken = COBOLToken.Null;
                    if (nextTokenLower === 'in' && nextPlusOneToken.length !== 0) {
                        const nextPlusOneTokenTrimmed = this.trimLiteral(nextPlusOneToken);
                        const desc: string = prevPlusCurrent + " in " + nextPlusOneTokenTrimmed;
                        if (this.copybookNestedInSection) {
                            if (state.currentSection !== COBOLToken.Null) {
                                copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBookIn, lineNumber, line, prevPlusCurrent, desc, state.currentSection, nextPlusOneTokenTrimmed);
                            } else {
                                copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBookIn, lineNumber, line, prevPlusCurrent, desc, state.currentDivision, nextPlusOneTokenTrimmed);
                            }
                        } else {
                            copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBookIn, lineNumber, line, prevPlusCurrent, desc, state.currentDivision, nextPlusOneTokenTrimmed);
                        }
                    }
                    else {
                        if (this.copybookNestedInSection) {
                            if (state.currentSection !== COBOLToken.Null) {
                                copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, prevPlusCurrent, prevPlusCurrent, state.currentSection);
                            } else {
                                copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, prevPlusCurrent, prevPlusCurrent, state.currentDivision);
                            }
                        } else {
                            copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, prevPlusCurrent, prevPlusCurrent, state.currentDivision);
                        }
                    }

                    if (this.copyBooksUsed.has(trimmedCopyBook) === false) {
                        this.copyBooksUsed.set(trimmedCopyBook, copyToken);

                        if (this.sourceReferences !== undefined && this.parse_copybooks_for_references) {
                            const fileName = expandLogicalCopyBookToFilenameOrEmpty(trimmedCopyBook, copyToken.extraInformation, this.configHandler);
                            if (fileName.length > 0) {
                                const qfile = new FileSourceHandler(fileName, false);
                                const currentTopLevel = this.sourceReferences.topLevel;
                                const currentIgnoreInOutlineView: boolean = state.ignoreInOutlineView;
                                state.ignoreInOutlineView = true;
                                this.sourceReferences.topLevel = false;
                                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                                const qps = new COBOLSourceScanner(qfile, this.configHandler, "", this.sourceReferences, this.parse_copybooks_for_references);
                                this.sourceReferences.topLevel = currentTopLevel;
                                state.ignoreInOutlineView = currentIgnoreInOutlineView;
                            }
                        }
                    }
                    continue;
                }

                // we are in the procedure division
                if (state.captureDivisions && state.currentDivision !== COBOLToken.Null &&
                    state.currentDivision === state.procedureDivision && endWithDot) {
                    if (!this.isValidKeyword(prevTokenLower) && !this.isValidKeyword(currentLower)) {
                        const beforeCurrent = line.substr(0, token.currentCol - 1).trim();
                        if (beforeCurrent.length === 0) {
                            const c = tcurrent;
                            if (c.length !== 0) {
                                if (this.isParagraph(c)) {
                                    if (state.currentSection !== COBOLToken.Null) {
                                        const newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, c, c, state.currentSection);
                                        this.paragraphs.set(newToken.tokenNameLower, newToken);
                                    } else {
                                        const newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, c, c, state.currentDivision);
                                        this.paragraphs.set(newToken.tokenNameLower, newToken);
                                    }
                                }
                            }
                        }
                    }
                }

                if (state.currentSection.tokenNameLower === 'input-output') {
                    if (prevTokenLower === 'fd' || prevTokenLower === 'select') {
                        state.pickFields = true;
                    }
                }

                // are we in the working-storage section?
                if (state.pickFields && prevToken.length > 0) {

                    /* only interesting in things that are after a number */
                    if (this.isNumber(prevToken) && !this.isNumber(current)) {
                        const isFiller: boolean = (currentLower === 'filler');
                        let pickUpThisField: boolean = isFiller;
                        let trimToken = this.trimLiteral(current);
                        //
                        // what other reasons do we need to pickup this line as a field?
                        if (!pickUpThisField) {
                            // not a complete inclusive list but should cover the normal cases
                            if (currentLower.startsWith("pic") || currentLower.startsWith("comp-") || currentLower.startsWith("binary-")) {
                                // fake up the line
                                line = prevToken + " filler ";
                                trimToken = "filler";
                                pickUpThisField = true;
                            } else {
                                // okay, current item looks like it could be a field
                                if (!this.isValidKeyword(currentLower)) {
                                    pickUpThisField = true;
                                }
                            }
                        }

                        if (pickUpThisField) {
                            if (this.isValidLiteral(currentLower)) {
                                let style = prevToken === "78" ? COBOLTokenStyle.Constant : COBOLTokenStyle.Variable;
                                let extraInfo = prevToken;
                                if (prevToken === '01' || prevToken === '1') {
                                    if (nextTokenLower === 'redefines') {
                                        extraInfo += "-GROUP";
                                    } else if (nextTokenLower.length === 0) {
                                        extraInfo += "-GROUP";
                                    } else if (state.currentSection.tokenNameLower === 'report') {
                                        extraInfo += "-GROUP";
                                    }

                                    if (token.isTokenPresent("constant")) {
                                        style = COBOLTokenStyle.Constant;
                                    }
                                    if (token.isTokenPresent("redefines")) {
                                        style = COBOLTokenStyle.Constant;
                                    }
                                }

                                const ctoken = this.newCOBOLToken(style, lineNumber, line, trimToken, trimToken, state.currentDivision, extraInfo);
                                if (!isFiller) {
                                    this.addVariableOrConstant(currentLower, ctoken);
                                }

                                // place the 88 under the 01 item
                                if (state.currentLevel !== COBOLToken.Null && prevToken === '88') {
                                    state.currentLevel.endLine = ctoken.startLine;
                                    state.currentLevel.endColumn = ctoken.startColumn + ctoken.tokenName.length;
                                }

                                if (prevToken !== '88') {
                                    state.currentLevel = ctoken;
                                }

                                if (prevToken === '01' || prevToken === '1' ||
                                    prevToken === '66' || prevToken === '77' || prevToken === '78') {
                                    if (nextTokenLower.length === 0 ||
                                        nextTokenLower === 'redefines' ||
                                        (state.currentSection.tokenNameLower === "report" && nextTokenLower === "type")) {
                                        state.current01Group = ctoken;
                                    } else {
                                        state.current01Group = COBOLToken.Null;
                                    }
                                }

                                if (state.current01Group !== COBOLToken.Null) {
                                    state.current01Group.endLine = ctoken.startLine;
                                    state.current01Group.endColumn = ctoken.startColumn + ctoken.tokenName.length;
                                }

                                /* if spans multiple lines, skip to dot */
                                if (this.sourceFormat !== ESourceFormat.fixed) {
                                    if (endWithDot === false) {
                                        state.skipToDot = true;
                                    }
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
                        const trimToken = this.trimLiteral(current);

                        if (this.isValidLiteral(currentLower)) {
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, trimToken, trimToken, state.currentDivision, prevTokenLower);
                            this.addVariableOrConstant(currentLower, variableToken);
                        }

                        if (prevTokenLower === "rd" || prevTokenLower === 'select') {
                            if (prevTokenLower === 'select') {
                                state.addReferencesDuringSkipToTag = true;
                            }
                            state.skipToDot = true;
                        }
                        continue;
                    }

                    if (prevTokenLower === "indexed" && currentLower === "by" && nextToken.length > 0) {
                        if (this.isValidKeyword(nextTokenLower) === false) {
                            const trimmedNextToken = this.trimLiteral(nextToken);
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, trimmedNextToken, trimmedNextToken, state.currentDivision);
                            this.addVariableOrConstant(trimmedNextToken.toLowerCase(), variableToken);
                        }
                    }
                }

                /* add reference when perform is used */
                if (this.parseReferences && this.sourceReferences !== undefined) {
                    if (state.inProcedureDivision) {
                        const trimmedCurrentLower = this.trimLiteral(currentLower);
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

    private addinMissingEndlings() {
        for (let i = 0; i < this.tokensInOrder.length; i++) {
            const token = this.tokensInOrder[i];

            if (token.endLine === 0) {
                token.endLine = token.startLine;
                token.endColumn = token.startColumn + token.tokenName.length;
            }
        }
    }

    private cobolLintLiteral = "cobol-lint";

    public processComment(commentLine: string, sourceFilename: string, sourceLineNumber: number): void {
        const startOfComment: number = commentLine.indexOf("*>");

        if (startOfComment === undefined || startOfComment === -1) {
            return;
        }

        if (this.commentTagStyle === CobolTagStyle.unknown) {
            // is it a coboldoc?
            if (commentLine.indexOf("*>*") !== -1) {
                this.commentTagStyle = CobolTagStyle.FREE;
            } else {
                if (commentLine.indexOf("*>") !== -1) {
                    this.commentTagStyle = CobolTagStyle.MICROFOCUS;
                }
            }
        }

        if (this.commentDocStyle === CobolDocStyle.unknown) {
            const possilexmltags: string[] = ["<summary>", "<param>", "<returns>"];
            for (const possibleTag of possilexmltags) {
                if (commentLine.indexOf(possibleTag) !== -1) {
                    this.commentDocStyle = CobolDocStyle.MSDN;
                }
            }

            const possiblecobdoc: string[] = ["@author", "@license"];
            for (const possibleTag of possiblecobdoc) {
                if (commentLine.indexOf(possibleTag) !== -1) {
                    this.commentDocStyle = CobolDocStyle.COBOLDOC;
                }
            }

            const possibleICOBOLs: string[] = ["((DOC))", "((END-DOC))"];
            for (const possibleICOBOL of possibleICOBOLs) {
                if (commentLine.indexOf(possibleICOBOL) !== -1) {
                    this.commentDocStyle = CobolDocStyle.ISCOBOL;
                }
            }

            const possibleFUJITSUs: string[] = ["@**", "H ", "D "];
            for (const possibleFUJITSU of possibleFUJITSUs) {
                const trimLine = commentLine.trimLeft();
                if (trimLine.startsWith(possibleFUJITSU)) {
                    this.commentDocStyle = CobolDocStyle.FUJITSU;
                }
            }

            // leave early, if comment style found
            if (this.commentDocStyle !== CobolDocStyle.unknown) {
                return;
            }
        }

        // const comment = commentLine.substring(2 + startOfComment).trim();
        const startOfCOBOLint: number = commentLine.indexOf(this.cobolLintLiteral);

        if (startOfCOBOLint !== -1) {
            const commentCommandArgs = commentLine.substring(this.cobolLintLiteral.length + startOfCOBOLint).trim();
            let args = commentCommandArgs.split(" ");
            const command = args[0];
            args = args.slice(1);
            const commandTrimmed = command !== undefined ? command.trim() : undefined;
            if (commandTrimmed !== undefined) {
                if (commandTrimmed === CobolLinterProvider.NotReferencedMarker_external) {
                    for (const offset in args) {
                        this.sourceReferences.ignoreUnusedSymbol.set(args[offset].toLocaleLowerCase(), args[offset]);
                    }
                }
            }
        }

        // only enable scanner hint when process_scanner_hints_embedded_in_comments is set
        if (this.configHandler.process_scanner_hints_embedded_in_comments) {
            const startOfTokenFor = this.configHandler.process_scanner_hint_token_for_source_dependancies;

            const startOfSourceDepIndex: number = commentLine.indexOf(startOfTokenFor);
            if (startOfSourceDepIndex !== -1) {
                const commentCommandArgs = commentLine.substring(startOfTokenFor.length + startOfSourceDepIndex).trim();
                const args = commentCommandArgs.split(" ");
                if (args.length !== 0) {
                    for (const offset in args) {
                        const filenameTrimmed = args[offset].trim();
                        const fileName = expandLogicalCopyBookToFilenameOrEmpty(filenameTrimmed, "", this.configHandler);
                        if (fileName.length > 0) {
                            if (this.copyBooksUsed.has(fileName) === false) {
                                this.copyBooksUsed.set(fileName, COBOLToken.Null);

                                const qfile = new FileSourceHandler(fileName, false);
                                const currentIgnoreInOutlineView: boolean = this.sourceReferences.state.ignoreInOutlineView;
                                this.sourceReferences.state.ignoreInOutlineView = true;
                                this.sourceReferences.topLevel = true;
                                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                                const qps = new COBOLSourceScanner(qfile, this.configHandler, "", this.sourceReferences, this.parse_copybooks_for_references);
                                this.sourceReferences.topLevel = true;
                                this.sourceReferences.state.ignoreInOutlineView = currentIgnoreInOutlineView;
                            }
                        } else {
                            const diagMessage = `${startOfTokenFor}: Unable to locate copybook ${filenameTrimmed}`;
                            this.diagWarnings.set(diagMessage, new COBOLFileSymbol(sourceFilename, sourceLineNumber));

                        }
                    }
                }
            }
        }
    }
}
