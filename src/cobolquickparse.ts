import ISourceHandler from "./isourcehandler";
import { cobolKeywordDictionary, cobolProcedureKeywordDictionary, cobolStorageKeywordDictionary } from "./keywords/cobolKeywords";

import { workspace, TextDocument, WorkspaceFolder } from 'vscode';
import { FileSourceHandler } from "./FileSourceHandler";
import { String, StringBuilder } from 'typescript-string-operations';

import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import * as crypto from 'crypto';

import { logCOBOLChannelLine, getCachingSetting, activateLogChannel, logCOBOLChannelLineException } from "./extension";
import { getExtensions, expandLogicalCopyBookToFilenameOrEmpty, isValidExtension } from "./opencopybook";
import { VSCodeSourceHandler } from "./VSCodeSourceHandler";
import { performance } from "perf_hooks";
import { Hash } from "crypto";
import { isCachingEnabled } from "./extension";

const util = require('util');

var lzjs = require('lzjs');


export enum COBOLTokenStyle {
    CopyBook = "Copybook",
    ProgramId = "Program-Id",
    FunctionId = "Function-Id",
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
        if (c === '"') {
            inQuoteSingle = !inQuoteSingle;
            cArg += c;
            if (inQuoteSingle === false) {
                ret.push(cArg);
                cArg = "";
            }
            continue;
        }

        if (c === "\"") {
            inQuote = !inQuote;
            cArg += c;
            if (inQuote === false) {
                ret.push(cArg);
                cArg = "";
            }
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

export function splitArgumentOld(input: string, sep: RegExp = /\s/g, keepQuotes: boolean = true): string[] {
    let separator = sep || /\s/g;
    var singleQuoteOpen = false;
    var doubleQuoteOpen = false;
    var tokenBuffer = [];
    var ret = [];

    var arr = input.split('');
    for (var i = 0; i < arr.length; ++i) {
        var element = arr[i];
        var matches = element.match(separator);
        if (element === "'" && !doubleQuoteOpen) {
            if (keepQuotes === true) {
                tokenBuffer.push(element);
            }
            singleQuoteOpen = !singleQuoteOpen;
            continue;
        } else if (element === '"' && !singleQuoteOpen) {
            if (keepQuotes === true) {
                tokenBuffer.push(element);
            }
            doubleQuoteOpen = !doubleQuoteOpen;
            continue;
        }

        if (!singleQuoteOpen && !doubleQuoteOpen && matches) {
            if (tokenBuffer.length > 0) {
                ret.push(tokenBuffer.join(''));
                tokenBuffer = [];
            } else if (!!sep) {
                ret.push(element);
            }
        } else {
            tokenBuffer.push(element);
        }
    }
    if (tokenBuffer.length > 0) {
        ret.push(tokenBuffer.join(''));
    } else if (!!sep) {
        ret.push('');
    }
    return ret;
}

export class COBOLToken {
    public tokenType: COBOLTokenStyle;
    public startLine: number;
    public startColumn: number;
    public tokenName: string;
    public tokenNameLower: string;
    public description: string;
    public level: number;
    public parentToken: COBOLToken | undefined;
    public endLine: number;
    public endColumn: number;

    public childTokens: COBOLToken[] = [];

    static Null: COBOLToken = new COBOLToken(COBOLTokenStyle.Null, -1, "", "", "", undefined);

    public getEndDelimiterToken(): COBOLToken {
        return new COBOLToken(COBOLTokenStyle.EndDelimiter, this.startLine, "", this.tokenName, this.description, this.parentToken);
    }

    public constructor(tokenType: COBOLTokenStyle, startLine: number, line: string, token: string, description: string, parentToken: COBOLToken | undefined) {
        this.tokenType = tokenType;
        this.startLine = startLine;
        this.tokenName = token.trim();
        this.tokenNameLower = this.tokenName.toLowerCase();
        this.startColumn = line.indexOf(this.tokenName);
        this.description = description;
        this.endLine = this.endColumn = 0;
        this.level = (parentToken === undefined) ? 1 : 1 + parentToken.level;
        this.parentToken = parentToken;

        if (this.tokenName.length !== 0) {
            /* ensure we don't have any odd start columns */
            if (this.startColumn < 0) {
                this.startColumn = 0;
            }
        }
    }
}

class Token {
    public lineNumber: number = 0;
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
        this.lineNumber = 1;

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
}



export default class QuickCOBOLParse {
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

    inProcedureDivision: boolean;
    pickFields: boolean;
    guessFields: boolean;

    currentToken: COBOLToken;
    currentRegion: COBOLToken;
    currentDivision: COBOLToken;
    currentSection: COBOLToken;
    procedureDivision: COBOLToken;
    parseColumnBOnwards: boolean = this.getColumBParsing();

    captureDivisions: boolean;
    currentClass: COBOLToken;
    currentMethod: COBOLToken;

    numberTokensInHeader: number;
    workingStorageRelatedTokens: number;
    procedureDivisionRelatedTokens: number;
    sectionsInToken: number;
    divisionsInToken: number;

    copybookNestedInSection: boolean;

    sourceLooksLikeCOBOL: boolean;

    public constructor(sourceHandler: ISourceHandler, filename: string) {
        let stat: fs.Stats = fs.statSync(filename);

        this.filename = path.normalize(filename);
        this.lastModifiedTime = stat.mtimeMs;
        this.inProcedureDivision = false;
        this.pickFields = false;
        this.guessFields = false;       // does not pickup the initial group item in a copybook, so it's not quite ready
        this.currentDivision = COBOLToken.Null;
        this.procedureDivision = COBOLToken.Null;
        this.currentSection = COBOLToken.Null;
        this.currentToken = COBOLToken.Null;
        this.currentClass = COBOLToken.Null;
        this.currentMethod = COBOLToken.Null;
        this.currentRegion = COBOLToken.Null;
        this.captureDivisions = true;
        this.numberTokensInHeader = 0;
        this.workingStorageRelatedTokens = 0;
        this.procedureDivisionRelatedTokens = 0;
        this.sectionsInToken = 0;
        this.divisionsInToken = 0;
        this.copybookNestedInSection = this.getCopybookNestedInSection();
        this.copyBooksUsed = new Map();
        this.isCached = false;
        this.sections = new Map<string, COBOLToken>();
        this.paragraphs = new Map<string, COBOLToken>();
        this.constantsOrVariables = new Map<string, COBOLToken[]>();
        this.callTargets = new Map<string, COBOLToken>();
        this.classes = new Map<string, COBOLToken>();
        this.methods = new Map<string, COBOLToken>();
        this.sourceLooksLikeCOBOL = false;
        let prevToken: Token = Token.Blank;

        let maxLines = sourceHandler.getLineCount();
        if (maxLines > 25) {
            maxLines = 25;
        }

        for (let l = 0; l < maxLines; l++) {
            try {
                let line = sourceHandler.getLine(l).trimRight();

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
                logCOBOLChannelLineException("CobolQuickParse - Parse error : " + e, e);
            }
        }

        // Do we have some sections?
        if (this.sectionsInToken === 0 && this.divisionsInToken === 0) {
            /* if we have items that could be in a data division */

            if (this.procedureDivisionRelatedTokens !== 0 && this.procedureDivisionRelatedTokens > this.workingStorageRelatedTokens) {
                let fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Procedure", "Division", "Procedure Division (CopyBook)", this.currentDivision);
                this.currentDivision = fakeDivision;
                this.procedureDivision = fakeDivision;
                this.pickFields = false;
                this.inProcedureDivision = true;
                this.sourceLooksLikeCOBOL = true;
            }
            else if ((this.workingStorageRelatedTokens !== 0 && this.numberTokensInHeader !== 0)) {
                let fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Data", "Division", "Data Division (CopyBook)", this.currentDivision);
                this.currentDivision = fakeDivision;
                this.pickFields = true;
                this.inProcedureDivision = false;
                this.sourceLooksLikeCOBOL = true;
            }
        }

        if (this.sectionsInToken !== 0 || this.divisionsInToken !== 0) {
            this.sourceLooksLikeCOBOL = true;
        }

        /* leave early */
        if (this.sourceLooksLikeCOBOL === false) {
            return;
        }

        prevToken = Token.Blank;
        for (let l = 0; l < sourceHandler.getLineCount(); l++) {
            try {
                let line = sourceHandler.getLine(l).trimRight();

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
                logCOBOLChannelLineException("CobolQuickParse - Parse error", e);
            }
        }
        this.updateEndings(sourceHandler);

        if (isCachingEnabled() && this.sourceLooksLikeCOBOL === true) {
            QuickCOBOLParse.processOneFile(this);
        }
    }

    private newCOBOLToken(tokenType: COBOLTokenStyle, startLine: number, line: string, token: string,
        description: string, parentToken: COBOLToken | undefined): COBOLToken {
        let ctoken = new COBOLToken(tokenType, startLine, line, token, description, parentToken);
        this.tokensInOrder.push(ctoken);
        return ctoken;

    }

    public static isFile(fileName: string): boolean {

        if (fs.existsSync(fileName) === false) {
            return false;
        }

        try {
            let stat: fs.Stats = fs.statSync(fileName);
            if (stat.isFile() && !stat.isDirectory()) {
                return true;
            }

        } catch (err) {

        }

        return false;
    }

    public static getCachedObject(document: TextDocument | undefined, fileName: string): QuickCOBOLParse | undefined {
        if (this.isFile(fileName) === false) {
            return undefined;
        }

        let cachedObject: QuickCOBOLParse | undefined = undefined;

        if (InMemoryCache.has(fileName)) {
            cachedObject = InMemoryCache.get(fileName);
        }

        /* if the document is edited, drop the in cached object */
        if (document !== undefined && document.isDirty) {
            InMemoryCache.delete(fileName);
            let file = new VSCodeSourceHandler(document, false);
            return new QuickCOBOLParse(file, document.fileName);
        }

        /* does the cache object need to be updated? */
        if (cachedObject !== null && cachedObject !== undefined) {
            let stat: fs.Stats = fs.statSync(fileName);
            if (cachedObject.lastModifiedTime !== stat.mtimeMs) {
                InMemoryCache.delete(fileName);
                cachedObject = undefined;
            }
        }

        /* grab, the file parse it can cache it */
        if (cachedObject === null || cachedObject === undefined) {
            try {
                var startTime = performance.now();

                let file = new FileSourceHandler(fileName, false);
                let qcpd = new QuickCOBOLParse(file, fileName);

                InMemoryCache.set(fileName, qcpd);
                logCOBOLChannelLine(' - Load/Parse - Execution time: %dms -> ' + fileName, (performance.now() - startTime).toFixed(2));
                if (InMemoryCache.size > 20) {
                    let firstKey = InMemoryCache.keys().next().value;
                    InMemoryCache.delete(firstKey);
                }
                return qcpd;
            }
            catch (e) {
                logCOBOLChannelLineException("getCachedObject", e);
            }
        }

        return cachedObject;
    }

    public static processOneFile(qcp: QuickCOBOLParse) {

        let filename = qcp.filename;

        if (COBOLSymbolTableHelper.cacheUpdateRequired(filename)) {
            let qcp_symtable: COBOLSymbolTable = COBOLSymbolTableHelper.getCOBOLSymbolTable(qcp);

            COBOLSymbolTableHelper.saveToFile(qcp_symtable);
        }

        /* iterater through all the known copybook references */
        for (let [key, value] of qcp.getcopyBooksUsed()) {
            try {
                let copyBookfilename: string = "";
                try {
                    copyBookfilename = expandLogicalCopyBookToFilenameOrEmpty(key);
                    if (copyBookfilename.length !== 0) {
                        if (COBOLSymbolTableHelper.cacheUpdateRequired(copyBookfilename)) {
                            logCOBOLChannelLine("   CopyBook: "+key+" => "+copyBookfilename);
                            let filefs_vb = new FileSourceHandler(copyBookfilename, false);
                            let qcp_vb = new QuickCOBOLParse(filefs_vb, copyBookfilename);
                            let qcp_symtable: COBOLSymbolTable = COBOLSymbolTableHelper.getCOBOLSymbolTable(qcp_vb);

                            COBOLSymbolTableHelper.saveToFile(qcp_symtable);
                        } else {
                            logCOBOLChannelLine("   CopyBook: "+key+" (no update required)");
                        }
                    } else {
                        logCOBOLChannelLine("   CopyBook: "+key+" (not found)");
                    }
                }
                catch (ex) {
                    if (copyBookfilename !== null) {
                        logCOBOLChannelLineException("processOneFile:" + copyBookfilename, ex);
                    } else {
                        logCOBOLChannelLineException("processOneFile:", ex);
                    }
                }
            }
            catch (fe) {
                logCOBOLChannelLineException("processOneFile", fe);
            }

        }
    }

    public static dumpMetaData() {
        activateLogChannel(true);       /* ignore setting */
        if (isCachingEnabled() === false) {
            logCOBOLChannelLine("Metadata is not enabled");
            return;
        }

        let cacheDir = COBOLSymbolTableHelper.getCacheDirectory();
        logCOBOLChannelLine("Metadata Dump");

        logCOBOLChannelLine(" cache folder   : " + cacheDir);
        logCOBOLChannelLine(" cache_metadata : " + getCachingSetting() + "\n");
        logCOBOLChannelLine(" Last modified  : " + InMemoryGlobalSymbolCache.lastModifiedTime);
        logCOBOLChannelLine(" Dirty flag     : " + InMemoryGlobalSymbolCache.isDirty);
        logCOBOLChannelLine("");
        logCOBOLChannelLine("Global symbols  : (made lowercase)");

        for (let [i, tag] of InMemoryGlobalSymbolCache.callableSymbols.entries()) {
            let fileSymbol: COBOLFileSymbol[] | undefined = InMemoryGlobalSymbolCache.callableSymbols.get(i);
            if (fileSymbol === undefined) {
                logCOBOLChannelLine("  " + i + " => empty");
            } else {
                fileSymbol.forEach(function (value: COBOLFileSymbol) {
                    logCOBOLChannelLine(String.Format(" {0} => {1} @ {2}", i.padEnd(40), value.filename, value.lnum));
                });
            }
        }

        logCOBOLChannelLine("");
        logCOBOLChannelLine("File symbols    :");

        for (var file of fs.readdirSync(cacheDir)) {
            if (file !== "globalsymbols.sym" && file.endsWith(".sym")) {
                let symTable = COBOLSymbolTableHelper.getSymbolTable_direct(path.join(cacheDir, file));
                if (symTable !== undefined) {
                    logCOBOLChannelLine(" " + symTable.fileName + " in " + file);
                    logCOBOLChannelLine("   Label symbol count    : " + symTable.labelSymbols.size);
                    logCOBOLChannelLine("   Variable symbol count : " + symTable.variableSymbols.size);
                }
            }
        }
    }

    private static processAllFilesDirectory(dir: string, recurse: boolean) {
        if (recurse) {
            logCOBOLChannelLine(" - Processing directory : " + dir);
        }
        for (var file of fs.readdirSync(dir)) {
            QuickCOBOLParse.processFileInDirectory(path.join(dir, file), recurse);
        }
    }

    private static processFileInDirectory(filename: string, recurse: boolean) {
        let stat = fs.statSync(filename);

        if (stat.isDirectory() === false) {
            if (isValidExtension(filename)) {
                if (QuickCOBOLParse.isFile(filename) === true) {
                    logCOBOLChannelLine(" - Processing file : " + filename);

                    let filefs = new FileSourceHandler(filename, false);
                    let qcp = new QuickCOBOLParse(filefs, filename);

                    /* iterater through all the known copybook references */
                    for (let [key, value] of qcp.getcopyBooksUsed()) {
                        try {
                            let copyBookfilename: string = "";
                            try {
                                copyBookfilename = expandLogicalCopyBookToFilenameOrEmpty(key);
                                if (copyBookfilename.length !== 0) {
                                    if (COBOLSymbolTableHelper.cacheUpdateRequired(copyBookfilename)) {
                                        let filefs_vb = new FileSourceHandler(copyBookfilename, false);
                                        let qcp_vb = new QuickCOBOLParse(filefs_vb, copyBookfilename);
                                        let qcp_symtable: COBOLSymbolTable = COBOLSymbolTableHelper.getCOBOLSymbolTable(qcp_vb);

                                        COBOLSymbolTableHelper.saveToFile(qcp_symtable);
                                    }
                                }
                            }
                            catch (ex) {
                                if (copyBookfilename !== null) {
                                    logCOBOLChannelLineException("processFileInDirectory/1: " + copyBookfilename, ex);
                                } else {
                                    logCOBOLChannelLineException("processFileInDirectory/1", ex);
                                }
                            }
                        }
                        catch (fe) {
                            logCOBOLChannelLineException("processFileInDirectory/2", fe);
                        }
                    }
                }

            }
        } else {
            if (recurse) {
                if (stat.isDirectory()) {
                    QuickCOBOLParse.processAllFilesDirectory(filename, recurse);
                } else {
                    logCOBOLChannelLine(" - Ignoring : " + filename);
                }
            } 
        }
    }

    public static processAllFilesInWorkspaces(recurse: boolean) {

        activateLogChannel(true);
        if (workspace.workspaceFolders) {
            var start = performance.now();

            try {
                for (var folder of workspace.workspaceFolders) {
                    QuickCOBOLParse.processAllFilesDirectory(folder.uri.fsPath, recurse);
                }
            }
            catch (re) {
                logCOBOLChannelLineException("processAllFilesInWorkspaces", re);
            }
            var end = performance.now() - start;

            logCOBOLChannelLine(util.format('Process all files in workspace -> Execution time: %dms', end.toFixed(2)));
        }

    }

    private static readonly literalRegex = /^[a-zA-Z][a-zA-Z0-9-_]*/g;

    private isValidLiteral(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(QuickCOBOLParse.literalRegex)) {
            return true;
        }

        return false;
    }

    private static readonly paragraphRegex = /^[a-zA-Z0-9][a-zA-Z0-9-_]*/g;

    private isParagraph(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(QuickCOBOLParse.paragraphRegex)) {
            return true;
        }

        return false;
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

    private isNumber(value: string | number): boolean {
        if (value.toString().length === 0) {
            return false;
        }
        return !isNaN(Number(value.toString()));
    }

    private trimLiteral(literal: string) {
        let literalTrimmed = literal.trim();

        /* remove quotes */
        if (literalTrimmed.startsWith("\"") && literalTrimmed.endsWith("\"")) {
            return literalTrimmed.substr(1, literalTrimmed.length - 2);
        }

        /* remove quotes */
        if (literalTrimmed.startsWith("\'") && literalTrimmed.endsWith("\'")) {
            return literalTrimmed.substr(1, literalTrimmed.length - 2);
        }

        /* remove end . */
        if (literalTrimmed.endsWith(".")) {
            return literalTrimmed.substr(0, literalTrimmed.length - 1);
        }

        return literalTrimmed;
    }

    private getColumBParsing(): boolean {
        var editorConfig = workspace.getConfiguration('coboleditor');
        var parsingB = editorConfig.get<boolean>('ignorecolumn_b_onwards');
        if (parsingB === undefined || parsingB === null) {
            parsingB = false;
        }
        return parsingB;
    }

    private getCopybookNestedInSection(): boolean {
        var editorConfig = workspace.getConfiguration('coboleditor');
        var nestedFlag = editorConfig.get<boolean>('copybooks_nested');
        if (nestedFlag === undefined || nestedFlag === null) {
            nestedFlag = false;
        }
        return nestedFlag;
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
                            this.sectionsInToken++;
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
                logCOBOLChannelLineException("Cobolquickparse relaxedParseLineByLine line error: ", e);
            }
        }
        while (token.moveToNextToken() === false);

        return token;
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

                if (tcurrent.endsWith(".")) {
                    tcurrent = tcurrent.substr(0, tcurrent.length - 1);
                    tcurrentLower = tcurrent.toLowerCase();
                    endWithDot = true;
                    token.endsWithDot = endWithDot;
                } else {
                    token.endsWithDot = false;
                }

                const current: string = tcurrent;
                const currentLower: string = tcurrentLower;
                const nextToken = token.nextToken;
                const nextTokenLower = token.nextTokenLower;
                const prevToken = this.trimLiteral(token.prevToken);
                const prevTokenLower = this.trimLiteral(token.prevTokenLower);
                const nextPlusOneToken = token.nextPlusOneToken;

                let prevPlusCurrent = token.prevToken + " " + current;

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

                // handle sections)
                if (this.currentClass === COBOLToken.Null && prevToken.length !== 0 && currentLower === "section" && (prevTokenLower !== 'exit')) {
                    if (prevTokenLower === "declare") {
                        continue;
                    }

                    // So we need to insert a fake data division?
                    if (this.currentDivision === COBOLToken.Null || this.currentDivision.tokenNameLower.startsWith("data") === false) {
                        if (prevTokenLower === 'file' ||
                            prevTokenLower === 'working-storage' ||
                            prevTokenLower === 'local-storage' ||
                            prevTokenLower === 'screen' ||
                            prevTokenLower === 'linkage') {

                            this.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Data", "Division", "Data Division (Optional)", this.currentDivision);
                        }
                    }

                    this.currentSection = this.newCOBOLToken(COBOLTokenStyle.Section, lineNumber, line, prevToken, prevPlusCurrent, this.currentDivision);
                    this.sections.set(prevTokenLower, this.currentSection);

                    if (prevTokenLower === "working-storage" || prevTokenLower === "linkage" || prevTokenLower === "file") {
                        this.pickFields = true;
                        this.inProcedureDivision = false;
                        sourceHandler.setDumpAreaA(false);
                        sourceHandler.setDumpAreaBOnwards(!this.parseColumnBOnwards);
                    }

                    continue;
                }

                // handle divisions
                if (this.captureDivisions && prevTokenLower.length !== 0 && currentLower === "division") {
                    this.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, line, prevPlusCurrent, prevPlusCurrent, COBOLToken.Null);

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
                    let ctoken = this.newCOBOLToken(COBOLTokenStyle.EntryPoint, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);
                    this.callTargets.set(currentLower, ctoken);
                    continue;
                }

                // handle program-id
                if (prevTokenLower === "program-id" && current.length !== 0) {
                    let ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);
                    this.callTargets.set(currentLower, ctoken);

                    // So we don't have any division?
                    if (this.currentDivision === COBOLToken.Null) {
                        this.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Identification", "Division", "Identification Division (Optional)", this.currentDivision);
                    }
                    continue;
                }

                // handle class-id
                if (prevTokenLower === "class-id" && current.length !== 0) {
                    this.currentClass = this.newCOBOLToken(COBOLTokenStyle.ClassId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);
                    this.captureDivisions = false;
                    this.currentMethod = COBOLToken.Null;
                    this.pickFields = true;
                    this.classes.set(currentLower, this.currentClass);

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
                    this.newCOBOLToken(COBOLTokenStyle.FunctionId, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);
                    continue;
                }

                // handle method-id
                if (prevTokenLower === "method-id" && current.length !== 0) {
                    let currentLowerTrim = this.trimLiteral(currentLower);
                    let style = currentLowerTrim === "new" ? COBOLTokenStyle.Constructor : COBOLTokenStyle.MethodId;

                    if (nextTokenLower === "property") {
                        this.currentMethod = this.newCOBOLToken(COBOLTokenStyle.Property, lineNumber, line, this.trimLiteral(nextPlusOneToken), nextToken + " " + nextPlusOneToken, this.currentDivision);
                    } else {
                        this.currentMethod = this.newCOBOLToken(style, lineNumber, line, this.trimLiteral(current), prevPlusCurrent, this.currentDivision);
                    }

                    this.pickFields = true;
                    this.captureDivisions = false;
                    this.methods.set(currentLowerTrim, this.currentMethod);
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

                // copybook handling
                if (prevTokenLower === "copy" && current.length !== 0) {
                    let trimmedCopyBook = this.trimLiteral(current);
                    if (this.copyBooksUsed.has(trimmedCopyBook) === false) {
                        this.copyBooksUsed.set(trimmedCopyBook, current);
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
                    continue;
                }

                // we are in the procedure division
                if (this.captureDivisions && this.currentDivision !== COBOLToken.Null &&
                    this.currentDivision === this.procedureDivision && endWithDot) {
                    if (!this.isValidKeyword(prevTokenLower) && !this.isValidKeyword(currentLower)) {
                        let beforeCurrent = line.substr(0, token.currentCol - 1).trim();
                        if (beforeCurrent.length === 0) {
                            let c = token.currentToken.substr(0, token.currentToken.length - 1);
                            if (c.length !== 0) {
                                if (this.isParagraph(c)) {
                                    if (this.currentSection !== COBOLToken.Null) {
                                        let newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, c, c, this.currentSection);
                                        this.currentSection.childTokens.push(newToken);
                                        this.paragraphs.set(newToken.tokenNameLower, newToken);
                                    } else {
                                        let newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, c, c, this.currentDivision);
                                        this.currentDivision.childTokens.push(newToken);
                                        this.paragraphs.set(newToken.tokenNameLower, newToken);
                                    }
                                }
                            }
                        }
                    }
                }

                // guess fields
                if (this.guessFields && this.currentDivision === COBOLToken.Null) {
                    if (this.isNumber(prevToken) && !this.isNumber(current) && nextTokenLower.startsWith("pic")) {
                        this.pickFields = true;

                        let fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Data", "Division", "Data Division (Optional)", this.currentDivision);
                        this.currentDivision = fakeDivision;

                        let fakeWorkingStorage = this.newCOBOLToken(COBOLTokenStyle.Section, lineNumber, "Working", "Storage", "Working Storage (Optional)", this.currentDivision);
                        fakeDivision.childTokens.push(fakeWorkingStorage);
                        this.currentSection = fakeWorkingStorage;
                    }
                }

                // are we in the working-storage section?
                if (this.pickFields) {
                    /* only interesting in things that are after a number */
                    if (this.isNumber(prevToken) && !this.isNumber(current)) {
                        if (!this.isValidKeyword(prevTokenLower) && !this.isValidKeyword(currentLower)) {
                            let trimToken = this.trimLiteral(current);
                            if (this.isValidLiteral(currentLower)) {
                                const style = prevToken === "78" ? COBOLTokenStyle.Constant : COBOLTokenStyle.Variable;
                                let constantToken = this.newCOBOLToken(style, lineNumber, line, trimToken, trimToken, this.currentDivision);
                                this.addVariableOrConstant(currentLower, constantToken);
                            }
                        }
                        continue;
                    }

                    if ((prevTokenLower === "fd" || prevTokenLower === "sd") && !this.isValidKeyword(currentLower)) {
                        let trimToken = this.trimLiteral(current);
                        if (this.isValidLiteral(currentLower)) {
                            let variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, trimToken, trimToken, this.currentDivision);
                            this.addVariableOrConstant(currentLower, variableToken);
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
            }
            catch (e) {
                logCOBOLChannelLineException("Cobolquickparse line error: ", e);
            }
        }
        while (token.moveToNextToken() === false);

        return token;
    }

    public getcopyBooksUsed(): Map<string, string> {
        return this.copyBooksUsed;
    }

    private processDivsionToken(sourceHandler: ISourceHandler, tokens: COBOLToken[], style: COBOLTokenStyle) {

        try {
            for (let i = 0; i < tokens.length; i++) {
                let token = tokens[i];
                if (token.tokenType === style || style === COBOLTokenStyle.Null) {
                    if (1 + i < tokens.length) {
                        let nextToken = tokens[i + 1];
                        token.endLine = nextToken.startLine - 1;          /* use the end of the previous line */
                        if (token.endLine < 0) {
                            token.endLine = 0;
                        }
                        token.endColumn = sourceHandler.getRawLine(token.endLine).length;
                    } else {
                        if (token.tokenType !== COBOLTokenStyle.EndDelimiter) {
                            token.endLine = sourceHandler.getLineCount();
                            if (token.endLine < 0) {
                                token.endLine = 0;
                            }
                            token.endColumn = sourceHandler.getRawLine(token.endLine).length;
                        }
                    }
                }
                this.processDivsionToken(sourceHandler, token.childTokens, style);
            }
        }
        catch (e) {
            logCOBOLChannelLineException("Cobolquickparse/processInsideTokens line error: ", e);
        }
    }

    private updateEndings(sourceHandler: ISourceHandler) {

        let divisions: COBOLToken[] = [];

        for (let i = 0; i < this.tokensInOrder.length; i++) {
            let token = this.tokensInOrder[i];
            if (token.tokenType === COBOLTokenStyle.Division) {
                divisions.push(token);

                /* pick up the section and place them in the division */
                for (let s = 1 + i; s < this.tokensInOrder.length; s++) {
                    let insideToken = this.tokensInOrder[s];
                    if (insideToken.tokenType === COBOLTokenStyle.Division) {
                        token.childTokens.push(insideToken.getEndDelimiterToken());
                        break;
                    }
                    if (insideToken.tokenType === COBOLTokenStyle.Section) {
                        token.childTokens.push(insideToken);
                    }
                }
            }

            if (token.tokenType === COBOLTokenStyle.Section) {
                /* pick up the section and place them in the division */
                for (let s = 1 + i; s < this.tokensInOrder.length; s++) {
                    let insideToken = this.tokensInOrder[s];

                    if (insideToken.tokenType === COBOLTokenStyle.Section) {
                        token.childTokens.push(insideToken.getEndDelimiterToken());
                        break;
                    }
                    if (insideToken.tokenType === COBOLTokenStyle.Paragraph) {
                        token.childTokens.push(insideToken);
                    }

                    if (this.copybookNestedInSection) {
                        if (insideToken.tokenType === COBOLTokenStyle.CopyBook) {
                            token.childTokens.push(insideToken);
                        }
                    }
                }
            }
        }

        this.processDivsionToken(sourceHandler, divisions, COBOLTokenStyle.Division);
        this.processDivsionToken(sourceHandler, divisions, COBOLTokenStyle.Section);
        this.processDivsionToken(sourceHandler, divisions, COBOLTokenStyle.Paragraph);

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
    public static getCOBOLSymbolTable(qp: QuickCOBOLParse): COBOLSymbolTable {
        let st = new COBOLSymbolTable();
        st.fileName = qp.filename;
        st.lastModifiedTime = qp.lastModifiedTime;

        var startTime = performance.now();
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
        logCOBOLChannelLine("- Creating symbol table for " + st.fileName + ", symbols found: " + qp.tokensInOrder.length + " -> " + (performance.now() - startTime).toFixed(2));
        return st;
    }

    public static getCacheDirectory(): string {
        let cacheDir: string = path.join(os.homedir(), ".vscode_cobol");
        let firstCacheDir = "";

        if (workspace.workspaceFolders) {
            for (var folder of workspace.workspaceFolders) {
                let cacheDir2: string = path.join(folder.uri.fsPath, ".vscode_cobol");
                if (fs.existsSync(cacheDir2)) {
                    return cacheDir2;
                }
                if (firstCacheDir === "") {
                    firstCacheDir = cacheDir2;
                }
            }

            if (fs.existsSync(firstCacheDir) === false) {
                fs.mkdirSync(firstCacheDir);
            }

            if (fs.existsSync(firstCacheDir)) {
                return firstCacheDir;
            }
        }

        if (fs.existsSync(cacheDir) === false) {
            fs.mkdirSync(cacheDir);
        }

        return cacheDir;

    }

    private static getHashForFilename(filename: string) {
        let hash: Hash = crypto.createHash('sha256');
        hash.update(filename);
        return hash.digest('hex');
    }

    public static saveToFile(st: COBOLSymbolTable) {
        let fn = path.join(this.getCacheDirectory(), this.getHashForFilename(st.fileName) + ".sym");

        fs.writeFileSync(fn, lzjs.compress(JSON.stringify(st, replacer)));
    }

    public static cacheUpdateRequired(nfilename: string): boolean {
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

        let fn: string = path.join(this.getCacheDirectory(), this.getHashForFilename(filename) + ".sym");
        if (fs.existsSync(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            let stat4src = fs.statSync(filename);
            if (stat4cache.mtimeMs < stat4src.mtimeMs) {
                return true;
            }
            return false;
        }

        return true;
    }

    public static getSymbolTableGivenFile(nfilename: string): COBOLSymbolTable | undefined {
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

        let fn: string = path.join(this.getCacheDirectory(), this.getHashForFilename(filename) + ".sym");
        if (fs.existsSync(fn)) {
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

    public static loadInMemoryGlobalSymbolCaches() {
        let symbolDir = COBOLSymbolTableHelper.getCacheDirectory();
        let fn: string = path.join(symbolDir, globalSymbolFilename);
        if (fs.existsSync(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            if (stat4cache.mtimeMs !== InMemoryGlobalSymbolCache.lastModifiedTime) {
                try {

                    let str: string = fs.readFileSync(fn).toString();
                    let cachableTable: COBOLGlobalSymbolTable = JSON.parse(lzjs.decompress(str), reviver);
                    InMemoryGlobalSymbolCache.callableSymbols = cachableTable.callableSymbols;
                    InMemoryGlobalSymbolCache.lastModifiedTime = stat4cache.mtimeMs;
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

    public static saveInMemoryGlobalCaches() {
        if (InMemoryGlobalSymbolCache.isDirty === false && InMemoryGlobalFileCache.isDirty === false) {
            return;
        }

        let symbolDir = COBOLSymbolTableHelper.getCacheDirectory();

        let fnGlobalSymbolFilename: string = path.join(symbolDir, globalSymbolFilename);
        fs.writeFileSync(fnGlobalSymbolFilename, lzjs.compress(JSON.stringify(InMemoryGlobalSymbolCache, replacer)));
        InMemoryGlobalSymbolCache.isDirty = false;


        let fnFileSymbolFilename: string = path.join(symbolDir, fileSymbolFilename);
        fs.writeFileSync(fnFileSymbolFilename, lzjs.compress(JSON.stringify(InMemoryGlobalFileCache, replacer)));
        InMemoryGlobalFileCache.isDirty = false;
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

    public static getGlobalSymbolCache(): COBOLGlobalSymbolTable {
        return InMemoryGlobalSymbolCache;
    }
}

const globalSymbolFilename: string = "globalsymbols.sym";
const fileSymbolFilename: string = "filesymbols.sym";

const InMemoryCache: Map<string, any> = new Map<string, any>();
const InMemorySymbolCache: Map<string, COBOLSymbolTable> = new Map<string, COBOLSymbolTable>();

const InMemoryGlobalSymbolCache: COBOLGlobalSymbolTable = new COBOLGlobalSymbolTable();
const InMemoryGlobalFileCache: COBOLGlobalFileTable = new COBOLGlobalFileTable();
