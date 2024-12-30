/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { ISourceHandler, ICommentCallback, ISourceHandlerLite } from "./isourcehandler";
import { cobolProcedureKeywordDictionary, cobolStorageKeywordDictionary, getCOBOLKeywordDictionary } from "./keywords/cobolKeywords";

import { FileSourceHandler } from "./filesourcehandler";
import { COBOLFileAndColumnSymbol, COBOLFileSymbol, COBOLWorkspaceFile } from "./cobolglobalcache";

import { ICOBOLSettings } from "./iconfiguration";
import { CobolLinterProviderSymbols, ESourceFormat, IExternalFeatures } from "./externalfeatures";

import * as path from "path";
import { SourceFormat } from "./sourceformat";
import { ExtensionDefaults } from "./extensionDefaults";
import { SplitTokenizer } from "./splittoken";
import { SourcePorter, portResult } from "./vsdirectivesconv";

export enum COBOLTokenStyle {
    CopyBook = "Copybook",
    CopyBookInOrOf = "CopybookInOrOf",
    File = "File",
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
    ConditionName = "ConditionName",
    RenameLevel = "RenameLevel",
    Constant = "Constant",
    Union = "Union",
    EndDelimiter = "EndDelimiter",
    Exec = "Exec",
    EndExec = "EndExec",
    Declaratives = "Declaratives",
    EndDeclaratives = "EndDeclaratives",
    Region = "Region",
    SQLCursor = "SQLCursor",
    Unknown = "Unknown",
    IgnoreLS = "IgnoreLS",
    Null = "Null"
}

export class SourceScannerUtils {
    public static camelize(text: string): string {
        let ret = "";
        let uppercaseNext = true;
        for (let c = 0; c < text.length; c++) {
            const ch = text[c];
            if (uppercaseNext) {
                ret += ch.toUpperCase();
                uppercaseNext = false;
            } else {
                if (ch === "-" || ch === "_") {
                    uppercaseNext = true;
                }

                ret += ch.toLowerCase();
            }
        }

        return ret;
    }
}

export class COBOLToken {
    public ignoreInOutlineView: boolean;
    public readonly filenameAsURI: string;
    public readonly filename: string;
    public readonly tokenType: COBOLTokenStyle;

    public readonly startLine: number;
    public readonly startColumn: number;
    public readonly endLine: number;
    public readonly endColumn: number;

    public rangeStartLine: number;
    public rangeStartColumn: number;
    public rangeEndLine: number;
    public rangeEndColumn: number;

    public readonly tokenName: string;
    public readonly tokenNameLower: string;

    public description: string;
    public readonly parentToken: COBOLToken | undefined;
    public readonly inProcedureDivision: boolean;

    public extraInformation1: string;
    public inSection: COBOLToken | undefined;

    public readonly sourceHandler: ISourceHandler;
    public readonly isTokenFromSourceDependancyCopyBook: boolean;

    public readonly isImplicitToken;

    public constructor(
        sourceHandler: ISourceHandler,
        filenameAsURI: string, filename: string,
        tokenType: COBOLTokenStyle,
        startLine: number, startColumn: number, token:
            string, description: string,
        parentToken: COBOLToken | undefined,
        inProcedureDivision: boolean,
        extraInformation1: string,
        isTokenFromSourceDependancyCopyBook: boolean,
        isImplicitToken:boolean) {

        this.sourceHandler = sourceHandler;
        this.ignoreInOutlineView = false;
        this.filenameAsURI = filenameAsURI;
        this.filename = filename;
        this.tokenType = tokenType;
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.endLine = this.startLine;
        this.tokenName = token.trim();
        this.tokenNameLower = this.tokenName.toLowerCase();
        this.endColumn = this.startColumn + this.tokenName.length;

        this.description = description;
        this.parentToken = parentToken;
        this.inProcedureDivision = inProcedureDivision;
        this.extraInformation1 = extraInformation1;
        this.inSection = undefined;

        this.isTokenFromSourceDependancyCopyBook = isTokenFromSourceDependancyCopyBook;
        this.isImplicitToken = isImplicitToken;

        this.rangeStartLine = this.startLine;
        this.rangeStartColumn = this.startColumn;
        this.rangeEndLine = this.endLine;
        this.rangeEndColumn = this.endColumn;

        if (this.tokenName.length !== 0) {
            /* ensure we don't have any odd start columns */
            if (this.startColumn < 0) {
                this.startColumn = 0;
            }
        }
    }
}

export class COBOLVariable {
    public readonly ignoreInOutlineView: boolean;
    public readonly token: COBOLToken;
    public readonly tokenType: COBOLTokenStyle;

    constructor(token: COBOLToken) {
        this.token = token;
        this.tokenType = token.tokenType;
        this.ignoreInOutlineView = token.ignoreInOutlineView;
    }
}

export class SQLDeclare {
    public readonly ignoreInOutlineView: boolean;
    public readonly token: COBOLToken;
    public readonly currentLine: number;
    public readonly sourceReferences: SourceReference[];

    constructor(token: COBOLToken, currentLine: number) {
        this.token = token;
        this.currentLine = currentLine;
        this.ignoreInOutlineView = token.ignoreInOutlineView;
        this.sourceReferences = [];
    }
}

export class SourceReference_Via_Length {
    public readonly fileIdentifer: number;
    public readonly line: number;
    public readonly column: number;
    public readonly length: number;
    public tokenStyle: COBOLTokenStyle;
    public readonly isSourceDepCopyBook: boolean;
    public readonly name: string;
    public readonly nameLower: string;
    public readonly reason: string;;

    public constructor(fileIdentifer: number, line: number, column: number, length: number, tokenStyle: COBOLTokenStyle, isSourceDepCopyBook: boolean, name: string, reason: string) {
        this.fileIdentifer = fileIdentifer;
        this.line = line;
        this.column = column;
        this.length = length;
        this.tokenStyle = tokenStyle;
        this.isSourceDepCopyBook = isSourceDepCopyBook;
        this.name = name;
        this.nameLower = name.toLowerCase();
        this.reason = reason;
    }
}

export class SourceReference {
    public readonly fileIdentifer: number;
    public readonly line: number;
    public readonly column: number;

    public readonly endLine: number;
    public readonly endColumn: number;

    public readonly tokenStyle: COBOLTokenStyle;

    public constructor(fileIdentifer: number, line: number, column: number, endLine: number, endColumn: number, tokenStyle: COBOLTokenStyle) {
        this.fileIdentifer = fileIdentifer;
        this.line = line;
        this.column = column;
        this.endLine = endLine;
        this.endColumn = endColumn;
        this.tokenStyle = tokenStyle;
    }
}

class StreamToken {
    public currentToken: string;
    public currentTokenLower: string;
    public endsWithDot: boolean;
    public currentCol: number;
    public currentLine: number;

    public static readonly Blank = new StreamToken("", "", false, 0, 0);

    public constructor(currentToken: string, currentTokenLower: string, endsWithDot: boolean, currentLine: number, currentCol: number) {
        this.currentToken = currentToken;
        this.currentTokenLower = currentTokenLower;
        this.endsWithDot = endsWithDot;
        this.currentCol = currentCol;
        this.currentLine = currentLine;
    }
}

class StreamTokens {
    public currentToken = "";
    public currentTokenLower = "";
    public currentCol = 0;
    public currentLineNumber: number;
    public endsWithDot = false;

    public prevTokenLower = "";
    public prevToken = "";
    public prevTokenLineNumber: number = 0;
    public prevCurrentCol: number = 0;

    private tokenIndex = 0;
    private stokens: StreamToken[] = [];

    public constructor(line: string, lineNumber: number, previousToken: StreamTokens | undefined) {
        const lineTokens: string[] = [];
        // this.prevTokenToken = previousToken;
        this.currentLineNumber = lineNumber;
        if (previousToken !== undefined) {
            this.prevToken = previousToken.currentToken;
            this.prevTokenLineNumber = previousToken.currentLineNumber;
        }
        SplitTokenizer.splitArgument(line, lineTokens);
        let rollingColumn = 0;
        for (let c = 0; c < lineTokens.length; c++) {
            const currentToken = lineTokens[c];
            const currentTokenLower = currentToken.toLowerCase();

            rollingColumn = line.indexOf(currentToken, rollingColumn);

            const endsWithDot = currentToken.length === 0 ? false : currentToken.charAt(currentToken.length - 1) === ".";
            this.stokens.push(new StreamToken(currentToken, currentTokenLower, endsWithDot, lineNumber, rollingColumn));
            rollingColumn += currentToken.length;
        }
        this.tokenIndex = 0;
        this.setupNextToken();

        if (previousToken !== undefined) {
            // wire in previous token into this token
            if (previousToken.stokens.length > 0) {
                const prevTokenId = previousToken.stokens.length - 1;
                this.prevToken = previousToken.stokens[prevTokenId].currentToken;
                this.prevTokenLower = previousToken.stokens[prevTokenId].currentTokenLower;
                this.prevCurrentCol = previousToken.stokens[prevTokenId].currentCol;
            }
        }
    }

    public static readonly Blank = new StreamTokens("", 0, undefined);

    public nextSTokenOrBlank(): StreamToken {
        if (1 + this.tokenIndex >= this.stokens.length) {
            return StreamToken.Blank;
        }

        return this.stokens[1 + this.tokenIndex];
    }

    public nextSTokenIndex(index: number): StreamToken {
        const nextIndex = this.tokenIndex + index;
        if (nextIndex >= this.stokens.length) {
            return StreamToken.Blank;
        }

        return this.stokens[nextIndex];
    }

    public compoundItems(startCompound: string) {
        if (this.endsWithDot) {
            return startCompound;
        }

        if (1 + this.tokenIndex >= this.stokens.length) {
            return startCompound;
        }

        let comp = startCompound;
        let addNext = false;
        for (let sc = 1 + this.tokenIndex; sc < this.stokens.length; sc++) {
            const stok = this.stokens[sc];
            const trimCurrent = COBOLSourceScanner.trimLiteral(stok.currentToken, false);
            if (stok.endsWithDot) {
                return comp + " " + trimCurrent;
            }
            if (addNext) {
                comp += " " + trimCurrent;
                addNext = false;
            } else if (stok.currentToken === "&") {
                comp += " " + trimCurrent;
                addNext = true;
            } else {
                return comp;
            }
        }

        return comp;
    }

    private setupNextToken() {
        this.prevToken = this.currentToken;
        this.prevTokenLower = this.currentTokenLower;
        this.prevTokenLineNumber = this.currentLineNumber;
        this.prevCurrentCol = this.currentCol;

        const stok: StreamToken = this.stokens[this.tokenIndex];
        if (stok !== undefined) {
            this.currentToken = stok.currentToken;
            this.currentTokenLower = stok.currentTokenLower;
            this.endsWithDot = stok.endsWithDot;
            this.currentCol = stok.currentCol;
            this.currentLineNumber = stok.currentLine;
        } else {
            this.currentToken = this.currentTokenLower = "";
            this.endsWithDot = false;
            this.currentCol = 0;
            this.currentLineNumber = 0;
        }
    }

    public moveToNextToken(): boolean {
        if (1 + this.tokenIndex > this.stokens.length) {
            return true;
        }

        this.tokenIndex++;
        this.setupNextToken();
        return false;
    }

    public endToken() {
        this.tokenIndex = this.stokens.length;
    }

    public isTokenPresent(possibleToken: string): boolean {
        const possibleTokenLower = possibleToken.toLowerCase();
        const possibleTokenLowerDot = possibleTokenLower + ".";

        for (let c = 0; c < this.stokens.length; c++) {
            if (this.stokens[c].currentTokenLower === possibleTokenLower) {
                return true;
            }
            if (this.stokens[c].currentTokenLower === possibleTokenLowerDot) {
                return true;
            }
        }

        return false;
    }
}

export class replaceToken {
    private readonly replaceToken: string;
    public readonly rex4wordreplace: RegExp;

    constructor(replaceTokenRaw: string, tokenState: IReplaceState) {
        this.replaceToken = this.escapeRegExp(replaceTokenRaw);
        if (tokenState.isPseudoTextDelimiter) {
            this.rex4wordreplace = new RegExp(`${this.replaceToken}`, "g");
        } else {
            this.rex4wordreplace = new RegExp(`\\b${this.replaceToken}\\b`, "g");
        }
    }

    private escapeRegExp(text: string) {
        return text.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");
    }
}

export interface IReplaceState {
    isPseudoTextDelimiter: boolean;
}

export class replaceState implements IReplaceState {
    public isPseudoTextDelimiter = false;
}

export class copybookState implements IReplaceState {
    public sourceHandler: ISourceHandler | undefined = undefined;
    public copyBook = "";
    public trimmedCopyBook = "";
    public isIn = false;
    public isOf = false;
    public isReplacing = false;
    public isReplacingBy = false;
    public startLineNumber = 0;
    public endLineNumber = 0;
    public endCol = 0;
    public startCol = 0;
    public line = "";
    public copyVerb = "";
    public literal2 = "";
    public library_name = "";
    public replaceLeft = "";
    public copyReplaceMap = new Map<string, replaceToken>();
    public isTrailing = false;
    public isLeading = false;
    public fileName = "";
    public isPseudoTextDelimiter = false;
}

export class COBOLCopybookToken {
    public readonly token: COBOLToken | undefined;
    public scanComplete: boolean;
    public statementInformation: copybookState | undefined;

    public static readonly Null = new COBOLCopybookToken(undefined, false, undefined);

    constructor(token: COBOLToken | undefined, parsed: boolean, statementInformation: copybookState | undefined) {
        this.token = token;
        this.scanComplete = parsed;
        this.statementInformation = statementInformation;
    }
}

export class SharedSourceReferences {
    public filenames: string[];
    public filenameURIs: string[];

    public readonly targetReferences: Map<string, SourceReference_Via_Length[]>;
    public readonly constantsOrVariablesReferences: Map<string, SourceReference_Via_Length[]>;
    public readonly unknownReferences: Map<string, SourceReference_Via_Length[]>;
    public readonly ignoreLSRanges: SourceReference[];

    public readonly sharedConstantsOrVariables: Map<string, COBOLVariable[]>;
    public readonly sharedSections: Map<string, COBOLToken>;
    public readonly sharedParagraphs: Map<string, COBOLToken>;
    public readonly copyBooksUsed: Map<string, COBOLCopybookToken>;
    public readonly execSQLDeclare: Map<string, SQLDeclare>;
    public state: ParseState;
    public tokensInOrder: COBOLToken[];

    public readonly ignoreUnusedSymbol: Map<string, string>;

    public topLevel: boolean;

    public startTime: number;

    constructor(configHandler: ICOBOLSettings, topLevel: boolean, startTime: number) {
        this.filenames = [];
        this.filenameURIs = [];
        this.targetReferences = new Map<string, SourceReference_Via_Length[]>();
        this.constantsOrVariablesReferences = new Map<string, SourceReference_Via_Length[]>();
        this.unknownReferences = new Map<string, SourceReference_Via_Length[]>();

        this.sharedConstantsOrVariables = new Map<string, COBOLVariable[]>();
        this.sharedSections = new Map<string, COBOLToken>();
        this.sharedParagraphs = new Map<string, COBOLToken>();
        this.copyBooksUsed = new Map<string, COBOLCopybookToken>();
        this.execSQLDeclare = new Map<string, SQLDeclare>();
        this.state = new ParseState(configHandler);
        this.tokensInOrder = [];
        this.topLevel = topLevel;
        this.startTime = startTime;
        this.ignoreUnusedSymbol = new Map<string, string>();
        this.ignoreLSRanges = [];
    }

    public reset(configHandler: ICOBOLSettings) {
        this.filenames = [];
        this.targetReferences.clear();
        this.constantsOrVariablesReferences.clear();
        this.unknownReferences.clear();
        this.sharedConstantsOrVariables.clear();
        this.sharedSections.clear();
        this.sharedParagraphs.clear();
        this.copyBooksUsed.clear();
        this.state = new ParseState(configHandler);
        this.tokensInOrder = [];
        this.ignoreUnusedSymbol.clear();
    }

    public getSourceFieldId(handFilename: string): number {
        let xpos = 0;
        for (const filename of this.filenames) {
            if (filename === handFilename) {
                return xpos;
            }

            xpos++;
        }

        return -1;
    }

    private getReferenceInformation4(refMap: Map<string, SourceReference_Via_Length[]>, sourceFileId: number, variable: string, startLine: number, startColumn: number): [number, number] {
        let defvars = refMap.get(variable);
        if (defvars === undefined) {
            defvars = refMap.get(variable.toLowerCase());
        }

        if (defvars === undefined) {
            return [0, 0];
        }

        let definedCount = 0;
        let referencedCount = 0;

        for (const defvar of defvars) {
            if (defvar.line === startLine && defvar.column === startColumn) {
                // drop anything not releated to this file
                if (defvar.fileIdentifer === sourceFileId) {
                    definedCount++;
                }
            } else {
                referencedCount++;
            }
        }

        return [definedCount, referencedCount];
    }

    public getReferenceInformation4variables(variable: string, sourceFileId: number, startLine: number, startColumn: number): [number, number] {
        return this.getReferenceInformation4(this.constantsOrVariablesReferences, sourceFileId, variable, startLine, startColumn);
    }

    public getReferenceInformation4targetRefs(variable: string, sourceFileId: number, startLine: number, startColumn: number): [number, number] {
        return this.getReferenceInformation4(this.targetReferences, sourceFileId, variable, startLine, startColumn);
    }
}

export enum UsingState {
    BY_VALUE,
    BY_REF,
    BY_CONTENT,
    BY_OUTPUT,
    RETURNING,
    UNKNOWN
}

export class COBOLParameter {
    readonly using: UsingState;
    readonly name: string;

    constructor(u: UsingState, n: string) {
        this.using = u;
        this.name = n;
    }
}

export class ParseState {
    currentToken: COBOLToken | undefined;
    currentRegion: COBOLToken | undefined;
    currentDivision: COBOLToken | undefined;
    currentSection: COBOLToken | undefined;
    currentSectionOutRefs: Map<string, SourceReference_Via_Length[]>;
    currentParagraph: COBOLToken | undefined;
    currentClass: COBOLToken | undefined;
    currentMethod: COBOLToken | undefined;
    currentFunctionId: COBOLToken | undefined;
    current01Group: COBOLToken | undefined;
    currentLevel: COBOLToken | undefined;
    currentProgramTarget: CallTargetInformation;

    copyBooksUsed: Map<string, COBOLToken | undefined>;

    procedureDivision: COBOLToken | undefined;
    declaratives: COBOLToken | undefined;
    captureDivisions: boolean;
    programs: COBOLToken[];
    pickFields: boolean;
    pickUpUsing: boolean;
    skipToDot: boolean;
    endsWithDot: boolean;
    prevEndsWithDot: boolean;
    currentLineIsComment: boolean;
    skipNextToken: boolean;
    inValueClause: boolean;
    skipToEndLsIgnore: boolean;

    inProcedureDivision: boolean;
    inDeclaratives: boolean;
    inReplace: boolean;
    replace_state: replaceState;

    inCopy: boolean;
    replaceLeft: string;
    replaceRight: string;
    captureReplaceLeft: boolean;
    ignoreInOutlineView: boolean;

    addReferencesDuringSkipToTag: boolean;
    addVariableDuringStipToTag: boolean;

    using: UsingState;

    parameters: COBOLParameter[];

    entryPointCount: number;

    replaceMap: Map<string, replaceToken>;

    enable_text_replacement: boolean;

    copybook_state: copybookState;

    inCopyStartColumn: number;

    constructor(configHandler: ICOBOLSettings) {
        this.currentDivision = undefined;
        this.procedureDivision = undefined;
        this.currentSection = undefined;
        this.currentParagraph = undefined;
        this.currentToken = undefined;
        this.currentClass = undefined;
        this.currentMethod = undefined;
        this.currentRegion = undefined;
        this.declaratives = undefined;
        this.current01Group = undefined;
        this.currentLevel = undefined;
        this.currentFunctionId = undefined;
        this.currentProgramTarget = new CallTargetInformation("", undefined, false, []);
        this.programs = [];
        this.captureDivisions = true;
        this.copyBooksUsed = new Map<string, COBOLToken>();
        this.pickFields = false;
        this.inProcedureDivision = false;
        this.inDeclaratives = false;
        this.ignoreInOutlineView = false;
        this.skipToDot = false;
        this.addReferencesDuringSkipToTag = false;
        this.addVariableDuringStipToTag = false;
        this.pickUpUsing = false;
        this.using = UsingState.BY_REF;
        this.parameters = [];
        this.entryPointCount = 0;
        this.endsWithDot = false;
        this.prevEndsWithDot = false;
        this.currentLineIsComment = false;
        this.inReplace = false;
        this.replace_state = new replaceState();
        this.inCopy = false;
        this.captureReplaceLeft = true;
        this.replaceLeft = "";
        this.replaceRight = "";
        this.replaceMap = new Map<string, replaceToken>();
        this.enable_text_replacement = configHandler.enable_text_replacement;
        this.copybook_state = new copybookState();
        this.inCopyStartColumn = 0;
        this.skipNextToken = false;
        this.inValueClause = false;
        this.skipToEndLsIgnore = false;
        this.currentSectionOutRefs = new Map<string, SourceReference_Via_Length[]>
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

    public Token: COBOLToken | undefined;
    public OriginalToken: string;
    public IsEntryPoint: boolean;
    public CallParameters: COBOLParameter[];

    constructor(originalToken: string, token: COBOLToken | undefined, isEntryPoint: boolean, params: COBOLParameter[]) {
        this.OriginalToken = originalToken;
        this.Token = token;
        this.IsEntryPoint = isEntryPoint;
        this.CallParameters = params;
    }
}

export interface ICOBOLSourceScannerEventer {
    sendMessage(message: string): void;
}

export interface ICOBOLSourceScannerEvents {
    start(qp: ICOBOLSourceScanner): void;
    processToken(token: COBOLToken): void;
    finish(): void;
}

export interface ICOBOLSourceScanner {
    filename: string;
    // eslint-disable-next-line @typescript-eslint/ban-types
    lastModifiedTime: BigInt;
    copyBooksUsed: Map<string, COBOLCopybookToken>;
    workspaceFile: COBOLWorkspaceFile;
}

export class EmptyCOBOLSourceScannerEventHandler implements ICOBOLSourceScannerEvents {

    static readonly Default = new EmptyCOBOLSourceScannerEventHandler();

    start(qp: ICOBOLSourceScanner): void {
        return;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    processToken(token: COBOLToken): void {
        return;
    }

    finish(): void {
        return;
    }
}

export class COBOLSourceScanner implements ICommentCallback, ICOBOLSourceScanner {
    public id: string;
    public readonly sourceHandler: ISourceHandler;
    public filename: string;
    // eslint-disable-next-line @typescript-eslint/ban-types
    public lastModifiedTime: BigInt = BigInt(0);

    public tokensInOrder: COBOLToken[] = [];
    public execTokensInOrder: COBOLToken[] = [];
    public readonly execSQLDeclare: Map<string, SQLDeclare>;

    public readonly sections: Map<string, COBOLToken>;
    public readonly paragraphs: Map<string, COBOLToken>;
    public readonly constantsOrVariables: Map<string, COBOLVariable[]>;
    public readonly callTargets: Map<string, CallTargetInformation>;
    public readonly functionTargets: Map<string, CallTargetInformation>;
    public readonly classes: Map<string, COBOLToken>;
    public readonly methods: Map<string, COBOLToken>;
    public readonly copyBooksUsed: Map<string, COBOLCopybookToken>;

    public readonly diagMissingFileWarnings: Map<string, COBOLFileSymbol>;
    public readonly portWarnings: portResult[];
    public readonly generalWarnings: COBOLFileSymbol[];

    public readonly commentReferences: COBOLFileAndColumnSymbol[];

    public readonly parse4References: boolean;
    public readonly sourceReferences: SharedSourceReferences;
    public readonly sourceFileId: number;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public cache4PerformTargets: any | undefined = undefined;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public cache4ConstantsOrVars: any | undefined = undefined;

    public ImplicitProgramId = "";
    public ProgramId = "";

    public sourceFormat: ESourceFormat = ESourceFormat.unknown;

    public sourceIsCopybook = false;

    public workspaceFile: COBOLWorkspaceFile;

    public readonly parse_copybooks_for_references: boolean;
    public readonly scan_comments_for_hints: boolean;
    public readonly isSourceDepCopyBook: boolean;
    public readonly scan_comment_for_ls_control: boolean;

    readonly copybookNestedInSection: boolean;

    readonly configHandler: ICOBOLSettings;

    parseHint_OnOpenFiles: string[] = [];
    parseHint_WorkingStorageFiles: string[] = [];
    parseHint_LocalStorageFiles: string[] = [];
    parseHint_ScreenSectionFiles: string[] = [];

    private eventHandler: ICOBOLSourceScannerEvents;
    private externalFeatures: IExternalFeatures;

    public scanAborted: boolean;

    private readonly languageId: string;
    private readonly usePortationSourceScanner: boolean;
    private currentExecToken: COBOLToken | undefined = undefined;
    private currentExec = "";
    private currentExecVerb = "";

    private readonly COBOLKeywordDictionary: Map<string, string>;

    private readonly sourcePorter: SourcePorter = new SourcePorter();

    private readonly activeRegions: COBOLToken[] = [];

    private readonly regions: COBOLToken[] = [];

    private implicitCount = 0;
    
    public static ScanUncached(sourceHandler: ISourceHandler,
        configHandler: ICOBOLSettings,
        parse_copybooks_for_references: boolean,
        eventHandler: ICOBOLSourceScannerEvents,
        externalFeatures: IExternalFeatures
    ): COBOLSourceScanner {

        const startTime = externalFeatures.performance_now();
        return new COBOLSourceScanner(
            startTime,
            sourceHandler,
            configHandler,
            new SharedSourceReferences(configHandler, true, startTime),
            parse_copybooks_for_references,
            eventHandler,
            externalFeatures,
            false
        );
    }

    private static ScanUncachedInlineCopybook(
        sourceHandler: ISourceHandler,
        parentSource: COBOLSourceScanner,
        parse_copybooks_for_references: boolean,
        eventHandler: ICOBOLSourceScannerEvents,
        externalFeatures: IExternalFeatures,
        isSourceDepCopyBook: boolean
    ): COBOLSourceScanner {

        const configHandler = parentSource.configHandler;
        const sharedSource = parentSource.sourceReferences;

        return new COBOLSourceScanner(
            sharedSource.startTime,
            sourceHandler,
            configHandler,
            sharedSource,
            parse_copybooks_for_references,
            eventHandler,
            externalFeatures,
            isSourceDepCopyBook);
    }

    public constructor(
        startTime: number,
        sourceHandler: ISourceHandler, configHandler: ICOBOLSettings,
        sourceReferences: SharedSourceReferences = new SharedSourceReferences(configHandler, true, startTime),
        parse_copybooks_for_references: boolean,
        sourceEventHandler: ICOBOLSourceScannerEvents,
        externalFeatures: IExternalFeatures,
        isSourceDepCopyBook: boolean) {
        const filename = sourceHandler.getFilename();

        this.sourceHandler = sourceHandler;
        this.id = sourceHandler.getUriAsString();
        this.configHandler = configHandler;
        this.filename = path.normalize(filename);
        this.ImplicitProgramId = path.basename(filename, path.extname(filename));
        this.parse_copybooks_for_references = parse_copybooks_for_references;
        this.eventHandler = sourceEventHandler;
        this.externalFeatures = externalFeatures;
        this.scan_comments_for_hints = configHandler.scan_comments_for_hints;
        this.isSourceDepCopyBook = isSourceDepCopyBook;

        this.copybookNestedInSection = configHandler.copybooks_nested;
        this.scan_comment_for_ls_control = configHandler.scan_comment_for_ls_control;
        this.copyBooksUsed = new Map<string, COBOLCopybookToken>();
        this.sections = new Map<string, COBOLToken>();
        this.paragraphs = new Map<string, COBOLToken>();
        this.constantsOrVariables = new Map<string, COBOLVariable[]>();
        this.callTargets = new Map<string, CallTargetInformation>();
        this.functionTargets = new Map<string, CallTargetInformation>();
        this.classes = new Map<string, COBOLToken>();
        this.methods = new Map<string, COBOLToken>();
        this.diagMissingFileWarnings = new Map<string, COBOLFileSymbol>();
        this.portWarnings = [];
        this.generalWarnings = [];
        this.commentReferences = [];
        this.parse4References = sourceHandler !== null;
        this.cache4PerformTargets = undefined;
        this.cache4ConstantsOrVars = undefined;
        this.scanAborted = false;
        this.languageId = this.sourceHandler.getLanguageId();
        this.COBOLKeywordDictionary = getCOBOLKeywordDictionary(this.languageId);
        switch (this.languageId.toLocaleLowerCase()) {
            case "cobol":
            case "bitlang-cobol":
                this.usePortationSourceScanner = configHandler.linter_port_helper;
                break;
            default:
                this.usePortationSourceScanner = false;
                break;
        }

        let sourceLooksLikeCOBOL = false;
        let prevToken: StreamTokens = StreamTokens.Blank;

        const hasCOBOLExtension = path.extname(filename).length > 0 ? true : false;
        this.sourceReferences = sourceReferences;

        this.sourceFileId = sourceReferences.filenames.length;
        sourceReferences.filenames.push(sourceHandler.getFilename());
        sourceReferences.filenameURIs.push(sourceHandler.getUriAsString());

        this.constantsOrVariables = sourceReferences.sharedConstantsOrVariables;
        this.paragraphs = sourceReferences.sharedParagraphs;
        this.sections = sourceReferences.sharedSections;
        this.tokensInOrder = sourceReferences.tokensInOrder;
        this.copyBooksUsed = sourceReferences.copyBooksUsed;
        this.execSQLDeclare = sourceReferences.execSQLDeclare;

        // set the source handler for the comment parsing
        sourceHandler.addCommentCallback(this);

        const state: ParseState = this.sourceReferences.state;

        /* mark this has been processed (to help copy of self) */
        state.copyBooksUsed.set(this.filename, undefined);
        if (this.sourceReferences.topLevel) {
            this.lastModifiedTime = externalFeatures.getFileModTimeStamp(this.filename);
        }

        this.workspaceFile = new COBOLWorkspaceFile(this.lastModifiedTime, sourceHandler.getShortWorkspaceFilename());

        // setup the event handler
        if (this.sourceReferences.topLevel) {
            this.eventHandler = sourceEventHandler;
            this.eventHandler.start(this);
        }

        const maxLineLength = configHandler.editor_maxTokenizationLineLength;

        if (this.sourceReferences.topLevel) {
            /* if we have an extension, then don't do a relaxed parse to determine if it is COBOL or not */
            const lineLimit = configHandler.pre_scan_line_limit;
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
                    line = sourceHandler.getLine(l, false);
                    if (line === undefined) {
                        break; // eof
                    }

                    line = line.trimEnd();

                    // ignore large lines
                    if (line.length > maxLineLength) {
                        this.externalFeatures.logMessage(`Aborted scanning ${this.filename} max line length exceeded`);
                        this.clearScanData();
                        continue;
                    }

                    // don't parse a empty line
                    if (line.length > 0) {
                        if (prevToken.endsWithDot === false) {
                            prevToken = this.relaxedParseLineByLine(prevToken, line, l, preParseState);
                        }
                        else {
                            prevToken = this.relaxedParseLineByLine(StreamTokens.Blank, line, l, preParseState);
                        }
                    } else {
                        maxLines++;     // increase the max lines, as this line is
                    }

                    if (preParseState.leaveEarly) {
                        break;
                    }

                }
                catch (e) {
                    this.externalFeatures.logException("COBOLScannner - Parse error : " + e, e as Error);
                }
            }

            // Do we have some sections?
            if (preParseState.sectionsInToken === 0 && preParseState.divisionsInToken === 0) {
                /* if we have items that could be in a data division */

                if (preParseState.procedureDivisionRelatedTokens !== 0 && preParseState.procedureDivisionRelatedTokens > preParseState.workingStorageRelatedTokens) {
                    this.ImplicitProgramId = "";

                    const fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Procedure Division", 0, "Procedure", "Procedure Division (CopyBook)", state.currentDivision, "", false);
                    state.currentSection = undefined;
                    state.currentDivision = fakeDivision;
                    state.procedureDivision = fakeDivision;
                    state.pickFields = false;
                    state.inProcedureDivision = true;
                    sourceLooksLikeCOBOL = true;
                    fakeDivision.ignoreInOutlineView = true;
                    this.sourceIsCopybook = true;
                    state.endsWithDot = true;
                    state.prevEndsWithDot = true;
                }
                else if ((preParseState.workingStorageRelatedTokens !== 0 && preParseState.numberTokensInHeader !== 0)) {
                    const fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Data Division", 0, "Data", "Data Division (CopyBook)", state.currentDivision, "", false);
                    state.currentSection = undefined;
                    state.currentParagraph = undefined;
                    state.currentDivision = fakeDivision;
                    state.pickFields = true;
                    state.inProcedureDivision = false;
                    sourceLooksLikeCOBOL = true;
                    this.ImplicitProgramId = "";
                    fakeDivision.ignoreInOutlineView = true;
                    this.sourceIsCopybook = true;
                }
            }

            //any divs or left early?
            if (preParseState.divisionsInToken !== 0 || preParseState.leaveEarly) {
                sourceLooksLikeCOBOL = true;
            }

            // could it be COBOL (just by the comment area?)
            if (!sourceLooksLikeCOBOL && sourceHandler.getCommentCount() > 0) {
                sourceLooksLikeCOBOL = true;
            }

            /* if the source has an extension, then continue on.. */
            if (hasCOBOLExtension) {
                sourceLooksLikeCOBOL = true;
            }

            /* leave early */
            if (sourceLooksLikeCOBOL === false) {
                if (filename.length > 0) {
                    const linesinSource = sourceHandler.getLineCount();
                    if (linesinSource > maxLines) {
                        this.externalFeatures.logMessage(` Warning - Unable to determine if ${filename} is COBOL after scanning ${maxLines} lines (configurable via ${ExtensionDefaults.defaultEditorConfig}.pre_scan_line_limit setting)`);
                    } else {
                        // if the number of lines is low, don't comment on it
                        if (linesinSource > 5) {
                            this.externalFeatures.logMessage(` Unable to determine if ${filename} is COBOL and how it is used`);
                        }
                    }
                }
            }

            // drop out early
            if (!sourceLooksLikeCOBOL) {
                return;
            }

        } else {
            sourceLooksLikeCOBOL = true;
        }
        sourceHandler.resetCommentCount();

        this.sourceFormat = SourceFormat.get(sourceHandler, configHandler);
        switch (this.sourceFormat) {
            case ESourceFormat.free: sourceHandler.setDumpAreaBOnwards(false);
                break;
            case ESourceFormat.variable: sourceHandler.setDumpAreaBOnwards(false);
                break;
            case ESourceFormat.fixed:
                sourceHandler.setDumpAreaA(true);
                sourceHandler.setDumpAreaBOnwards(true);
                break;
            case ESourceFormat.terminal:
                sourceHandler.setSourceFormat(this.sourceFormat);
                break;
        }

        prevToken = StreamTokens.Blank;
        sourceHandler.resetCommentCount();

        let sourceTimeout = externalFeatures.getSourceTimeout(this.configHandler);
        if (externalFeatures)
            for (let l = 0; l < sourceHandler.getLineCount(); l++) {
                try {
                    state.currentLineIsComment = false;
                    const processedLine: string | undefined = sourceHandler.getLine(l, false);

                    // eof
                    if (processedLine === undefined) {
                        break;
                    }

                    // don't process line
                    if (processedLine.length === 0 && state.currentLineIsComment) {
                        continue;
                    }

                    const line = processedLine.trimEnd();

                    // don't parse a empty line
                    if (line.length > 0) {
                        const prevTokenToParse = prevToken.endsWithDot === false ? prevToken : StreamTokens.Blank;
                        prevToken = this.parseLineByLine(l, prevTokenToParse, line);
                    }

                    // if we are not debugging..
                    if (externalFeatures.isDebuggerActive() === false) {
                        // check for timeout every 1000 lines
                        if (l % 1000 !== 0) {
                            const elapsedTime = externalFeatures.performance_now() - this.sourceReferences.startTime;
                            if (elapsedTime > sourceTimeout) {
                                this.externalFeatures.logMessage(`Aborted scanning ${this.filename} after ${elapsedTime}`);
                                this.clearScanData();
                                return;
                            }
                        }
                    }

                    // only do this for "COBOL" language
                    if (this.usePortationSourceScanner) {
                        const portResult = this.sourcePorter.isDirectiveChangeRequired(this.filename, l, line);
                        if (portResult !== undefined) {
                            this.portWarnings.push(portResult);
                        }
                    }
                }
                catch (e) {
                    this.externalFeatures.logException("COBOLScannner - Parse error", e as Error);
                }
            }

        if (this.sourceReferences.topLevel) {
            const lastLineCount = sourceHandler.getLineCount();
            const lastLineLengthU = sourceHandler.getLine(lastLineCount, true);
            const lastLineLength = lastLineLengthU === undefined ? 0 : lastLineLengthU.length;
            if (state.programs.length !== 0) {
                for (let cp = 0; cp < state.programs.length; cp++) {
                    const currentProgram = state.programs.pop();
                    if (currentProgram !== undefined) {
                        currentProgram.rangeEndLine = sourceHandler.getLineCount();
                        currentProgram.rangeEndColumn = lastLineLength;
                    }
                }
            }

            if (state.currentDivision !== undefined) {
                state.currentDivision.rangeEndLine = sourceHandler.getLineCount();
                state.currentDivision.rangeEndColumn = lastLineLength;
            }

            if (state.currentSection !== undefined) {
                state.currentSection.rangeEndLine = sourceHandler.getLineCount();
                state.currentSection.rangeEndColumn = lastLineLength;
            }

            if (state.currentParagraph !== undefined) {
                state.currentParagraph.rangeEndLine = sourceHandler.getLineCount();
                state.currentParagraph.rangeEndColumn = lastLineLength;
            }

            if (this.ImplicitProgramId.length !== 0) {
                const ctoken = this.newCOBOLToken(COBOLTokenStyle.ImplicitProgramId, 0, "", 0, this.ImplicitProgramId, this.ImplicitProgramId, undefined, "", false);
                ctoken.rangeEndLine = sourceHandler.getLineCount();
                ctoken.rangeStartLine = 0;
                ctoken.rangeEndColumn = lastLineLength;
                ctoken.ignoreInOutlineView = true;
                state.currentProgramTarget.Token = ctoken;
                this.tokensInOrder.pop();

                this.callTargets.set(this.ImplicitProgramId, state.currentProgramTarget);
            }

            for (const [sql_declare_name, sql_declare] of this.execSQLDeclare) {
                for (const refExecToken of this.execTokensInOrder) {
                    const fileid = this.sourceReferences.getSourceFieldId(refExecToken.filename);
                    const text = refExecToken.sourceHandler.getText(refExecToken.rangeStartLine, refExecToken.rangeStartColumn, refExecToken.rangeEndLine, refExecToken.rangeEndColumn);
                    this.parseSQLDeclareForReferences(fileid, sql_declare_name, refExecToken, text, sql_declare);
                }
            }

            // setup references for unknown forward reference-items
            const unknown = [];
            for (const [strRef, sourceRefs] of this.sourceReferences.unknownReferences) {
                const possibleTokens = this.constantsOrVariables.get(strRef);
                if (possibleTokens !== undefined) {
                    let ttype: COBOLTokenStyle = COBOLTokenStyle.Variable;
                    let addReference = true;
                    for (const token of possibleTokens) {
                        if (token.ignoreInOutlineView == false || token.token.isTokenFromSourceDependancyCopyBook) {
                            ttype = (token.tokenType === COBOLTokenStyle.Unknown) ? ttype : token.tokenType;
                        } else {
                            addReference = false;
                        }
                    }
                    if (addReference) {
                        this.transferReference(strRef, sourceRefs, this.sourceReferences.constantsOrVariablesReferences, ttype);
                    }
                }
                else if (this.isVisibleSection(strRef)) {
                    this.transferReference(strRef, sourceRefs, this.sourceReferences.targetReferences, COBOLTokenStyle.Section);
                } else if (this.isVisibleParagraph(strRef)) {
                    this.transferReference(strRef, sourceRefs, this.sourceReferences.targetReferences, COBOLTokenStyle.Paragraph);
                } else {
                    unknown.push(strRef);
                }
            }

            if (this.sourceReferences.state.skipToEndLsIgnore) {
                if (this.lastCOBOLLS !== undefined) {
                    const diagMessage = `Missing ${configHandler.scan_comment_end_ls_ignore}`;
                    this.generalWarnings.push(new COBOLFileSymbol(this.filename, this.lastCOBOLLS.startLine, diagMessage));
                    while (this.sourceReferences.ignoreLSRanges.pop() !== undefined);
                }
            }
            this.sourceReferences.unknownReferences.clear();
            this.eventHandler.finish();
        }
    }


    private clearScanData() {
        this.tokensInOrder = [];
        this.copyBooksUsed.clear();
        this.sections.clear();
        this.paragraphs.clear();
        this.constantsOrVariables.clear();
        this.callTargets.clear();
        this.functionTargets.clear();
        this.classes.clear();
        this.methods.clear();
        this.diagMissingFileWarnings.clear();
        this.cache4PerformTargets = undefined;
        this.cache4ConstantsOrVars = undefined;
        this.scanAborted = true;
        this.sourceReferences.reset(this.configHandler);
    }

    private isVisibleSection(sectionName: string): boolean {
        const foundSectionToken = this.sections.get(sectionName);
        if (foundSectionToken === undefined) {
            return false;
        }

        if (foundSectionToken.isTokenFromSourceDependancyCopyBook) {
            return true;
        }

        return !foundSectionToken.ignoreInOutlineView;
    }

    private isVisibleParagraph(paragrapName: string): boolean {
        const foundParagraph = this.paragraphs.get(paragrapName);
        if (foundParagraph === undefined) {
            return false;
        }

        if (foundParagraph.isTokenFromSourceDependancyCopyBook) {
            return true;
        }

        return !foundParagraph.ignoreInOutlineView;
    }

    private transferReference(symbol: string, symbolRefs: SourceReference_Via_Length[], transferReferenceMap: Map<string, SourceReference_Via_Length[]>, tokenStyle: COBOLTokenStyle): void {
        if (symbol.length === 0) {
            return;
        }

        if (this.isValidKeyword(symbol)) {
            return;
        }

        const refList = transferReferenceMap.get(symbol);
        if (refList !== undefined) {
            for (const sourceRef of symbolRefs) {
                sourceRef.tokenStyle = tokenStyle;
                refList.push(sourceRef);
            }
        } else {
            for (const sourceRef of symbolRefs) {
                sourceRef.tokenStyle = tokenStyle;
            }
            transferReferenceMap.set(symbol, symbolRefs);
        }
    }

    // public getCurrentDivision(): string {
    //     return this.sourceReferences.state.currentDivision === undefined ? "" : this.sourceReferences.state.currentDivision.tokenName;
    // }

    // public getCurrentSection(): npm list --production --parseable --depth=99999 --loglevel=errorstring {
    //     return this.sourceReferences.state.currentSection === undefined ? "" : this.sourceReferences.state.currentSection.tokenName;
    // }

    private tokensInOrderPush(token: COBOLToken, sendEvent: boolean): void {
        this.tokensInOrder.push(token);
        if (sendEvent) {
            this.eventHandler.processToken(token);
        }
    }

    private newCOBOLToken(tokenType: COBOLTokenStyle, startLine: number, _line: string, currentCol: number, token: string,
        description: string, parentToken: COBOLToken | undefined,
        extraInformation1:string, isImplicitToken: boolean): COBOLToken {

        const state: ParseState = this.sourceReferences.state;
        let startColumn = _line.indexOf(token, currentCol);
        if (startColumn === -1) {
            startColumn = _line.indexOf(token);
            if (startColumn === -1) {
                if (currentCol <= _line.length) {
                    startColumn = currentCol;
                } else {
                    startColumn = 0;
                }
            }
        }
        const ctoken = new COBOLToken(
            this.sourceHandler,
            this.sourceHandler.getUriAsString(), this.filename, tokenType, startLine, startColumn, token, description, parentToken, state.inProcedureDivision, extraInformation1,
            this.isSourceDepCopyBook,
            isImplicitToken);
        ctoken.ignoreInOutlineView = state.ignoreInOutlineView;
        ctoken.inSection = this.sourceReferences.state.currentSection;

        if (ctoken.ignoreInOutlineView || tokenType === COBOLTokenStyle.ImplicitProgramId) {
            this.tokensInOrderPush(ctoken, true);
            return ctoken;
        }

        /* if we are in a paragraph update */
        if (state.currentParagraph !== undefined) {
            state.currentParagraph.rangeEndLine = startLine;
            if (ctoken.startColumn !== 0) {
                state.currentParagraph.rangeEndColumn = ctoken.rangeStartColumn - 1;
            }
        }

        // new division
        if (tokenType === COBOLTokenStyle.Division && state.currentDivision !== undefined) {

            state.currentParagraph = undefined;
            state.currentDivision.rangeEndLine = startLine;
            // state.currentDivision.endLine = parentToken !== undefined ? parentToken.endLine : startLine;
            if (ctoken.rangeStartColumn !== 0) {
                state.currentDivision.rangeEndColumn = ctoken.rangeStartColumn - 1;
            }

            if (state.currentSection !== undefined) {
                state.currentSection.rangeEndLine = startLine;
                if (ctoken.rangeStartColumn !== 0) {
                    state.currentSection.rangeEndColumn = ctoken.rangeStartColumn - 1;
                }
                state.currentSection = undefined;
                state.currentParagraph = undefined;
            }
            this.tokensInOrder.push(ctoken);
            this.eventHandler.processToken(ctoken);

            return ctoken;
        }

        // new section
        if (tokenType === COBOLTokenStyle.Section) {
            if (state.currentSection !== undefined) {
                state.currentParagraph = undefined;
                state.currentSection.rangeEndLine = startLine;
                if (ctoken.rangeStartColumn !== 0) {
                    state.currentSection.rangeEndColumn = ctoken.rangeStartColumn - 1;
                }
            }
            this.tokensInOrderPush(ctoken, state.inProcedureDivision);

            return ctoken;
        }

        // new paragraph
        if (tokenType === COBOLTokenStyle.Paragraph) {
            if (state.currentSection !== undefined) {
                state.currentSection.rangeEndLine = startLine;
                if (ctoken.rangeStartColumn !== 0) {
                    state.currentSection.rangeEndColumn = ctoken.rangeStartColumn - 1;
                }
            }

            state.currentParagraph = ctoken;

            if (state.currentDivision !== undefined) {
                state.currentDivision.rangeEndLine = startLine;
                if (ctoken.rangeStartColumn !== 0) {
                    state.currentDivision.rangeEndColumn = ctoken.rangeStartColumn - 1;
                }
            }
            this.tokensInOrderPush(ctoken, true);
            return ctoken;
        }

        // new ignore 
        if (tokenType === COBOLTokenStyle.IgnoreLS) {
            this.tokensInOrderPush(ctoken, false);
            return ctoken;
        }

        this.tokensInOrderPush(ctoken, true);

        return ctoken;
    }


    // private static readonly literalRegexOld = /^[#a-zA-Z0-9][a-zA-Z0-9-_]*$/g;
    private static readonly literalRegex = /^([a-zA-Z-0-9_]*[a-zA-Z0-9]|([#]?[0-9a-zA-Z]+[a-zA-Z-0-9_]*[a-zA-Z0-9]))$/g;

    public static isValidLiteral(id: string): boolean {

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


    private isValidParagraphName(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(COBOLSourceScanner.paragraphRegex) === null) {
            return false;
        }

        return true;
    }

    private isValidKeyword(keyword: string): boolean {
        return this.COBOLKeywordDictionary.has(keyword);
    }

    private isValidProcedureKeyword(keyword: string): boolean {
        return cobolProcedureKeywordDictionary.has(keyword);
    }

    private isValidStorageKeyword(keyword: string): boolean {
        return cobolStorageKeywordDictionary.has(keyword);
    }

    private isNumber(value: string): boolean {
        try {
            if (value.toString().length === 0) {
                return false;
            }
            return !isNaN(Number(value.toString()));
        }
        catch (e) {
            this.externalFeatures.logException("isNumber(" + value + ")", e as Error);
            return false;
        }
    }

    private containsIndex(literal: string): boolean {
        for (let pos = 0; pos < literal.length; pos++) {
            if (literal[pos] === "(" || literal[pos] === ")" &&
                literal[pos] === "[" || literal[pos] === "]") {
                return true;
            }
        }
        return false;
    }

    private trimVariableToMap(literal: string): Map<number, string> {
        const varMap = new Map<number, string>();
        let v = "";
        let startPos = 0;
        for (let pos = 0; pos < literal.length; pos++) {
            switch (literal[pos]) {
                case "(": if (v.length !== 0) {
                    varMap.set(startPos, v);
                    startPos = 1 + pos;
                    v = "";
                }
                    break;
                case "[": if (v.length !== 0) {
                    varMap.set(startPos, v);
                    startPos = 1 + pos;
                    v = "";
                }
                    break;
                case ")": if (v.length !== 0) {
                    varMap.set(startPos, v);
                    startPos = 1 + pos;
                    v = "";
                }
                    break;
                case "]": if (v.length !== 0) {
                    varMap.set(startPos, v);
                    startPos = 1 + pos;
                    v = "";
                }
                    break;
                default:
                    v += literal[pos];
            }
        }

        if (v.length !== 0) {
            varMap.set(startPos, v);
        }
        return varMap;
    }

    public static trimLiteral(literal: string, trimQuotes: boolean): string {
        let literalTrimmed = literal.trim();

        if (literalTrimmed.length === 0) {
            return literalTrimmed;
        }

        /* remove ( */
        if (literalTrimmed[0] === "(") {
            literalTrimmed = literalTrimmed.substring(1, literalTrimmed.length);
        }

        /* remove  */
        if (literalTrimmed.endsWith(")")) {
            literalTrimmed = literalTrimmed.substring(0, literalTrimmed.length - 1);
        }

        literalTrimmed = literalTrimmed.trim();
        if (literalTrimmed.length === 0) {
            return literalTrimmed;
        }

        /* remove end . */
        if (literalTrimmed.endsWith(".")) {
            literalTrimmed = literalTrimmed.substring(0, literalTrimmed.length - 1);
        }

        if (trimQuotes) {
            /* remove quotes */
            if (literalTrimmed[0] === "\"" && literalTrimmed.endsWith("\"")) {
                return literalTrimmed.substring(1, literalTrimmed.length - 1);
            }
            /* remove quotes */
            if (literalTrimmed[0] === "'" && literalTrimmed.endsWith("'")) {
                return literalTrimmed.substring(1, literalTrimmed.length - 1);
            }
        }

        return literalTrimmed;
    }

    private isQuotedLiteral(literal: string): boolean {
        let literalTrimmed = literal.trim();

        if (literalTrimmed.length === 0) {
            return false;
        }

        /* remove end . */
        let lastChar = literalTrimmed[literalTrimmed.length - 1];
        if (lastChar === ".") {
            literalTrimmed = literalTrimmed.substring(0, literalTrimmed.length - 1);
        }

        // too small
        if (literalTrimmed.length < 2) {
            return false;
        }

        lastChar = literalTrimmed[literalTrimmed.length - 1];
        if (literalTrimmed[0] === "\"" && lastChar === "\"") {
            return true;
        }

        /* remove quotes */
        if (literalTrimmed[0] === "'" && lastChar === "'") {
            return true;
        }

        return false;
    }

    private relaxedParseLineByLine(prevToken: StreamTokens, line: string, lineNumber: number, state: PreParseState): StreamTokens {
        const token = new StreamTokens(line, lineNumber, prevToken);
        let tokenCountPerLine = 0;
        do {
            try {
                const endsWithDot = token.endsWithDot;
                let current: string = token.currentToken;
                let currentLower: string = token.currentTokenLower;
                tokenCountPerLine++;

                if (endsWithDot) {
                    current = current.substring(0, current.length - 1);
                    currentLower = currentLower.substring(0, currentLower.length - 1);
                }

                if (tokenCountPerLine === 1) {
                    const tokenAsNumber = Number.parseInt(current, 10);
                    if (tokenAsNumber !== undefined && (!isNaN(tokenAsNumber))) {
                        state.numberTokensInHeader++;
                        continue;
                    }
                }
                switch (currentLower) {
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
                                case "input-output": state.sectionsInToken++;
                                    state.leaveEarly = true;
                                    break;
                                default:
                                    if (this.isValidProcedureKeyword(token.prevTokenLower) === false) {
                                        state.procedureDivisionRelatedTokens++;
                                    }
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
                            case "procedure":
                                state.procedureDivisionRelatedTokens++;
                                state.leaveEarly = true;
                                break;
                        }
                        break;
                    default:
                        if (this.isValidProcedureKeyword(currentLower)) {
                            state.procedureDivisionRelatedTokens++;
                        }

                        if (this.isValidStorageKeyword(currentLower)) {
                            state.workingStorageRelatedTokens++;
                        }

                        break;
                }
                // continue now
                if (current.length === 0) {
                    continue;
                }
            }
            catch (e) {
                this.externalFeatures.logException("COBOLScannner relaxedParseLineByLine line error: ", e as Error);
            }
        }
        while (token.moveToNextToken() === false);

        return token;
    }

    private addVariableReference(referencesMap: Map<string, SourceReference_Via_Length[]>, lowerCaseVariable: string, line: number, column: number, tokenStyle: COBOLTokenStyle): boolean {
        const l = lowerCaseVariable.length;
        if (l === 0) {
            return false;
        }

        if (this.isValidKeyword(lowerCaseVariable)) {
            return false;
        }

        if (COBOLSourceScanner.isValidLiteral(lowerCaseVariable) === false) {
            return false;
        }

        const lowerCaseVariableRefs = referencesMap.get(lowerCaseVariable);
        if (lowerCaseVariableRefs !== undefined) {
            let duplicateFound = false;
            for (const lowerCaseVariableRef of lowerCaseVariableRefs) {
                if (lowerCaseVariableRef.line === line && lowerCaseVariableRef.column === column && lowerCaseVariableRef.length === l) {
                    duplicateFound = true;
                }
            }
            if (duplicateFound === false) {
                lowerCaseVariableRefs.push(new SourceReference_Via_Length(this.sourceFileId, line, column, l, tokenStyle, this.isSourceDepCopyBook, lowerCaseVariable, ""));
            }
            return true;
        }

        const sourceRefs: SourceReference_Via_Length[] = [];
        sourceRefs.push(new SourceReference_Via_Length(this.sourceFileId, line, column, l, tokenStyle, this.isSourceDepCopyBook, lowerCaseVariable, ""));
        referencesMap.set(lowerCaseVariable, sourceRefs);
        return true;
    }

    private addTargetReference(referencesMap: Map<string, SourceReference_Via_Length[]>, _targetReference: string, line: number, column: number, tokenStyle: COBOLTokenStyle, reason:string): boolean {

        const l = _targetReference.length;
        if (l === 0) {
            return false;
        }

        const targetReference = _targetReference.toLowerCase();
        if (this.isValidKeyword(targetReference)) {
            return false;
        }

        if (COBOLSourceScanner.isValidLiteral(targetReference) === false) {
            return false;
        }

        const srl = new SourceReference_Via_Length(this.sourceFileId, line, column, l, tokenStyle, this.isSourceDepCopyBook, _targetReference, reason);
        const state = this.sourceReferences.state;
        let inSectionOrParaToken = state.currentParagraph !== undefined ? state.currentParagraph : state.currentSection;

        // do we have a reference map for the target?
        let lowerCaseTargetRefs = referencesMap.get(targetReference);
        if (lowerCaseTargetRefs === undefined) {
            lowerCaseTargetRefs = []
            referencesMap.set(targetReference, lowerCaseTargetRefs);
        }

        if (inSectionOrParaToken !== undefined && inSectionOrParaToken.inProcedureDivision) {
            let duplicateFound = false;
            for (const lowerCaseVariableRef of lowerCaseTargetRefs) {
                if (lowerCaseVariableRef.line === line && lowerCaseVariableRef.column === column && lowerCaseVariableRef.length === l) {
                    duplicateFound = true;
                }
            }
            if (duplicateFound === false) {
                lowerCaseTargetRefs.push(srl);
                let csr = state.currentSectionOutRefs.get(inSectionOrParaToken.tokenNameLower);
                if (csr === undefined) {
                    csr = [];
                    state.currentSectionOutRefs.set(inSectionOrParaToken.tokenName.toLowerCase(), csr);
                }
                csr.push(srl);
            }
            return true;
        }

        lowerCaseTargetRefs.push(srl);
        return true;
    }

    private addVariableOrConstant(lowerCaseVariable: string, cobolToken: COBOLToken) {
        if (lowerCaseVariable.length === 0) {
            return;
        }

        if (this.isValidKeyword(lowerCaseVariable)) {
            return;
        }

        if (this.addVariableReference(this.sourceReferences.constantsOrVariablesReferences, lowerCaseVariable, cobolToken.startLine, cobolToken.startColumn, cobolToken.tokenType) == false) {
            return;
        }

        const constantsOrVariablesToken = this.constantsOrVariables.get(lowerCaseVariable);
        if (constantsOrVariablesToken !== undefined) {
            constantsOrVariablesToken.push(new COBOLVariable(cobolToken));
            return;
        }

        const tokens: COBOLVariable[] = [];
        tokens.push(new COBOLVariable(cobolToken));
        this.constantsOrVariables.set(lowerCaseVariable, tokens);
    }

    private cleanupReplaceToken(token: string, rstate: IReplaceState): string {
        if (token.endsWith(",")) {
            token = token.substring(0, token.length - 1);
        }

        if (token.startsWith("==") && token.endsWith("==")) {
            rstate.isPseudoTextDelimiter = true;
            return token.substring(2, token.length - 2);
        }

        return token;
    }

    private parseLineByLine(lineNumber: number, prevToken: StreamTokens, line: string): StreamTokens {
        const token = new StreamTokens(line, lineNumber, prevToken);

        const state = this.sourceReferences.state;
        let stream = this.processToken(lineNumber, token, line, state.replaceMap.size !== 0);

        // update current group to always include the end of current line
        if (state.current01Group !== undefined && line.length !== 0) {
            state.current01Group.rangeEndLine = lineNumber;
            state.current01Group.rangeEndColumn = line.length;
        }

        return stream;
    }

    private processToken(lineNumber: number, token: StreamTokens, line: string, replaceOn: boolean): StreamTokens {
        const state: ParseState = this.sourceReferences.state;

        do {
            try {
                // console.log(`DEBUG: ${line}`);
                let _tcurrent: string = token.currentToken;
                // continue now
                if (_tcurrent.length === 0) {
                    continue;
                }

                // if skip to end lsp
                if (state.skipToEndLsIgnore) {
                    // add skipped token into sourceref range to colour as a comment
                    if (this.configHandler.enable_semantic_token_provider) {
                        const sr = new SourceReference(this.sourceFileId, token.currentLineNumber, token.currentCol, token.currentLineNumber, token.currentCol + token.currentToken.length, COBOLTokenStyle.IgnoreLS);
                        this.sourceReferences.ignoreLSRanges.push(sr);
                    }

                    continue;
                }

                // skip one token
                if (state.skipNextToken) {
                    state.skipNextToken = false;

                    // time to leave?  
                    //  [handles "pic x.", where x. is the token to be skipped]
                    if (state.skipToDot && _tcurrent.endsWith(".")) {
                        state.skipToDot = false;
                        // drop through, so all other flags can be reset or contune past
                    } else {
                        continue;
                    }
                }

                // fakeup a replace algorithmf
                if (replaceOn) {
                    let rightLine = line.substring(token.currentCol);
                    const rightLineOrg = line.substring(token.currentCol);
                    for (const [k, r] of state.replaceMap) {
                        rightLine = rightLine.replace(r.rex4wordreplace, k);
                    }

                    if (rightLine !== rightLineOrg) {
                        try {
                            const leftLine = line.substring(0, token.currentCol);

                            this.sourceHandler.setUpdatedLine(lineNumber, leftLine + rightLine);
                            const lastTokenId = this.tokensInOrder.length;
                            const newToken = new StreamTokens(rightLine as string, lineNumber, new StreamTokens(token.prevToken, token.prevTokenLineNumber, undefined));
                            const retToken = this.processToken(lineNumber, newToken, rightLine, false);

                            // ensure any new token match the original soure
                            if (lastTokenId !== this.tokensInOrder.length) {
                                for (let ltid = lastTokenId; ltid < this.tokensInOrder.length; ltid++) {
                                    const addedToken = this.tokensInOrder[ltid];
                                    addedToken.rangeStartColumn = token.currentCol;
                                    addedToken.rangeEndColumn = token.currentCol + _tcurrent.length;
                                }
                            }
                            return retToken;
                        } catch (e) {
                            this.externalFeatures.logException("replace", e as Error);
                        }
                    }
                }


                // HACK for "set x to entry"
                if (token.prevTokenLower === "to" && token.currentTokenLower === "entry") {
                    token.moveToNextToken();
                    continue;
                }

                if (_tcurrent.endsWith(",")) {
                    _tcurrent = _tcurrent.substring(0, _tcurrent.length - 1);
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = false;
                } else if (token.endsWithDot) {
                    _tcurrent = _tcurrent.substring(0, _tcurrent.length - 1);
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = true;
                } else {
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = false;
                }

                const currentCol = token.currentCol;
                const current: string = _tcurrent;
                const currentLower: string = _tcurrent.toLowerCase();

                // if pickUpUsing
                if (state.pickUpUsing) {
                    if (state.endsWithDot) {
                        state.pickUpUsing = false;
                    }
                    if (token.prevTokenLower !== "type") {
                        continue;
                    }

                    switch (currentLower) {
                        case "signed":
                            break;
                        case "unsigned":
                            break;
                        case "as":
                            break;
                        case "type":
                            break;
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
                        case "output":
                            state.using = UsingState.BY_OUTPUT;
                            break;
                        case "returning":
                            state.using = UsingState.RETURNING;
                            break;
                        default:
                            // of are after a returning statement with a "." then parsing must end now
                            if (state.using === UsingState.UNKNOWN) {
                                state.pickUpUsing = false
                                break;
                            }

                            if (this.sourceReferences !== undefined) {
                                if (currentLower === "any") {
                                    state.parameters.push(new COBOLParameter(state.using, current));
                                } else if (currentLower.length > 0 && this.isValidKeyword(currentLower) === false && this.isNumber(currentLower) === false) {
                                    // no forward validation can be done, as this is a one pass scanner
                                    if (this.addVariableReference(this.sourceReferences.unknownReferences, currentLower, lineNumber, token.currentCol, COBOLTokenStyle.Variable)) {
                                        state.parameters.push(new COBOLParameter(state.using, current));
                                    }
                                }
                            }

                            if (state.using === UsingState.RETURNING) {
                                state.using = UsingState.UNKNOWN;
                            }

                            // if entry or procedure division does not have "." then parsing must end now
                            if (currentLower !== "any" && this.isValidKeyword(currentLower)) {
                                state.currentProgramTarget.CallParameters = state.parameters;
                                state.using = UsingState.UNKNOWN;
                                state.pickUpUsing = false
                                const diagMessage = `Unexpected keyword '${current}' when scanning USING parameters`;
                                let nearestLine = state.currentProgramTarget.Token !== undefined ? state.currentProgramTarget.Token.startLine : lineNumber;
                                this.generalWarnings.push(new COBOLFileSymbol(this.filename, nearestLine, diagMessage));
                                break;
                            }


                        // logMessage(`INFO: using parameter : ${tcurrent}`);
                    }
                    if (state.endsWithDot) {
                        state.currentProgramTarget.CallParameters = state.parameters;
                        state.pickUpUsing = false;
                    }

                    continue;
                }


                // if skipToDot and not the end of the statement.. swallow
                if (state.skipToDot) {
                    const trimTokenLower = COBOLSourceScanner.trimLiteral(currentLower, false);
                    const tokenIsKeyword = this.isValidKeyword(trimTokenLower);

                    if (state.addReferencesDuringSkipToTag) {
                        if (this.sourceReferences !== undefined) {
                            if (COBOLSourceScanner.isValidLiteral(trimTokenLower) && !this.isNumber(trimTokenLower) && tokenIsKeyword === false) {
                                // no forward validation can be done, as this is a one pass scanner
                                if (token.prevTokenLower !== "pic" && token.prevTokenLower !== "picture") {
                                    this.addVariableReference(this.sourceReferences.unknownReferences, trimTokenLower, lineNumber, token.currentCol, COBOLTokenStyle.Unknown);
                                }
                            }
                        }
                    }

                    if (trimTokenLower === "value") {
                        state.inValueClause = true;
                        state.addVariableDuringStipToTag = false;
                        continue;
                    }

                    // turn off at keyword
                    if (trimTokenLower === "pic" || trimTokenLower === "picture") {
                        state.skipNextToken = true;
                        state.addVariableDuringStipToTag = false;
                        continue;
                    }

                    // if we are in a to.. or indexed
                    if (token.prevTokenLower === "to") {
                        state.addVariableDuringStipToTag = false;
                        continue;
                    }

                    if (token.prevTokenLower === "indexed" && token.currentTokenLower === "by") {
                        state.addVariableDuringStipToTag = true;
                    }

                    if (token.prevTokenLower === "depending" && token.currentTokenLower === "on") {
                        state.addVariableDuringStipToTag = false;
                    }

                    if (state.addVariableDuringStipToTag && tokenIsKeyword === false && state.inValueClause === false) {
                        if (COBOLSourceScanner.isValidLiteral(trimTokenLower) && !this.isNumber(trimTokenLower)) {
                            const trimToken = COBOLSourceScanner.trimLiteral(current, false);
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, currentCol, trimToken, trimToken, state.currentDivision, token.prevToken, false);
                            this.addVariableOrConstant(trimTokenLower, variableToken);
                        }
                    }

                    if (state.inReplace) {
                        switch (currentLower) {
                            case "by":
                                state.captureReplaceLeft = false;
                                break;
                            case "off":
                                state.skipToDot = false;
                                state.inReplace = false;
                                state.replaceMap.clear();
                                break;
                            default:
                                if (state.captureReplaceLeft) {
                                    if (state.replaceLeft.length !== 0) {
                                        state.replaceLeft += " ";
                                    }
                                    state.replaceLeft += current;
                                } else {
                                    if (state.replaceRight.length !== 0) {
                                        state.replaceRight += " ";
                                    }
                                    state.replaceRight += current;
                                }
                                if (!state.captureReplaceLeft && current.endsWith("==")) {
                                    state.replaceMap.set(this.cleanupReplaceToken("" + state.replaceRight, state.replace_state),
                                        new replaceToken(this.cleanupReplaceToken("" + state.replaceLeft, state.replace_state), state.replace_state));
                                    state.replaceLeft = state.replaceRight = "";
                                    state.captureReplaceLeft = true;
                                }
                                break;
                        }

                    }

                    if (state.inCopy) {
                        const cbState = state.copybook_state;
                        switch (currentLower) {
                            case "":
                                break;
                            case "suppress":
                            case "resource":
                            case "indexed":
                                break;
                            case "leading":
                                cbState.isLeading = true;
                                break;
                            case "trailing":
                                cbState.isTrailing = true;
                                break;
                            case "of": cbState.isOf = true;
                                break;
                            case "in": cbState.isIn = true;
                                break;
                            case "replacing":
                                cbState.isReplacingBy = false;
                                cbState.isReplacing = true;
                                cbState.isLeading = false;
                                cbState.isTrailing = false;
                                break;
                            case "by":
                                cbState.isReplacingBy = true;
                                break;
                            default: {
                                if (cbState.isIn && cbState.literal2.length === 0) {
                                    cbState.literal2 = current;
                                    break;
                                }
                                if (cbState.isOf && cbState.library_name.length === 0) {
                                    cbState.library_name = current;
                                    break;
                                }
                                if (cbState.isReplacing && cbState.replaceLeft.length === 0) {
                                    cbState.replaceLeft = current;
                                    break;
                                }
                                if (cbState.isReplacingBy) {
                                    if (this.configHandler.enable_text_replacement) {
                                        cbState.copyReplaceMap.set(
                                            this.cleanupReplaceToken("" + current, cbState),
                                            new replaceToken(this.cleanupReplaceToken("" + cbState.replaceLeft, cbState), cbState));
                                    }
                                    cbState.isReplacingBy = false;
                                    cbState.isReplacing = true;
                                    cbState.isLeading = false;
                                    cbState.isTrailing = false;
                                    cbState.replaceLeft = "";
                                    break;
                                }
                                if (currentLower.length > 0 && !cbState.isOf && !cbState.isIn && !cbState.isReplacing) {
                                    cbState.copyBook = current;
                                    cbState.trimmedCopyBook = COBOLSourceScanner.trimLiteral(current, true);
                                    cbState.startLineNumber = lineNumber;
                                    cbState.startCol = state.inCopyStartColumn; // stored when 'copy' is seen
                                    cbState.line = line;
                                    break;
                                }
                            }
                        }
                    }

                    //reset and process anything if necessary
                    if (state.endsWithDot === true) {
                        state.inReplace = false;
                        state.skipToDot = false;
                        state.inValueClause = false;
                        state.addReferencesDuringSkipToTag = false;

                        if (state.inCopy) {
                            state.copybook_state.endLineNumber = lineNumber;
                            state.copybook_state.endCol = token.currentCol + token.currentToken.length;
                            this.processCopyBook(state.copybook_state);
                        }
                        state.inCopy = false;
                    }
                    continue;
                }


                const prevToken = COBOLSourceScanner.trimLiteral(token.prevToken, false);
                const prevTokenLowerUntrimmed = token.prevTokenLower.trim();
                const prevTokenLower = COBOLSourceScanner.trimLiteral(prevTokenLowerUntrimmed, false);
                const prevCurrentCol = token.prevCurrentCol;

                const prevPlusCurrent = token.prevToken + " " + current;

                if (currentLower === "exec") {
                    this.currentExec = token.nextSTokenIndex(1).currentToken;
                    this.currentExecVerb = token.nextSTokenIndex(2).currentToken;
                    state.currentToken = this.newCOBOLToken(COBOLTokenStyle.Exec, lineNumber, line, 0, current, `EXEC ${this.currentExec} ${this.currentExecVerb}`, state.currentDivision,"",false);
                    this.currentExecToken = state.currentToken;
                    token.moveToNextToken();
                    continue;
                }

                // do we have a split line exec
                if (this.currentExecToken !== undefined) {
                    if (this.currentExecVerb.length === 0) {
                        this.currentExecVerb = current;
                        this.currentExecToken.description = `EXEC ${this.currentExec} ${this.currentExecVerb}`;
                    }
                }

                /* finish processing end-exec */
                if (currentLower === "end-exec") {
                    if (this.currentExecToken !== undefined) {
                        if (state.currentToken !== undefined) {
                            this.currentExecToken.rangeEndLine = token.currentLineNumber;
                            this.currentExecToken.rangeEndColumn = token.currentCol + token.currentToken.length;
                            this.execTokensInOrder.push(this.currentExecToken);  // remember token
                        }

                        if (this.configHandler.enable_exec_sql_cursors) {
                            const text = this.currentExecToken.sourceHandler.getText(this.currentExecToken.rangeStartLine, this.currentExecToken.rangeStartColumn, this.currentExecToken.rangeEndLine, this.currentExecToken.rangeEndColumn);
                            this.parseExecStatement(this.currentExec, this.currentExecToken, text);
                        }
                    }
                    this.currentExec = "";
                    this.currentExecVerb = "";
                    state.currentToken = undefined;
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = true;
                    token.endsWithDot = state.endsWithDot;

                    this.currentExecToken = undefined;
                    continue;
                }

                /* skip everything in between exec .. end-exec */
                if (state.currentToken !== undefined && this.currentExec.length !== 0) {
                    if (currentLower === "include" && this.currentExec.toLowerCase() === "sql") {
                        const sqlCopyBook = token.nextSTokenOrBlank().currentToken;
                        const trimmedCopyBook = COBOLSourceScanner.trimLiteral(sqlCopyBook, true);
                        let insertInSection = this.copybookNestedInSection ? state.currentSection : state.currentDivision;
                        if (insertInSection === undefined) {
                            insertInSection = state.currentDivision;
                        }
                        this.tokensInOrder.pop();
                        const copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, currentCol, trimmedCopyBook, "SQL INCLUDE " + sqlCopyBook, insertInSection,"",false);
                        if (this.copyBooksUsed.has(trimmedCopyBook) === false) {
                            const cbInfo = new copybookState();
                            cbInfo.trimmedCopyBook = trimmedCopyBook;
                            cbInfo.copyBook = sqlCopyBook;
                            cbInfo.line = line;
                            cbInfo.startLineNumber = lineNumber;
                            cbInfo.endLineNumber = lineNumber;
                            cbInfo.startCol = currentCol;
                            cbInfo.endCol = line.indexOf(sqlCopyBook) + sqlCopyBook.length;
                            const fileName = this.externalFeatures.expandLogicalCopyBookToFilenameOrEmpty(trimmedCopyBook, copyToken.extraInformation1, this.configHandler);
                            if (fileName.length === 0) {
                                continue;
                            }
                            cbInfo.fileName = fileName;
                            const copybookToken = new COBOLCopybookToken(copyToken, false, cbInfo);
                            this.copyBooksUsed.set(trimmedCopyBook, copybookToken);
                            const qfile = new FileSourceHandler(this.configHandler, undefined, fileName, this.externalFeatures);
                            const currentIgnoreInOutlineView: boolean = this.sourceReferences.state.ignoreInOutlineView;
                            this.sourceReferences.state.ignoreInOutlineView = true;
                            this.sourceReferences.topLevel = true;

                            // eslint-disable-next-line @typescript-eslint/no-unused-vars
                            COBOLSourceScanner.ScanUncachedInlineCopybook(qfile, this, this.parse_copybooks_for_references, this.eventHandler, this.externalFeatures, false);
                            this.sourceReferences.topLevel = true;
                            this.sourceReferences.state.ignoreInOutlineView = currentIgnoreInOutlineView;
                        }
                        state.currentToken = copyToken;
                        continue;
                    }

                    // tweak exec to include verb
                    if (this.currentExecVerb.length === 0) {
                        this.currentExecVerb = token.currentToken;
                        state.currentToken.description += " " + this.currentExecVerb;
                        continue;
                    }

                    /* is this a reference to a variable? */
                    const varTokens = this.constantsOrVariables.get(currentLower);
                    if (varTokens !== undefined) {
                        let ctype: COBOLTokenStyle = COBOLTokenStyle.Variable;
                        let addReference = true;
                        for (const varToken of varTokens) {
                            if (varToken.ignoreInOutlineView === false || varToken.token.isTokenFromSourceDependancyCopyBook) {
                                ctype = (varToken.tokenType === COBOLTokenStyle.Unknown) ? ctype : varToken.tokenType;
                            } else {
                                addReference = false;
                            }
                        }
                        if (addReference) {
                            this.addVariableReference(this.sourceReferences.constantsOrVariablesReferences, currentLower, lineNumber, token.currentCol, ctype);
                        }
                    }
                    continue;
                }

                if (prevToken === "$") {
                    if (currentLower === "region") {
                        const trimmedCurrent = COBOLSourceScanner.trimLiteral(current, false);
                        const restOfLine = line.substring(token.currentCol);
                        const ctoken = this.newCOBOLToken(COBOLTokenStyle.Region, lineNumber, line, currentCol, trimmedCurrent, restOfLine, state.currentDivision,"",false);

                        this.activeRegions.push(ctoken);
                        token.endToken();
                        continue;
                    }

                    if (currentLower === "end-region") {
                        if (this.activeRegions.length > 0) {
                            const ctoken = this.activeRegions.pop();
                            if (ctoken !== undefined) {
                                ctoken.rangeEndLine = lineNumber;
                                ctoken.rangeEndColumn = line.toLowerCase().indexOf(currentLower) + currentLower.length;
                                this.regions.push(ctoken);
                            }
                        }
                    }
                }

                if (state.declaratives !== undefined && prevTokenLower === "end" && currentLower === "declaratives") {
                    state.declaratives.rangeEndLine = lineNumber;
                    state.declaratives.rangeEndColumn = line.indexOf(current);
                    state.inDeclaratives = false;
                    state.declaratives = undefined;
                    state.declaratives = this.newCOBOLToken(COBOLTokenStyle.EndDeclaratives, lineNumber, line, currentCol, current, current, state.currentDivision,"",false);

                    continue;
                }

                //remember replace
                if (state.enable_text_replacement && currentLower === "replace") {
                    state.inReplace = true;
                    state.skipToDot = true;
                    state.captureReplaceLeft = true;
                    state.replaceLeft = state.replaceRight = "";
                    continue;
                }

                // handle sections
                if (state.currentClass === undefined && prevToken.length !== 0 && currentLower === "section" && (prevTokenLower !== "exit")) {
                    if (prevTokenLower === "declare") {
                        continue;
                    }

                    // So we need to insert a fake data division?
                    if (state.currentDivision === undefined) {
                        if (prevTokenLower === "file" ||
                            prevTokenLower === "working-storage" ||
                            prevTokenLower === "local-storage" ||
                            prevTokenLower === "screen" ||
                            prevTokenLower === "linkage") {

                            if (this.ImplicitProgramId.length !== 0) {
                                const trimmedCurrent = COBOLSourceScanner.trimLiteral(this.ImplicitProgramId, true);
                                const ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, "program-id. " + this.ImplicitProgramId, 0, trimmedCurrent, prevPlusCurrent, state.currentDivision,"",true);
                                state.programs.push(ctoken);
                                ctoken.ignoreInOutlineView = true;
                                this.ImplicitProgramId = "";        /* don't need it */
                            }
                            state.currentSection = undefined;
                            state.currentParagraph = undefined;
                            state.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Data Division", 0, "Data", "Data Division (Optional)", state.currentDivision,"",false);
                            state.currentDivision.ignoreInOutlineView = true;
                        }
                    }

                    if (prevTokenLower === "working-storage" || prevTokenLower === "linkage" ||
                        prevTokenLower === "local-storage" || prevTokenLower === "file-control" ||
                        prevTokenLower === "file" || prevTokenLower === "screen") {
                        state.pickFields = true;
                        state.inProcedureDivision = false;
                    }

                    state.currentParagraph = undefined;
                    state.currentSection = this.newCOBOLToken(COBOLTokenStyle.Section, lineNumber, line, prevCurrentCol, prevToken, prevPlusCurrent, state.currentDivision,"",false);
                    this.sections.set(prevTokenLower, state.currentSection);
                    state.current01Group = undefined;
                    state.currentLevel = undefined;

                    continue;
                }

                // handle divisions
                if (state.captureDivisions && prevTokenLower.length !== 0 && currentLower === "division") {
                    state.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, line, 0, prevToken, prevPlusCurrent, undefined,"",false);

                    if (prevTokenLower === "procedure") {
                        state.inProcedureDivision = true;
                        state.pickFields = false;
                        state.procedureDivision = state.currentDivision;

                        // create implicit section/paragraph and also a duplicate "procedure division" named fake section
                        let pname = this.implicitCount === 0 ? prevToken : prevToken+"-"+this.implicitCount;
                        const pname_lower = pname.toLowerCase();
                        const newTokenParagraph = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, 0, pname_lower, prevPlusCurrent, state.currentDivision,"",true);
                        newTokenParagraph.ignoreInOutlineView = true;
                        this.paragraphs.set(pname_lower, newTokenParagraph);
                        this.sourceReferences.ignoreUnusedSymbol.set(newTokenParagraph.tokenNameLower,newTokenParagraph.tokenNameLower);
                        this.sourceReferences.ignoreUnusedSymbol.set(pname_lower,pname_lower);

                        state.currentParagraph = newTokenParagraph;
                        state.currentSection = undefined;
                        if (state.endsWithDot === false) {
                            state.pickUpUsing = true;
                        }
                        this.implicitCount++;
                    }

                    continue;
                }

                // handle entries
                if (prevTokenLowerUntrimmed === "entry") {
                    let entryStatement = prevPlusCurrent;
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current, true);
                    const nextSTokenOrBlank = token.nextSTokenOrBlank().currentToken;
                    if (nextSTokenOrBlank === "&") {
                        entryStatement = prevToken + " " + token.compoundItems(trimmedCurrent);
                    }
                    const ctoken = this.newCOBOLToken(COBOLTokenStyle.EntryPoint, lineNumber, line, currentCol, trimmedCurrent, entryStatement, state.currentDivision,"",false);

                    state.entryPointCount++;
                    state.parameters = [];
                    state.currentProgramTarget = new CallTargetInformation(current, ctoken, true, []);
                    this.callTargets.set(trimmedCurrent, state.currentProgramTarget);
                    state.pickUpUsing = true;
                    continue;
                }

                // handle program-id
                if (prevTokenLower === "program-id") {
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current, true);
                    const ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, line, prevCurrentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision,"",false);
                    ctoken.rangeStartColumn = prevCurrentCol;
                    state.programs.push(ctoken);
                    if (state.currentDivision !== undefined) {
                        state.currentDivision.rangeEndLine = ctoken.endLine;
                        state.currentDivision.rangeEndColumn = ctoken.endColumn;
                    }
                    if (trimmedCurrent.indexOf(" ") === -1 && token.isTokenPresent("external") === false) {
                        state.parameters = [];
                        state.currentProgramTarget = new CallTargetInformation(current, ctoken, false, []);
                        this.callTargets.set(trimmedCurrent, state.currentProgramTarget);
                        this.ProgramId = trimmedCurrent;
                    }
                    this.ImplicitProgramId = "";        /* don't need it */
                    continue;
                }

                // handle class-id
                if (prevTokenLower === "class-id") {
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current, true);
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.ClassId, lineNumber, line, currentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision,"",false);
                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    state.pickFields = true;
                    this.classes.set(trimmedCurrent, state.currentClass);

                    continue;
                }

                // handle "end class, enum, valuetype"
                if (state.currentClass !== undefined && prevTokenLower === "end" &&
                    (currentLower === "class" || currentLower === "enum" || currentLower === "valuetype" || currentLower === "interface")) {
                    state.currentClass.rangeEndLine = lineNumber;
                    state.currentClass.rangeEndColumn = line.toLowerCase().indexOf(currentLower) + currentLower.length;

                    state.currentClass = undefined;
                    state.captureDivisions = true;
                    state.currentMethod = undefined;
                    state.pickFields = false;
                    state.inProcedureDivision = false;
                    continue;
                }

                // handle enum-id
                if (prevTokenLower === "enum-id") {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.EnumId, lineNumber, line, currentCol, COBOLSourceScanner.trimLiteral(current, true), prevPlusCurrent, undefined,"",false);

                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    state.pickFields = true;
                    continue;
                }

                // handle interface-id
                if (prevTokenLower === "interface-id") {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.InterfaceId, lineNumber, line, currentCol, COBOLSourceScanner.trimLiteral(current, true), prevPlusCurrent, state.currentDivision,"",false);

                    state.pickFields = true;
                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    continue;
                }

                // handle valuetype-id
                if (prevTokenLower === "valuetype-id") {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.ValueTypeId, lineNumber, line, currentCol, COBOLSourceScanner.trimLiteral(current, true), prevPlusCurrent, state.currentDivision,"",false);

                    state.pickFields = true;
                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    continue;
                }

                // handle function-id
                if (prevTokenLower === "function-id") {
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current, true);
                    state.currentFunctionId = this.newCOBOLToken(COBOLTokenStyle.FunctionId, lineNumber, line, currentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision,"",false);
                    state.captureDivisions = true;
                    state.pickFields = true;
                    state.parameters = [];
                    state.currentProgramTarget = new CallTargetInformation(current, state.currentFunctionId, false, []);
                    this.functionTargets.set(trimmedCurrent, state.currentProgramTarget);

                    continue;
                }

                // handle method-id
                if (prevTokenLower === "method-id") {
                    const currentLowerTrim = COBOLSourceScanner.trimLiteral(currentLower, true);
                    const style = currentLowerTrim === "new" ? COBOLTokenStyle.Constructor : COBOLTokenStyle.MethodId;
                    const nextTokenLower = token.nextSTokenOrBlank().currentTokenLower;
                    const nextToken = token.nextSTokenOrBlank().currentToken;

                    if (nextTokenLower === "property") {
                        const nextPlusOneToken = token.nextSTokenIndex(2).currentToken;
                        const trimmedProperty = COBOLSourceScanner.trimLiteral(nextPlusOneToken, true);
                        state.currentMethod = this.newCOBOLToken(COBOLTokenStyle.Property, lineNumber, line, currentCol, trimmedProperty, nextToken + " " + nextPlusOneToken, state.currentDivision,"",false);
                        this.methods.set(trimmedProperty, state.currentMethod);
                    } else {
                        const trimmedCurrent = COBOLSourceScanner.trimLiteral(current, true);
                        state.currentMethod = this.newCOBOLToken(style, lineNumber, line, currentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision,"",false);
                        this.methods.set(trimmedCurrent, state.currentMethod);
                    }

                    state.pickFields = true;
                    state.captureDivisions = false;
                    continue;
                }

                // handle "end method"
                if (state.currentMethod !== undefined && prevTokenLower === "end" && currentLower === "method") {
                    state.currentMethod.rangeEndLine = lineNumber;
                    state.currentMethod.rangeEndColumn = line.toLowerCase().indexOf(currentLower) + currentLower.length;

                    state.currentMethod = undefined;
                    state.pickFields = false;
                    continue;
                }

                // handle "end program"
                if (state.programs.length !== 0 && prevTokenLower === "end" && currentLower === "program") {
                    const currentProgram: COBOLToken | undefined = state.programs.pop();
                    if (currentProgram !== undefined) {
                        currentProgram.rangeEndLine = lineNumber;
                        currentProgram.rangeEndColumn = line.length;
                    }

                    if (state.currentDivision !== undefined) {
                        state.currentDivision.rangeEndLine = lineNumber;
                        state.currentDivision.rangeEndColumn = prevCurrentCol - 1;
                    }

                    if (state.currentSection !== undefined) {
                        state.currentSection.rangeEndLine = lineNumber;
                        state.currentSection.rangeEndColumn = prevCurrentCol - 1;
                    }

                    if (state.currentParagraph !== undefined) {
                        state.currentParagraph.rangeEndLine = lineNumber;
                        state.currentParagraph.rangeEndColumn = prevCurrentCol - 1;
                    }

                    state.currentDivision = undefined;
                    state.currentSection = undefined;
                    state.currentParagraph = undefined;
                    state.inProcedureDivision = false;
                    state.pickFields = false;
                    continue;
                }

                // handle "end function"
                if (state.currentFunctionId !== undefined && prevTokenLower === "end" && currentLower === "function") {
                    state.currentFunctionId.rangeEndLine = lineNumber;
                    state.currentFunctionId.rangeEndColumn = line.toLowerCase().indexOf(currentLower) + currentLower.length;
                    this.newCOBOLToken(COBOLTokenStyle.EndFunctionId, lineNumber, line, currentCol, prevToken, current, state.currentDivision,"",false);

                    state.pickFields = false;
                    state.inProcedureDivision = false;

                    if (state.currentDivision !== undefined) {
                        state.currentDivision.rangeEndLine = lineNumber;
                        // state.currentDivision.endColumn = 0;
                    }

                    if (state.currentSection !== undefined) {
                        state.currentSection.rangeEndLine = lineNumber;
                        // state.currentSection.endColumn = 0;
                    }
                    state.currentDivision = undefined;
                    state.currentSection = undefined;
                    state.currentParagraph = undefined;
                    state.procedureDivision = undefined;
                    continue;
                }


                if (prevTokenLower !== "end" && currentLower === "declaratives") {
                    state.declaratives = this.newCOBOLToken(COBOLTokenStyle.Declaratives, lineNumber, line, currentCol, current, current, state.currentDivision,"",false);
                    state.inDeclaratives = true;
                    // this.tokensInOrder.pop();       /* only interested it at the end */
                    continue;
                }

                //remember copy
                if (currentLower === "copy") {
                    state.copybook_state = new copybookState();
                    state.inCopy = true;
                    state.inCopyStartColumn = token.currentCol;
                    state.skipToDot = true;
                    state.copybook_state.copyVerb = current;
                    continue;
                }

                // we are in the procedure division
                if (state.captureDivisions && state.currentDivision !== undefined &&
                    state.currentDivision === state.procedureDivision && state.endsWithDot && state.prevEndsWithDot) {
                    if (!this.isValidKeyword(currentLower)) {
                        if (current.length !== 0) {
                            if (this.isParagraph(current)) {
                                if (state.currentSection !== undefined) {
                                    const newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, currentCol, current, current, state.currentSection,"",false);
                                    this.paragraphs.set(newToken.tokenNameLower, newToken);
                                } else {
                                    const newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, currentCol, current, current, state.currentDivision,"",false);
                                    this.paragraphs.set(newToken.tokenNameLower, newToken);
                                }
                            }
                        }
                    }
                }

                if (state.currentSection !== undefined) {
                    if (state.currentSection.tokenNameLower === "input-output") {
                        if (prevTokenLower === "fd" || prevTokenLower === "select") {
                            state.pickFields = true;
                        }
                    }

                    if (state.currentSection.tokenNameLower === "communication") {
                        if (prevTokenLower === "cd") {
                            state.pickFields = true;
                        }
                    }
                }


                // are we in the working-storage section?
                if (state.pickFields && prevToken.length > 0) {
                    /* only interesting in things that are after a number */
                    if (this.isNumber(prevToken) && !this.isNumber(current)) {
                        const isFiller: boolean = (currentLower === "filler");
                        let pickUpThisField: boolean = isFiller;
                        let trimToken = COBOLSourceScanner.trimLiteral(current, false);

                        // what other reasons do we need to pickup this line as a field?
                        if (!pickUpThisField) {
                            const compRegEx = /comp-[0-9]/;
                            // not a complete inclusive list but should cover the normal cases
                            if (currentLower === "pic" || currentLower === "picture" || compRegEx.test(currentLower) || currentLower.startsWith("binary-")) {
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
                            if (COBOLSourceScanner.isValidLiteral(currentLower)) {
                                let style = COBOLTokenStyle.Variable;
                                const nextTokenLower = token.nextSTokenOrBlank().currentTokenLower;

                                switch (prevToken) {
                                    case "78":
                                        style = COBOLTokenStyle.Constant;
                                        break;
                                    case "88":
                                        style = COBOLTokenStyle.ConditionName;
                                        break;
                                    case "66":
                                        style = COBOLTokenStyle.RenameLevel;
                                        break;
                                }

                                let extraInfo = prevToken;
                                let redefinesPresent = false;
                                let occursPresent = false;
                                if (prevToken === "01" || prevToken === "1") {
                                    if (nextTokenLower === "redefines") {
                                        extraInfo += "-GROUP";
                                    } else if (nextTokenLower.length === 0) {
                                        extraInfo += "-GROUP";
                                    } else if (state.currentSection !== undefined && state.currentSection.tokenNameLower === "report") {
                                        extraInfo += "-GROUP";
                                    }

                                    if (token.isTokenPresent("constant")) {
                                        style = COBOLTokenStyle.Constant;
                                    }

                                    redefinesPresent = token.isTokenPresent("redefines");
                                    if (redefinesPresent) {
                                        style = COBOLTokenStyle.Union;
                                    }
                                } else {
                                    if (nextTokenLower.length === 0) {
                                        extraInfo += "-GROUP";
                                    }

                                    occursPresent = token.isTokenPresent("occurs");
                                    if (occursPresent) {
                                        extraInfo += "-OCCURS";
                                    }
                                }

                                const ctoken = this.newCOBOLToken(style, lineNumber, line, prevCurrentCol, trimToken, trimToken, state.currentDivision, extraInfo,false);
                                if (!isFiller) {
                                    this.addVariableOrConstant(currentLower, ctoken);
                                }

                                // place the 88 under the 01 item
                                if (state.currentLevel !== undefined && prevToken === "88") {
                                    state.currentLevel.rangeEndLine = ctoken.startLine;
                                    state.currentLevel.rangeEndColumn = ctoken.startColumn + ctoken.tokenName.length;
                                }

                                if (prevToken !== "88") {
                                    state.currentLevel = ctoken;
                                }


                                if (prevToken === "01" || prevToken === "1") {
                                    // adjust group to include level number
                                    if (ctoken.rangeStartColumn > prevCurrentCol) {
                                        ctoken.rangeStartColumn = prevCurrentCol;
                                    }
                                }

                                if (prevToken === "01" || prevToken === "1" ||
                                    prevToken === "66" || prevToken === "77" || prevToken === "78") {
                                    if (nextTokenLower.length === 0 ||
                                        redefinesPresent || occursPresent ||
                                        (state.currentSection !== undefined && state.currentSection.tokenNameLower === "report" && nextTokenLower === "type")) {
                                        state.current01Group = ctoken;
                                    } else {
                                        state.current01Group = undefined;
                                    }
                                }

                                if (state.current01Group !== undefined) {
                                    state.current01Group.rangeEndLine = ctoken.rangeStartLine;
                                    state.current01Group.rangeEndColumn = ctoken.rangeEndColumn;
                                }

                                /* if spans multiple lines, skip to dot */
                                if (state.endsWithDot === false) {
                                    state.skipToDot = true;
                                    state.addReferencesDuringSkipToTag = true;
                                }
                            }
                        }
                        continue;
                    }

                    if ((prevTokenLower === "fd"
                        || prevTokenLower === "sd"
                        || prevTokenLower === "cd"
                        || prevTokenLower === "rd"
                        || prevTokenLower === "select")
                        && !this.isValidKeyword(currentLower)) {
                        const trimToken = COBOLSourceScanner.trimLiteral(current, false);

                        if (COBOLSourceScanner.isValidLiteral(currentLower)) {
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, currentCol, trimToken, trimToken, state.currentDivision, prevTokenLower,false);
                            this.addVariableOrConstant(currentLower, variableToken);
                        }

                        if (prevTokenLower === "rd" || prevTokenLower === "select") {
                            if (prevTokenLower === "select") {
                                state.addReferencesDuringSkipToTag = true;
                            }
                            state.skipToDot = true;
                        }
                        continue;
                    }

                    // add tokens from comms section
                    if (state !== undefined && state.currentSection !== undefined && state.currentSection.tokenNameLower === "communication" && !this.isValidKeyword(currentLower)) {
                        if (prevTokenLower === "is") {
                            const trimToken = COBOLSourceScanner.trimLiteral(current, false);
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, currentCol, trimToken, trimToken, state.currentDivision, prevTokenLower,false);
                            this.addVariableOrConstant(currentLower, variableToken);
                        }
                    }
                }

                /* add reference when perform is used */
                if (this.parse4References && this.sourceReferences !== undefined) {
                    if (state.inProcedureDivision) {
                        // not interested in literals
                        if (this.isQuotedLiteral(currentLower)) {
                            continue;
                        }

                        if (this.isNumber(currentLower) === true || this.isValidKeyword(currentLower) === true) {
                            continue;
                        }

                        // if the token contain '(' or ')' then it must be a variable reference
                        if (this.containsIndex(currentLower) === false) {

                            if (prevTokenLower === "perform" || prevTokenLower === "to" || prevTokenLower === "goto" ||
                                prevTokenLower === "thru" || prevTokenLower === "through") {

                                /* go nn, could be "move xx to nn" or "go to nn" */
                                let sourceStyle = COBOLTokenStyle.Unknown;
                                let sharedReferences = this.sourceReferences.unknownReferences;
                                if (this.isVisibleSection(currentLower)) {
                                    sourceStyle = COBOLTokenStyle.Section;
                                    sharedReferences = this.sourceReferences.targetReferences;
                                } else if (this.isVisibleParagraph(currentLower)) {
                                    sourceStyle = COBOLTokenStyle.Paragraph;
                                    sharedReferences = this.sourceReferences.targetReferences;
                                }
                                this.addTargetReference(sharedReferences, current, lineNumber, currentCol, sourceStyle, prevTokenLower);
                                continue;
                            }

                            /* is this a reference to a variable? */
                            const varTokens = this.constantsOrVariables.get(currentLower);
                            if (varTokens !== undefined) {
                                let ctype: COBOLTokenStyle = COBOLTokenStyle.Variable;
                                let addReference = true;
                                for (const varToken of varTokens) {
                                    if (varToken.ignoreInOutlineView === false || varToken.token.isTokenFromSourceDependancyCopyBook) {
                                        ctype = (varToken.tokenType === COBOLTokenStyle.Unknown) ? ctype : varToken.tokenType;
                                    } else {
                                        addReference = false;
                                    }
                                }
                                if (addReference) {
                                    this.addVariableReference(this.sourceReferences.constantsOrVariablesReferences, currentLower, lineNumber, token.currentCol, ctype);
                                }
                            } else {
                                // possible reference to a para or section
                                if (currentLower.length > 0) {
                                    let sourceStyle = COBOLTokenStyle.Unknown;
                                    let sharedReferences = this.sourceReferences.unknownReferences;

                                    if (this.isValidParagraphName(currentLower)) {
                                        const nextTokenLower = token.nextSTokenOrBlank().currentTokenLower;
                                        // const nextToken = token.nextSTokenOrBlank().currentToken;

                                        if (this.isVisibleSection(currentLower)) {
                                            sourceStyle = COBOLTokenStyle.Section;
                                            sharedReferences = this.sourceReferences.targetReferences;
                                            this.addTargetReference(sharedReferences, current, lineNumber, currentCol, sourceStyle, "");
                                        } else if (this.isVisibleParagraph(currentLower)) {
                                            sourceStyle = COBOLTokenStyle.Paragraph;
                                            sharedReferences = this.sourceReferences.targetReferences;
                                            this.addTargetReference(sharedReferences, current, lineNumber, currentCol, sourceStyle, "");
                                        } else {
                                            if (!(state.endsWithDot || nextTokenLower.startsWith("section"))) {
                                                this.addTargetReference(sharedReferences, current, lineNumber, currentCol, sourceStyle, "");
                                            }
                                        }
                                    }
                                }
                            }

                            continue;
                        }

                        // traverse map of possible variables that could be indexes etc.. a bit messy but works
                        for (const [pos, trimmedCurrentLower] of this.trimVariableToMap(currentLower)) {

                            if (this.isNumber(trimmedCurrentLower) === true || this.isValidKeyword(trimmedCurrentLower) === true || COBOLSourceScanner.isValidLiteral(trimmedCurrentLower) === false) {
                                continue;
                            }

                            /* is this a reference to a variable? */
                            const varTokens = this.constantsOrVariables.get(trimmedCurrentLower);
                            if (varTokens !== undefined) {
                                let ctype: COBOLTokenStyle = COBOLTokenStyle.Variable;
                                let addReference = true;
                                for (const varToken of varTokens) {
                                    if (varToken.ignoreInOutlineView === false || varToken.token.isTokenFromSourceDependancyCopyBook) {
                                        ctype = (varToken.tokenType === COBOLTokenStyle.Unknown) ? ctype : varToken.tokenType;
                                    } else {
                                        addReference = false;
                                    }
                                }
                                if (addReference) {
                                    this.addVariableReference(this.sourceReferences.constantsOrVariablesReferences, trimmedCurrentLower, lineNumber, token.currentCol + pos, ctype);
                                }
                            } else {
                                this.addVariableReference(this.sourceReferences.unknownReferences, trimmedCurrentLower, lineNumber, token.currentCol + pos, COBOLTokenStyle.Unknown);
                            }
                        }
                    }

                }
            }
            catch (e) {
                this.externalFeatures.logException("COBOLScannner line error: ", e as Error);
            }
        }
        while (token.moveToNextToken() === false);

        return token;
    }

    parseExecStatement(currentExecStype: string, token: COBOLToken, lines: string) {
        if (this.currentExecToken === undefined) {
            return;
        }

        let currentLine = this.currentExecToken.startLine;
        if (currentExecStype.toLowerCase() === "sql") {
            let prevDeclare = false
            for (const line of lines.split("\n")) {

                for (const execWord of line.replace('\t', ' ').split(" ")) {
                    if (prevDeclare) {
                        //                        this.externalFeatures.logMessage(` Found declare ${execWord} at ${currentLine}`);
                        const c = new SQLDeclare(token, currentLine);
                        this.execSQLDeclare.set(execWord.toLowerCase(), c)
                        prevDeclare = false;
                    } else {
                        if (execWord.toLowerCase() === 'declare') {
                            prevDeclare = true;
                        }
                    }
                }
                currentLine++;
            }

        }
    }

    parseSQLDeclareForReferences(fileid: number, _refExecSQLDeclareName: string, refExecToken: COBOLToken, lines: string, sqldeclare: SQLDeclare) {

        let currentLine = refExecToken.startLine;
        let currentColumn = refExecToken.startColumn;
        const refExecSQLDeclareNameLower = _refExecSQLDeclareName.toLowerCase();
        for (const line of lines.split("\n")) {
            for (const execWord of line.replace('\t', ' ').split(" ")) {
                const execWordLower = execWord.toLowerCase();
                if (execWordLower === refExecSQLDeclareNameLower) {
                    currentColumn = line.toLowerCase().indexOf(execWordLower);
                    const sr = new SourceReference(fileid, currentLine, currentColumn, currentLine, currentColumn + execWord.length, COBOLTokenStyle.SQLCursor);
                    sqldeclare.sourceReferences.push(sr);
                }
            }
            currentLine++;
        }

    }

    private processCopyBook(cbInfo: copybookState) {
        const state: ParseState = this.sourceReferences.state;

        let copyToken: COBOLToken | undefined = undefined;
        const isIn = cbInfo.isIn;
        const isOf = cbInfo.isOf;
        const lineNumber = cbInfo.startLineNumber;
        const tcurrentCurrentCol = cbInfo.startCol;
        const line = cbInfo.line;
        const trimmedCopyBook = cbInfo.trimmedCopyBook;
        const copyVerb = cbInfo.copyVerb;
        const copyBook = cbInfo.copyBook;

        let insertInSection = this.copybookNestedInSection ? state.currentSection : state.currentDivision;
        if (insertInSection === undefined) {
            insertInSection = state.currentDivision;
        }

        if (isIn || isOf) {
            const middleDesc = isIn ? " in " : " of ";
            const library_name_or_lit = COBOLSourceScanner.trimLiteral(cbInfo.library_name, true) + COBOLSourceScanner.trimLiteral(cbInfo.literal2, true);
            const desc: string = copyVerb + " " + copyBook + middleDesc + library_name_or_lit;
            // trim...
            copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBookInOrOf, lineNumber, line, tcurrentCurrentCol, trimmedCopyBook, desc, insertInSection, library_name_or_lit, false);
        }
        else {
            copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, tcurrentCurrentCol, trimmedCopyBook, copyVerb + " " + copyBook, insertInSection,"",false);
        }

        copyToken.rangeEndLine = cbInfo.endLineNumber;
        copyToken.rangeEndColumn = cbInfo.endCol;

        state.inCopy = false;

        if (this.copyBooksUsed.has(trimmedCopyBook) === false) {
            const copybookToken = new COBOLCopybookToken(copyToken, false, cbInfo);
            this.copyBooksUsed.set(trimmedCopyBook, copybookToken);
            const fileName = this.externalFeatures.expandLogicalCopyBookToFilenameOrEmpty(trimmedCopyBook, copyToken.extraInformation1, this.configHandler);
            if (fileName.length > 0) {
                cbInfo.fileName = fileName;
            } else {
                return;
            }

            if (this.sourceReferences !== undefined) {
                if (this.parse_copybooks_for_references && fileName.length > 0) {
                    cbInfo.fileName = fileName;
                    if (this.copyBooksUsed.has(fileName) === false) {

                        // move the source version of the copybook
                        this.copyBooksUsed.delete(trimmedCopyBook);

                        // add the specific version
                        this.copyBooksUsed.set(fileName, copybookToken);
                        const qfile = new FileSourceHandler(this.configHandler, undefined, fileName, this.externalFeatures);
                        const currentTopLevel = this.sourceReferences.topLevel;
                        const currentIgnoreInOutlineView: boolean = state.ignoreInOutlineView;
                        state.ignoreInOutlineView = true;
                        this.sourceReferences.topLevel = false;
                        const prevRepMap = this.sourceReferences.state.replaceMap;

                        if (this.configHandler.enable_text_replacement) {
                            this.sourceReferences.state.replaceMap = new Map<string, replaceToken>([...cbInfo.copyReplaceMap, ...prevRepMap]);
                        }
                        // eslint-disable-next-line @typescript-eslint/no-unused-vars
                        const qps = COBOLSourceScanner.ScanUncachedInlineCopybook(qfile, this, this.parse_copybooks_for_references, this.eventHandler, this.externalFeatures, false);
                        cbInfo.sourceHandler = qps.sourceHandler;
                        this.sourceReferences.state.replaceMap = prevRepMap;
                        this.sourceReferences.topLevel = currentTopLevel;
                        state.ignoreInOutlineView = currentIgnoreInOutlineView;

                        copybookToken.scanComplete = true;
                    }
                } else {
                    if (this.parse_copybooks_for_references && this.configHandler.linter_ignore_missing_copybook === false) {
                        const diagMessage = `Unable to locate copybook ${trimmedCopyBook}`;
                        this.diagMissingFileWarnings.set(diagMessage, new COBOLFileSymbol(this.filename, copyToken.startLine, trimmedCopyBook));
                    }
                }
            }
        }
    }

    private cobolLintLiteral = "cobol-lint";


    private processHintComments(commentLine: string, sourceFilename: string, sourceLineNumber: number) {
        const startOfTokenFor = this.configHandler.scan_comment_copybook_token;
        const startOfSourceDepIndex: number = commentLine.indexOf(startOfTokenFor);
        if (startOfSourceDepIndex !== -1) {
            const commentCommandArgs = commentLine.substring(startOfTokenFor.length + startOfSourceDepIndex).trim();
            const args = commentCommandArgs.split(" ");
            if (args.length !== 0) {
                let possRegExe: RegExp | undefined = undefined;
                for (const offset in args) {
                    const filenameTrimmed = args[offset].trim();
                    if (filenameTrimmed.startsWith("/") && filenameTrimmed.endsWith("/")) {
                        const regpart = filenameTrimmed.substring(1, filenameTrimmed.length - 1);
                        try {
                            possRegExe = new RegExp(regpart, "i");
                        }
                        catch (ex) {
                            this.generalWarnings.push(
                                new COBOLFileSymbol(this.filename, sourceLineNumber, `${ex}`)
                            )
                        }
                        continue;
                    }
                    const fileName = this.externalFeatures.expandLogicalCopyBookToFilenameOrEmpty(filenameTrimmed, "", this.configHandler);
                    if (fileName.length > 0) {
                        if (this.copyBooksUsed.has(fileName) === false) {
                            this.copyBooksUsed.set(fileName, COBOLCopybookToken.Null);

                            const qfile = new FileSourceHandler(this.configHandler, possRegExe, fileName, this.externalFeatures);
                            const currentIgnoreInOutlineView: boolean = this.sourceReferences.state.ignoreInOutlineView;
                            const currentTopLevel = this.sourceReferences.topLevel;
                            this.sourceReferences.state.ignoreInOutlineView = true;
                            this.sourceReferences.topLevel = false;

                            // eslint-disable-next-line @typescript-eslint/no-unused-vars
                            COBOLSourceScanner.ScanUncachedInlineCopybook(qfile, this, this.parse_copybooks_for_references, this.eventHandler, this.externalFeatures, this.configHandler.scan_comments_for_references);
                            this.sourceReferences.topLevel = currentTopLevel;
                            this.sourceReferences.state.ignoreInOutlineView = currentIgnoreInOutlineView;
                        }
                    } else {
                        if (this.configHandler.linter_ignore_missing_copybook === false) {
                            const diagMessage = `${startOfTokenFor}: Unable to locate copybook ${filenameTrimmed} specified in embedded comment`;
                            this.diagMissingFileWarnings.set(diagMessage, new COBOLFileSymbol(sourceFilename, sourceLineNumber, filenameTrimmed));
                        }
                    }
                }
            }
        }
    }

    private processCommentForLinter(commentLine: string, startOfCOBOLLint: number): void {
        const commentCommandArgs = commentLine.substring(this.cobolLintLiteral.length + startOfCOBOLLint).trim();
        let args = commentCommandArgs.split(" ");
        const command = args[0];
        args = args.slice(1);
        const commandTrimmed = command !== undefined ? command.trim() : undefined;
        if (commandTrimmed !== undefined) {
            if (commandTrimmed === CobolLinterProviderSymbols.NotReferencedMarker_external ||
                commandTrimmed === CobolLinterProviderSymbols.OLD_NotReferencedMarker_external) {
                for (const offset in args) {
                    this.sourceReferences.ignoreUnusedSymbol.set(args[offset].toLowerCase(), args[offset]);
                }
            }
        }
    }

    private lastCOBOLLS: COBOLToken | undefined = undefined;

    public processComment(configHandler: ICOBOLSettings, sourceHandler: ISourceHandlerLite, commentLine: string, sourceFilename: string, sourceLineNumber: number, startPos: number, format: ESourceFormat): void {
        this.sourceReferences.state.currentLineIsComment = true;

        // should consider other inline comments (aka terminal) and fixed position comments
        const startOfComment: number = commentLine.indexOf("*>");

        if (startOfComment !== undefined && startOfComment !== -1) {
            const trimmedLine = commentLine.substring(0, startOfComment).trimEnd();
            if (trimmedLine.length !== 0) {
                // we still have something to process
                this.sourceReferences.state.currentLineIsComment = false;
                commentLine = trimmedLine;
            }
        }

        const startOfCOBOLLint: number = commentLine.indexOf(this.cobolLintLiteral);
        if (startOfCOBOLLint !== -1) {
            this.processCommentForLinter(commentLine, startOfCOBOLLint);
        }

        if (this.scan_comments_for_hints) {
            this.processHintComments(commentLine, sourceFilename, sourceLineNumber);
        }

        // ls control
        if (this.scan_comment_for_ls_control) {
            if (this.sourceReferences.state.skipToEndLsIgnore) {
                const startOfEndLs = commentLine.indexOf(this.configHandler.scan_comment_end_ls_ignore);
                if (startOfEndLs !== -1) {
                    this.sourceReferences.state.skipToEndLsIgnore = false;
                    if (this.lastCOBOLLS !== undefined) {
                        this.lastCOBOLLS.rangeEndLine = sourceLineNumber;
                        this.lastCOBOLLS.rangeEndColumn = startOfEndLs + this.configHandler.scan_comment_begin_ls_ignore.length;

                        this.lastCOBOLLS = undefined;
                    }
                }
            } else {
                const startOfBeginLS = commentLine.indexOf(this.configHandler.scan_comment_begin_ls_ignore);
                if (startOfBeginLS !== -1) {
                    this.sourceReferences.state.skipToEndLsIgnore = true;
                    this.lastCOBOLLS = this.newCOBOLToken(COBOLTokenStyle.IgnoreLS, sourceLineNumber, commentLine, startOfBeginLS, this.configHandler.scan_comment_begin_ls_ignore, "Ignore Source", undefined, "", false);
                    this.lastCOBOLLS.rangeStartColumn = startPos;
                }
            }
        }
    }

    public findNearestSectionOrParagraph(line: number):COBOLToken|undefined {
        let nearToken : COBOLToken|undefined = undefined;
        for(const [, token] of this.sections) {
            if (line >= token.rangeStartLine && line <= token.rangeEndLine) {
                nearToken = token;
            }
        }
        for(const [, token] of this.paragraphs) {
            if (line >= token.rangeStartLine && line <= token.rangeEndLine) {
                nearToken = token;;
            }
        }

        return nearToken;
    }
}
