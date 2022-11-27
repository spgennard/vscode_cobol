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
    Unknown = "Unknown",
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
    public filenameAsURI: string;
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
    public extraInformation1: string;
    public inSection: COBOLToken | undefined;
    public tokenOffset: number;

    public constructor(
        filenameAsURI: string, filename: string, tokenType: COBOLTokenStyle, startLine: number,
        startColumn: number, token: string, description: string,
        parentToken: COBOLToken | undefined, inProcedureDivision: boolean, extraInformation1: string) {
        this.ignoreInOutlineView = false;
        this.filenameAsURI = filenameAsURI;
        this.filename = filename;
        this.tokenType = tokenType;
        this.startLine = startLine;
        this.tokenName = token.trim();
        this.tokenNameLower = this.tokenName.toLowerCase();
        this.startColumn = startColumn;
        this.description = description;
        this.endLine = this.startLine;
        this.endColumn = this.startColumn + this.tokenName.length;
        this.parentToken = parentToken;
        this.inProcedureDivision = inProcedureDivision;
        this.extraInformation1 = extraInformation1;
        this.inSection = undefined;
        this.tokenOffset = 0;

        if (this.tokenName.length !== 0) {
            /* ensure we don't have any odd start columns */
            if (this.startColumn < 0) {
                this.startColumn = 0;
            }
        }
    }
}

export class COBOLVariable {
    public ignoreInOutlineView: boolean;
    public token: COBOLToken;
    public tokenType: COBOLTokenStyle;

    constructor(token: COBOLToken) {
        this.token = token;
        this.tokenType = token.tokenType;
        this.ignoreInOutlineView = token.ignoreInOutlineView;
    }
}

export class SourceReference {
    public fileIdentifer: number;
    public line: number;
    public column: number;
    public length: number;
    public tokenStyle: COBOLTokenStyle;

    public constructor(fileIdentifer: number, line: number, column: number, length: number, tokenStyle: COBOLTokenStyle) {
        this.fileIdentifer = fileIdentifer;
        this.line = line;
        this.column = column;
        this.length = length;
        this.tokenStyle = tokenStyle;
    }
}

class StreamToken {
    public currentToken: string;
    public currentTokenLower: string;
    public endsWithDot: boolean;
    public currentCol: number;

    public static Blank = new StreamToken("", "", false, 0);

    public constructor(currentToken: string, currentTokenLower: string, endsWithDot: boolean, currentCol: number) {
        this.currentToken = currentToken;
        this.currentTokenLower = currentTokenLower;
        this.endsWithDot = endsWithDot;
        this.currentCol = currentCol;
    }
}

class Token {
    private tokenIndex = 0;

    public currentToken = "";
    public prevToken = "";
    public prevTokenToken: Token | undefined = undefined;
    public currentTokenLower = "";
    public prevTokenLower = "";
    public currentCol = 0;

    public endsWithDot = false;

    private stokens: StreamToken[] = [];

    public constructor(line: string, previousToken: Token | undefined) {
        const lineTokens: string[] = [];
        this.prevTokenToken = previousToken;
        if (previousToken !== undefined) {
            this.prevToken = previousToken.currentToken;
        }
        SplitTokenizer.splitArgument(line, lineTokens);
        let rollingColumn = 0;
        for (let c = 0; c < lineTokens.length; c++) {
            const currentToken = lineTokens[c];
            const currentTokenLower = currentToken.toLowerCase();

            rollingColumn = line.indexOf(currentToken, rollingColumn);

            const endsWithDot = currentToken.length === 0 ? false : currentToken.charAt(currentToken.length - 1) === ".";
            this.stokens.push(new StreamToken(currentToken, currentTokenLower, endsWithDot, rollingColumn));
        }
        this.tokenIndex = 0;
        this.setupNextToken();

        if (previousToken !== undefined) {
            // wire in previous token into this token
            if (previousToken.stokens.length > 0) {
                const prevTokenId = previousToken.stokens.length - 1;
                this.prevToken = previousToken.stokens[prevTokenId].currentToken;
                this.prevTokenLower = previousToken.stokens[prevTokenId].currentTokenLower;
            }
        }
    }

    public static Blank = new Token("", undefined);

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

    public compoundItems(startCompound: string, scanner: COBOLSourceScanner) {
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
            const trimCurrent = COBOLSourceScanner.trimLiteral(stok.currentToken);
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

        const stok: StreamToken = this.stokens[this.tokenIndex];
        if (stok !== undefined) {
            this.currentToken = stok.currentToken;
            this.currentTokenLower = stok.currentTokenLower;
            this.endsWithDot = stok.endsWithDot;
            this.currentCol = stok.currentCol;
        } else {
            this.currentToken = this.currentTokenLower = "";
            this.endsWithDot = false;
            this.currentCol = 0;
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
    private replaceToken: string;
    public rex4wordreplace: RegExp;

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

    public static Null = new COBOLCopybookToken(undefined, false, undefined);

    constructor(token: COBOLToken | undefined, parsed: boolean, statementInformation: copybookState | undefined) {
        this.token = token;
        this.scanComplete = parsed;
        this.statementInformation = statementInformation;
    }
}

export class SharedSourceReferences {
    public filenames: string[];
    public filenameURIs: string[];

    public targetReferences: Map<string, SourceReference[]>;
    public constantsOrVariablesReferences: Map<string, SourceReference[]>;
    public unknownReferences: Map<string, SourceReference[]>;

    public sharedConstantsOrVariables: Map<string, COBOLVariable[]>;
    public sharedSections: Map<string, COBOLToken>;
    public sharedParagraphs: Map<string, COBOLToken>;
    public copyBooksUsed: Map<string, COBOLCopybookToken>;
    public state: ParseState;
    public tokensInOrder: COBOLToken[];

    public ignoreUnusedSymbol: Map<string, string>;

    public topLevel: boolean;

    public startTime: number;

    constructor(configHandler: ICOBOLSettings, topLevel: boolean, startTime: number) {
        this.filenames = [];
        this.filenameURIs = [];
        this.targetReferences = new Map<string, SourceReference[]>();
        this.constantsOrVariablesReferences = new Map<string, SourceReference[]>();
        this.unknownReferences = new Map<string, SourceReference[]>();

        this.sharedConstantsOrVariables = new Map<string, COBOLVariable[]>();
        this.sharedSections = new Map<string, COBOLToken>();
        this.sharedParagraphs = new Map<string, COBOLToken>();
        this.copyBooksUsed = new Map<string, COBOLCopybookToken>();
        this.state = new ParseState(configHandler);
        this.tokensInOrder = [];
        this.topLevel = topLevel;
        this.startTime = startTime;
        this.ignoreUnusedSymbol = new Map<string, string>();
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

    public getReferenceInformation(variable: string, startLine: number, startColumn: number): [number, number] {
        let defvars = this.constantsOrVariablesReferences.get(variable);
        if (defvars === undefined) {
            defvars = this.constantsOrVariablesReferences.get(variable.toLowerCase());
        }

        if (defvars === undefined) {
            return [0, 0];
        }

        let definedCount = 0;
        let referencedCount = 0;

        for (const defvar of defvars) {
            if (defvar.line === startLine && defvar.column === startColumn) {
                definedCount++;
            } else {
                referencedCount++;
            }
        }

        return [definedCount, referencedCount];
    }

}

export enum UsingState {
    BY_VALUE,
    BY_REF,
    BY_CONTENT,
    RETURNING,
    UNKNOWN
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
    currentToken: COBOLToken | undefined;
    currentRegion: COBOLToken | undefined;
    currentDivision: COBOLToken | undefined;
    currentSection: COBOLToken | undefined;
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
    public sourceHandler: ISourceHandler;
    public filename: string;
    // eslint-disable-next-line @typescript-eslint/ban-types
    public lastModifiedTime: BigInt = BigInt(0);

    public tokensInOrder: COBOLToken[] = [];

    public sections: Map<string, COBOLToken>;
    public paragraphs: Map<string, COBOLToken>;
    public constantsOrVariables: Map<string, COBOLVariable[]>;
    public callTargets: Map<string, CallTargetInformation>;
    public functionTargets: Map<string, CallTargetInformation>;
    public classes: Map<string, COBOLToken>;
    public methods: Map<string, COBOLToken>;
    public copyBooksUsed: Map<string, COBOLCopybookToken>;
    public diagWarnings: Map<string, COBOLFileSymbol>;
    public commentReferences: COBOLFileAndColumnSymbol[];

    public parse4References: boolean;
    public sourceReferences: SharedSourceReferences;
    public sourceFileId: number;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public cache4PerformTargets: any | undefined = undefined;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public cache4ConstantsOrVars: any | undefined = undefined;

    public ImplicitProgramId = "";

    public sourceFormat: ESourceFormat = ESourceFormat.unknown;

    public sourceIsCopybook = false;

    public workspaceFile: COBOLWorkspaceFile;

    public readonly parse_copybooks_for_references: boolean;
    public readonly scan_comments_for_hints: boolean;

    readonly copybookNestedInSection: boolean;

    readonly configHandler: ICOBOLSettings;

    parseHint_OnOpenFiles: string[] = [];
    parseHint_WorkingStorageFiles: string[] = [];
    parseHint_LocalStorageFiles: string[] = [];
    parseHint_ScreenSectionFiles: string[] = [];

    private eventHandler: ICOBOLSourceScannerEvents;
    private externalFeatures: IExternalFeatures;

    public scanAborted: boolean;

    private currentExec = "";
    private currentExecVerb = "";

    readonly COBOLKeywordDictionary: Map<string, string>;


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
            externalFeatures
        );
    }

    private static ScanUncachedInlineCopybook(
        sourceHandler: ISourceHandler,
        parentSource: COBOLSourceScanner,
        parse_copybooks_for_references: boolean,
        eventHandler: ICOBOLSourceScannerEvents,
        externalFeatures: IExternalFeatures

    ): COBOLSourceScanner {

        const configHandler = parentSource.configHandler;
        const sharedSource = parentSource.sourceReferences;

        return new COBOLSourceScanner(
            externalFeatures.performance_now(),
            sourceHandler,
            configHandler,
            sharedSource,
            parse_copybooks_for_references,
            eventHandler,
            externalFeatures);
    }

    public constructor(
        startTime: number,
        sourceHandler: ISourceHandler, configHandler: ICOBOLSettings,
        sourceReferences: SharedSourceReferences = new SharedSourceReferences(configHandler, true, startTime),
        parse_copybooks_for_references: boolean,
        sourceEventHandler: ICOBOLSourceScannerEvents,
        externalFeatures: IExternalFeatures) {
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

        this.copybookNestedInSection = configHandler.copybooks_nested;
        this.copyBooksUsed = new Map<string, COBOLCopybookToken>();
        this.sections = new Map<string, COBOLToken>();
        this.paragraphs = new Map<string, COBOLToken>();
        this.constantsOrVariables = new Map<string, COBOLVariable[]>();
        this.callTargets = new Map<string, CallTargetInformation>();
        this.functionTargets = new Map<string, CallTargetInformation>();
        this.classes = new Map<string, COBOLToken>();
        this.methods = new Map<string, COBOLToken>();
        this.diagWarnings = new Map<string, COBOLFileSymbol>();
        this.commentReferences = [];
        this.parse4References = sourceHandler !== null;
        this.cache4PerformTargets = undefined;
        this.cache4ConstantsOrVars = undefined;
        this.scanAborted = false;
        this.COBOLKeywordDictionary = getCOBOLKeywordDictionary(this.sourceHandler.getLanguageId());
        let sourceLooksLikeCOBOL = false;
        let prevToken: Token = Token.Blank;

        const hasCOBOLExtension = path.extname(filename).length > 0 ? true : false;
        this.sourceReferences = sourceReferences;

        this.sourceFileId = 0;
        this.sourceFileId = sourceReferences.filenames.length;
        sourceReferences.filenames.push(sourceHandler.getFilename());
        sourceReferences.filenameURIs.push(sourceHandler.getUriAsString());

        this.constantsOrVariables = sourceReferences.sharedConstantsOrVariables;
        this.paragraphs = sourceReferences.sharedParagraphs;
        this.sections = sourceReferences.sharedSections;
        this.tokensInOrder = sourceReferences.tokensInOrder;
        this.copyBooksUsed = sourceReferences.copyBooksUsed;

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
                    this.externalFeatures.logException("COBOLScannner - Parse error : " + e, e as Error);
                }
            }

            // Do we have some sections?
            if (preParseState.sectionsInToken === 0 && preParseState.divisionsInToken === 0) {
                /* if we have items that could be in a data division */

                if (preParseState.procedureDivisionRelatedTokens !== 0 && preParseState.procedureDivisionRelatedTokens > preParseState.workingStorageRelatedTokens) {
                    this.ImplicitProgramId = "";

                    const fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Procedure Division", 0, "Procedure", "Procedure Division (CopyBook)", state.currentDivision);
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
                    const fakeDivision = this.newCOBOLToken(COBOLTokenStyle.Division, 0, "Data Division", 0, "Data", "Data Division (CopyBook)", state.currentDivision);
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

            /* leave early */
            if (sourceLooksLikeCOBOL === false) {
                if (filename.length > 0) {
                    if (sourceHandler.getLineCount() > maxLines) {
                        this.externalFeatures.logMessage(` Warning - Unable to determine if ${filename} is COBOL after scanning ${maxLines} lines (configurable via ${ExtensionDefaults.defaultEditorConfig}.pre_scan_line_limit setting)`);
                    } else {
                        this.externalFeatures.logMessage(` Unable to determine if ${filename} is COBOL and how it is used`);
                    }
                }
            }

            /* if the source has an extension, then continue on.. */
            if (hasCOBOLExtension) {
                sourceLooksLikeCOBOL = true;
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

        prevToken = Token.Blank;
        sourceHandler.resetCommentCount();

        const sourceTimeout = externalFeatures.getSourceTimeout(this.configHandler);
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
                    const prevTokenToParse = prevToken.endsWithDot === false ? prevToken : Token.Blank;
                    prevToken = this.parseLineByLine(l, prevTokenToParse, line);
                }

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
                        currentProgram.endLine = sourceHandler.getLineCount();
                        currentProgram.endColumn = lastLineLength;
                    }
                }
            }

            if (state.currentDivision !== undefined) {
                state.currentDivision.endLine = sourceHandler.getLineCount();
                state.currentDivision.endColumn = lastLineLength;
            }

            if (state.currentSection !== undefined) {
                state.currentSection.endLine = sourceHandler.getLineCount();
                state.currentSection.endColumn = lastLineLength;
            }

            if (state.currentParagraph !== undefined) {
                state.currentParagraph.endLine = sourceHandler.getLineCount();
                state.currentParagraph.endColumn = lastLineLength;
            }

            if (this.ImplicitProgramId.length !== 0) {
                const ctoken = this.newCOBOLToken(COBOLTokenStyle.ImplicitProgramId, 0, "", 0, this.ImplicitProgramId, this.ImplicitProgramId, undefined);
                ctoken.endLine = sourceHandler.getLineCount();
                ctoken.startLine = 0;
                ctoken.endColumn = lastLineLength;
                ctoken.ignoreInOutlineView = true;
                state.currentProgramTarget.Token = ctoken;
                this.tokensInOrder.pop();

                this.callTargets.set(this.ImplicitProgramId, state.currentProgramTarget);
            }

            // setup references for unknown forward references
            const unknown = [];
            // console.log(`DEBUG: unknownReferences : ${this.sourceReferences.unknownReferences.size}`);
            for (const [strRef, sourceRefs] of this.sourceReferences.unknownReferences) {
                const possibleTokens = this.constantsOrVariables.get(strRef);
                if (possibleTokens !== undefined) {
                    let ttype: COBOLTokenStyle = COBOLTokenStyle.Variable;
                    let addReference = true;
                    for (const token of possibleTokens) {
                        if (token.ignoreInOutlineView) {
                            addReference = false;
                        } else {
                            ttype = (token.tokenType === COBOLTokenStyle.Unknown) ? ttype : token.tokenType;
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
            // console.log(`DEBUG: unprocessed : ${unknown.length}, p=${pcount},v=${vcount},s=${scount}`);
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
        this.diagWarnings.clear();
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
        return !foundSectionToken.ignoreInOutlineView;
    }

    private isVisibleParagraph(paragrapName: string): boolean {
        const foundParagraph = this.paragraphs.get(paragrapName);
        if (foundParagraph === undefined) {
            return false;
        }
        return !foundParagraph.ignoreInOutlineView;
    }

    private transferReference(symbol: string, symbolRefs: SourceReference[], transferReferenceMap: Map<string, SourceReference[]>, tokenStyle: COBOLTokenStyle): void {
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

    public getCurrentDivision(): string {
        return this.sourceReferences.state.currentDivision === undefined ? "" : this.sourceReferences.state.currentDivision.tokenName;
    }

    public getCurrentSection(): string {
        return this.sourceReferences.state.currentSection === undefined ? "" : this.sourceReferences.state.currentSection.tokenName;
    }

    public getCopyFilename(copybook: string, inInfo: string): string {
        const trimmedCopyBook = copybook.trim();

        return this.externalFeatures.expandLogicalCopyBookToFilenameOrEmpty(trimmedCopyBook, inInfo, this.configHandler);
    }

    private tokensInOrderPush(token: COBOLToken, sendEvent: boolean): void {
        token.tokenOffset = this.tokensInOrder.length;
        this.tokensInOrder.push(token);
        if (sendEvent) {
            this.eventHandler.processToken(token);
        }
    }

    private newCOBOLToken(tokenType: COBOLTokenStyle, startLine: number, _line: string, currentCol: number, token: string,
        description: string, parentToken: COBOLToken | undefined,
        extraInformation1 = ""): COBOLToken {

        const state: ParseState = this.sourceReferences.state;
        let startColumn = _line.indexOf(token, currentCol);
        if (startColumn === -1) {
            startColumn = _line.indexOf(token);
            if (startColumn === -1) {
                startColumn = 0;
            }
        }
        const ctoken = new COBOLToken(
            this.sourceHandler.getUriAsString(), this.filename, tokenType, startLine, startColumn, token, description, parentToken, state.inProcedureDivision, extraInformation1);
        ctoken.ignoreInOutlineView = state.ignoreInOutlineView;
        ctoken.inSection = this.sourceReferences.state.currentSection;

        if (ctoken.ignoreInOutlineView || tokenType === COBOLTokenStyle.ImplicitProgramId) {
            this.tokensInOrderPush(ctoken, true);
            return ctoken;
        }

        /* if we are in a paragraph update */
        if (state.currentParagraph !== undefined) {
            state.currentParagraph.endLine = startLine;
            if (ctoken.startColumn !== 0) {
                state.currentParagraph.endColumn = ctoken.startColumn - 1;
            }
        }

        // new division
        if (tokenType === COBOLTokenStyle.Division && state.currentDivision !== undefined) {

            state.currentParagraph = undefined;
            state.currentDivision.endLine = startLine;
            // state.currentDivision.endLine = parentToken !== undefined ? parentToken.endLine : startLine;
            if (ctoken.startColumn !== 0) {
                state.currentDivision.endColumn = ctoken.startColumn - 1;
            }

            if (state.currentSection !== undefined) {
                state.currentSection.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    state.currentSection.endColumn = ctoken.startColumn - 1;
                }
                state.currentSection = undefined;
            }
            this.tokensInOrder.push(ctoken);
            this.eventHandler.processToken(ctoken);

            return ctoken;
        }

        // new section
        if (tokenType === COBOLTokenStyle.Section) {
            if (state.currentSection !== undefined) {
                state.currentParagraph = undefined;
                state.currentSection.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    state.currentSection.endColumn = ctoken.startColumn - 1;
                }
            }
            this.tokensInOrderPush(ctoken, state.inProcedureDivision);

            return ctoken;
        }

        // new paragraph
        if (tokenType === COBOLTokenStyle.Paragraph) {
            if (state.currentSection !== undefined) {
                state.currentSection.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    state.currentSection.endColumn = ctoken.startColumn - 1;
                }
            }

            state.currentParagraph = ctoken;

            if (state.currentDivision !== undefined) {
                state.currentDivision.endLine = startLine;
                if (ctoken.startColumn !== 0) {
                    state.currentDivision.endColumn = ctoken.startColumn - 1;
                }
            }
            this.tokensInOrderPush(ctoken, true);
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

    public static trimLiteral(literal: string, trimQuotes = false): string {
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

    private relaxedParseLineByLine(prevToken: Token, line: string, state: PreParseState): Token {
        const token = new Token(line, prevToken);
        let tokenCountPerLine = 0;
        do {
            try {
                const endsWithDot = token.endsWithDot;
                let tcurrent: string = token.currentToken;
                let tcurrentLower: string = token.currentTokenLower;
                tokenCountPerLine++;

                if (endsWithDot) {
                    tcurrent = tcurrent.substring(0, tcurrent.length - 1);
                    tcurrentLower = tcurrentLower.substring(0, tcurrentLower.length - 1);
                }

                if (tokenCountPerLine === 1) {
                    const tokenAsNumber = Number.parseInt(tcurrent, 10);
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
                this.externalFeatures.logException("COBOLScannner relaxedParseLineByLine line error: ", e as Error);
            }
        }
        while (token.moveToNextToken() === false);

        return token;
    }

    private addReference(referencesMap: Map<string, SourceReference[]>, lowerCaseVariable: string, line: number, column: number, tokenStyle: COBOLTokenStyle) {
        const lowerCaseVariableRefs = referencesMap.get(lowerCaseVariable);
        if (lowerCaseVariableRefs !== undefined) {
            lowerCaseVariableRefs.push(new SourceReference(this.sourceFileId, line, column, lowerCaseVariable.length, tokenStyle));
            return;
        }

        const sourceRefs: SourceReference[] = [];
        sourceRefs.push(new SourceReference(this.sourceFileId, line, column, lowerCaseVariable.length, tokenStyle));
        referencesMap.set(lowerCaseVariable, sourceRefs);
    }

    private addVariableOrConstant(lowerCaseVariable: string, cobolToken: COBOLToken) {
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

    private parseLineByLine(lineNumber: number, prevToken: Token, line: string): Token {
        const token = new Token(line, prevToken);

        return this.processToken(lineNumber, token, line, this.sourceReferences.state.replaceMap.size !== 0);
    }

    private processToken(lineNumber: number, token: Token, line: string, replaceOn: boolean): Token {
        const state: ParseState = this.sourceReferences.state;

        do {
            try {
                // console.log(`DEBUG: ${line}`);
                let tcurrent: string = token.currentToken;
                // continue now
                if (tcurrent.length === 0) {
                    continue;
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
                            const newToken = new Token(rightLine as string, new Token(token.prevToken, undefined));
                            const retToken = this.processToken(lineNumber, newToken, rightLine, false);

                            // ensure any new token match the original soure
                            if (lastTokenId !== this.tokensInOrder.length) {
                                for (let ltid = lastTokenId; ltid < this.tokensInOrder.length; ltid++) {
                                    const addedToken = this.tokensInOrder[ltid];
                                    addedToken.startColumn = token.currentCol;
                                    addedToken.endColumn = token.currentCol + tcurrent.length;
                                }
                            }
                            return retToken;
                        } catch (e) {
                            this.externalFeatures.logException("replace", e as Error);
                        }
                    }
                }

                let tcurrentLower: string = token.currentTokenLower;

                // HACK for "set x to entry"
                if (token.prevTokenLower === "to" && tcurrentLower === "entry") {
                    token.moveToNextToken();
                    continue;
                }

                if (tcurrent.endsWith(",")) {
                    tcurrent = tcurrent.substring(0, tcurrent.length - 1);
                    tcurrentLower = tcurrent.toLowerCase();
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = false;
                } else if (token.endsWithDot) {
                    tcurrent = tcurrent.substring(0, tcurrent.length - 1);
                    tcurrentLower = tcurrent.toLowerCase();
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = true;
                } else {
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = false;
                }

                // if pickUpUsing
                if (state.pickUpUsing) {
                    if (state.endsWithDot) {
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
                            if (state.using === UsingState.UNKNOWN) {
                                break;
                            }
                            if (this.sourceReferences !== undefined) {
                                if (tcurrentLower === "any") {
                                    state.parameters.push(new COBOLParameter(state.using, tcurrent));
                                } else if ((this.isValidKeyword(tcurrentLower) === false) && (this.isNumber(tcurrentLower) === false)) {
                                    // no forward validation can be done, as this is a one pass scanner
                                    this.addReference(this.sourceReferences.unknownReferences, tcurrentLower, lineNumber, token.currentCol, COBOLTokenStyle.Variable);
                                    state.parameters.push(new COBOLParameter(state.using, tcurrent));
                                }
                            }

                            if (state.using === UsingState.RETURNING) {
                                state.using = UsingState.UNKNOWN;
                            }
                        // logMessage(`INFO: using parameter : ${tcurrent}`);
                    }
                    if (state.endsWithDot) {
                        state.currentProgramTarget.CallParameters = state.parameters;
                    }

                    continue;
                }

                const tcurrentCurrentCol = token.currentCol;

                // if skipToDot and not the end of the statement.. swallow
                if (state.skipToDot) {
                    if (state.addReferencesDuringSkipToTag) {
                        const trimTokenLower = COBOLSourceScanner.trimLiteral(tcurrentLower);
                        const isValidKeyword = this.isValidKeyword(trimTokenLower);

                        if (this.sourceReferences !== undefined) {
                            if (COBOLSourceScanner.isValidLiteral(trimTokenLower) && !this.isNumber(trimTokenLower) && isValidKeyword === false) {
                                // no forward validation can be done, as this is a one pass scanner
                                this.addReference(this.sourceReferences.unknownReferences, trimTokenLower, lineNumber, token.currentCol, COBOLTokenStyle.Unknown);
                            }
                        }

                        // turn off at keyword
                        if (isValidKeyword) {
                            state.addVariableDuringStipToTag = false;
                        }

                        // if we are in a to.. or indexed
                        if (token.prevTokenLower === "to") {
                            state.addVariableDuringStipToTag = false;
                        }

                        if (token.prevTokenLower === "indexed" && token.currentTokenLower === "by") {
                            state.addVariableDuringStipToTag = true;
                        }

                        if (token.prevTokenLower === "depending" && token.currentTokenLower === "on") {
                            state.addVariableDuringStipToTag = false;
                        }

                        if (state.addVariableDuringStipToTag === false && this.sourceReferences.constantsOrVariablesReferences.has(trimTokenLower)) {
                            state.addVariableDuringStipToTag = true;
                        }

                        if (state.addVariableDuringStipToTag && isValidKeyword === false) {
                            if (COBOLSourceScanner.isValidLiteral(trimTokenLower) && !this.isNumber(trimTokenLower)) {
                                const trimToken = COBOLSourceScanner.trimLiteral(tcurrent);
                                const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, tcurrentCurrentCol, trimToken, trimToken, state.currentDivision, token.prevToken);
                                this.addVariableOrConstant(trimTokenLower, variableToken);
                            }
                        }
                    }

                    if (state.inReplace) {
                        switch (tcurrentLower) {
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
                                    state.replaceLeft += tcurrent;
                                } else {
                                    if (state.replaceRight.length !== 0) {
                                        state.replaceRight += " ";
                                    }
                                    state.replaceRight += tcurrent;
                                }
                                if (!state.captureReplaceLeft && tcurrent.endsWith("==")) {
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
                        switch (tcurrentLower) {
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
                                    cbState.literal2 = tcurrent;
                                    break;
                                }
                                if (cbState.isOf && cbState.library_name.length === 0) {
                                    cbState.library_name = tcurrent;
                                    break;
                                }
                                if (cbState.isReplacing && cbState.replaceLeft.length === 0) {
                                    cbState.replaceLeft = tcurrent;
                                    break;
                                }
                                if (cbState.isReplacingBy) {
                                    if (this.configHandler.enable_text_replacement) {
                                        cbState.copyReplaceMap.set(
                                            this.cleanupReplaceToken("" + tcurrent, cbState),
                                            new replaceToken(this.cleanupReplaceToken("" + cbState.replaceLeft, cbState), cbState));
                                    }
                                    cbState.isReplacingBy = false;
                                    cbState.isReplacing = true;
                                    cbState.isLeading = false;
                                    cbState.isTrailing = false;
                                    cbState.replaceLeft = "";
                                    break;
                                }
                                if (tcurrentLower.length > 0 && !cbState.isOf && !cbState.isIn && !cbState.isReplacing) {
                                    cbState.copyBook = tcurrent;
                                    cbState.trimmedCopyBook = COBOLSourceScanner.trimLiteral(tcurrent);
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

                const current: string = tcurrent;
                const currentLower: string = tcurrentLower;
                // const nextToken = token.nextToken;
                // const nextTokenLower = token.nextTokenLower;
                const prevToken = COBOLSourceScanner.trimLiteral(token.prevToken);
                const prevTokenLowerUntrimmed = token.prevTokenLower.trim();
                const prevTokenLower = COBOLSourceScanner.trimLiteral(prevTokenLowerUntrimmed);

                const prevPlusCurrent = token.prevToken + " " + current;

                if (currentLower === "exec") {
                    const nextSTokenOrBlank = token.nextSTokenOrBlank().currentToken;
                    this.currentExec = nextSTokenOrBlank;
                    this.currentExecVerb = "";
                    state.currentToken = this.newCOBOLToken(COBOLTokenStyle.Exec, lineNumber, line, 0, nextSTokenOrBlank, `EXEC ${nextSTokenOrBlank}`, state.currentDivision);
                    token.moveToNextToken();
                    continue;
                }

                /* finish processing end-exec */
                if (currentLower === "end-exec") {
                    this.currentExec = "";
                    this.currentExecVerb = "";
                    state.currentToken = undefined;
                    state.prevEndsWithDot = state.endsWithDot;
                    state.endsWithDot = true;
                    token.endsWithDot = state.endsWithDot;
                    continue;
                }

                /* skip everything in between exec .. end-exec */
                if (state.currentToken !== undefined && this.currentExec.length !== 0) {
                    if (currentLower === "include" && this.currentExec.toLowerCase() === "sql") {
                        const sqlCopyBook = token.nextSTokenOrBlank().currentToken;
                        const trimmedCopyBook = COBOLSourceScanner.trimLiteral(sqlCopyBook);
                        let insertInSection = this.copybookNestedInSection ? state.currentSection : state.currentDivision;
                        if (insertInSection === undefined) {
                            insertInSection = state.currentDivision;
                        }
                        this.tokensInOrder.pop();
                        const copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, tcurrentCurrentCol, trimmedCopyBook, "SQL INCLUDE " + sqlCopyBook, insertInSection);
                        if (this.copyBooksUsed.has(trimmedCopyBook) === false) {
                            const cbInfo = new copybookState();
                            cbInfo.trimmedCopyBook = trimmedCopyBook;
                            cbInfo.copyBook = sqlCopyBook;
                            cbInfo.line = line;
                            cbInfo.startLineNumber = lineNumber;
                            cbInfo.endLineNumber = lineNumber;
                            cbInfo.startCol = tcurrentCurrentCol;
                            cbInfo.endCol = line.indexOf(sqlCopyBook) + sqlCopyBook.length;
                            const fileName = this.externalFeatures.expandLogicalCopyBookToFilenameOrEmpty(trimmedCopyBook, copyToken.extraInformation1, this.configHandler);
                            cbInfo.fileName = fileName;
                            const copybookToken = new COBOLCopybookToken(copyToken, false, cbInfo);
                            this.copyBooksUsed.set(trimmedCopyBook, copybookToken);
                        }
                        state.currentToken = copyToken;
                        continue;
                    }

                    // tweak exec to include verb
                    if (this.currentExecVerb.length == 0) {
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
                            if (varToken.ignoreInOutlineView === false) {
                                ctype = (varToken.tokenType === COBOLTokenStyle.Unknown) ? ctype : varToken.tokenType;
                            } else {
                                addReference = false;
                            }
                        }
                        if (addReference) {
                            this.addReference(this.sourceReferences.constantsOrVariablesReferences, currentLower, lineNumber, token.currentCol, ctype);
                        }
                    }
                    continue;
                }

                if (state.declaratives !== undefined && prevTokenLower === "end" && currentLower === "declaratives") {
                    state.declaratives.endLine = lineNumber;
                    state.declaratives.endColumn = line.indexOf(tcurrent);
                    state.inDeclaratives = false;
                    state.declaratives = undefined;
                    state.declaratives = this.newCOBOLToken(COBOLTokenStyle.EndDeclaratives, lineNumber, line, tcurrentCurrentCol, current, current, state.currentDivision);

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
                                const trimmedCurrent = COBOLSourceScanner.trimLiteral(this.ImplicitProgramId);
                                const ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, "program-id. " + this.ImplicitProgramId, 0, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                                state.programs.push(ctoken);
                                ctoken.ignoreInOutlineView = true;
                                this.ImplicitProgramId = "";        /* don't need it */
                            }

                            state.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, "Data Division", 0, "Data", "Data Division (Optional)", state.currentDivision);
                            state.currentDivision.ignoreInOutlineView = true;
                        }
                    }

                    if (prevTokenLower === "working-storage" || prevTokenLower === "linkage" ||
                        prevTokenLower === "local-storage" || prevTokenLower === "file-control" ||
                        prevTokenLower === "file" || prevTokenLower === "screen") {
                        state.pickFields = true;
                        state.inProcedureDivision = false;
                    }

                    state.currentSection = this.newCOBOLToken(COBOLTokenStyle.Section, lineNumber, line, 0, prevToken, prevPlusCurrent, state.currentDivision);
                    this.sections.set(prevTokenLower, state.currentSection);
                    state.current01Group = undefined;
                    state.currentLevel = undefined;

                    continue;
                }

                // handle divisions
                if (state.captureDivisions && prevTokenLower.length !== 0 && currentLower === "division") {
                    state.currentDivision = this.newCOBOLToken(COBOLTokenStyle.Division, lineNumber, line, 0, prevToken, prevPlusCurrent, undefined);

                    if (prevTokenLower === "procedure") {
                        state.inProcedureDivision = true;
                        state.pickFields = false;
                        state.procedureDivision = state.currentDivision;
                        if (state.endsWithDot === false) {
                            state.pickUpUsing = true;
                        }
                    }

                    continue;
                }

                // handle entries
                if (prevTokenLowerUntrimmed === "entry") {
                    let entryStatement = prevPlusCurrent;
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current);
                    const nextSTokenOrBlank = token.nextSTokenOrBlank().currentToken;
                    if (nextSTokenOrBlank === "&") {
                        entryStatement = prevToken + " " + token.compoundItems(trimmedCurrent, this);
                    }
                    const ctoken = this.newCOBOLToken(COBOLTokenStyle.EntryPoint, lineNumber, line, tcurrentCurrentCol, trimmedCurrent, entryStatement, state.currentDivision);

                    state.entryPointCount++;
                    state.parameters = [];
                    state.currentProgramTarget = new CallTargetInformation(current, ctoken, true, []);
                    this.callTargets.set(trimmedCurrent, state.currentProgramTarget);
                    state.pickUpUsing = true;
                    continue;
                }

                // handle program-id
                if (prevTokenLower === "program-id") {
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current);
                    const ctoken = this.newCOBOLToken(COBOLTokenStyle.ProgramId, lineNumber, line, tcurrentCurrentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                    state.programs.push(ctoken);
                    if (state.currentDivision !== undefined) {
                        state.currentDivision.endLine = ctoken.endLine;
                        state.currentDivision.endColumn = ctoken.endColumn;
                    }
                    if (trimmedCurrent.indexOf(" ") === -1 && token.isTokenPresent("external") === false) {
                        state.parameters = [];
                        state.currentProgramTarget = new CallTargetInformation(current, ctoken, false, []);
                        this.callTargets.set(trimmedCurrent, state.currentProgramTarget);
                    }
                    this.ImplicitProgramId = "";        /* don't need it */
                    continue;
                }

                // handle class-id
                if (prevTokenLower === "class-id") {
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current);
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.ClassId, lineNumber, line, tcurrentCurrentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    state.pickFields = true;
                    this.classes.set(trimmedCurrent, state.currentClass);

                    continue;
                }

                // handle "end class, enum, valuetype"
                if (state.currentClass !== undefined && prevTokenLower === "end" &&
                    (currentLower === "class" || currentLower === "enum" || currentLower === "valuetype" || currentLower === "interface")) {
                    state.currentClass.endLine = lineNumber;
                    state.currentClass.endColumn = line.toLowerCase().indexOf(currentLower) + currentLower.length;

                    state.currentClass = undefined;
                    state.captureDivisions = true;
                    state.currentMethod = undefined;
                    state.pickFields = false;
                    state.inProcedureDivision = false;
                    continue;
                }

                // handle enum-id
                if (prevTokenLower === "enum-id") {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.EnumId, lineNumber, line, tcurrentCurrentCol, COBOLSourceScanner.trimLiteral(current), prevPlusCurrent, undefined);

                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    state.pickFields = true;
                    continue;
                }

                // handle interface-id
                if (prevTokenLower === "interface-id") {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.InterfaceId, lineNumber, line, tcurrentCurrentCol, COBOLSourceScanner.trimLiteral(current), prevPlusCurrent, state.currentDivision);

                    state.pickFields = true;
                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    continue;
                }

                // handle valuetype-id
                if (prevTokenLower === "valuetype-id") {
                    state.currentClass = this.newCOBOLToken(COBOLTokenStyle.ValueTypeId, lineNumber, line, tcurrentCurrentCol, COBOLSourceScanner.trimLiteral(current), prevPlusCurrent, state.currentDivision);

                    state.pickFields = true;
                    state.captureDivisions = false;
                    state.currentMethod = undefined;
                    continue;
                }

                // handle function-id
                if (prevTokenLower === "function-id") {
                    const trimmedCurrent = COBOLSourceScanner.trimLiteral(current);
                    state.currentFunctionId = this.newCOBOLToken(COBOLTokenStyle.FunctionId, lineNumber, line, tcurrentCurrentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                    state.captureDivisions = true;
                    state.pickFields = true;
                    state.parameters = [];
                    state.currentProgramTarget = new CallTargetInformation(current, state.currentFunctionId, false, []);
                    this.functionTargets.set(trimmedCurrent, state.currentProgramTarget);

                    continue;
                }

                // handle method-id
                if (prevTokenLower === "method-id") {
                    const currentLowerTrim = COBOLSourceScanner.trimLiteral(currentLower);
                    const style = currentLowerTrim === "new" ? COBOLTokenStyle.Constructor : COBOLTokenStyle.MethodId;
                    const nextTokenLower = token.nextSTokenOrBlank().currentTokenLower;
                    const nextToken = token.nextSTokenOrBlank().currentToken;

                    if (nextTokenLower === "property") {
                        const nextPlusOneToken = token.nextSTokenIndex(2).currentToken;
                        const trimmedProperty = COBOLSourceScanner.trimLiteral(nextPlusOneToken);
                        state.currentMethod = this.newCOBOLToken(COBOLTokenStyle.Property, lineNumber, line, tcurrentCurrentCol, trimmedProperty, nextToken + " " + nextPlusOneToken, state.currentDivision);
                        this.methods.set(trimmedProperty, state.currentMethod);
                    } else {
                        const trimmedCurrent = COBOLSourceScanner.trimLiteral(current);
                        state.currentMethod = this.newCOBOLToken(style, lineNumber, line, tcurrentCurrentCol, trimmedCurrent, prevPlusCurrent, state.currentDivision);
                        this.methods.set(trimmedCurrent, state.currentMethod);
                    }

                    state.pickFields = true;
                    state.captureDivisions = false;
                    continue;
                }

                // handle "end method"
                if (state.currentMethod !== undefined && prevTokenLower === "end" && currentLower === "method") {
                    state.currentMethod.endLine = lineNumber;
                    state.currentMethod.endColumn = line.toLowerCase().indexOf(currentLower) + currentLower.length;

                    state.currentMethod = undefined;
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

                    if (state.currentDivision !== undefined) {
                        state.currentDivision.endLine = lineNumber;
                        state.currentDivision.endColumn = line.length;
                    }

                    if (state.currentSection !== undefined) {
                        state.currentSection.endLine = lineNumber;
                        state.currentSection.endColumn = line.length;
                    }

                    if (state.currentParagraph !== undefined) {
                        state.currentParagraph.endLine = lineNumber;
                        state.currentParagraph.endColumn = line.length;
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
                    state.currentFunctionId.endLine = lineNumber;
                    state.currentFunctionId.endColumn = line.toLowerCase().indexOf(currentLower) + currentLower.length;
                    this.newCOBOLToken(COBOLTokenStyle.EndFunctionId, lineNumber, line, tcurrentCurrentCol, prevToken, current, state.currentDivision);

                    state.pickFields = false;
                    state.inProcedureDivision = false;

                    if (state.currentDivision !== undefined) {
                        state.currentDivision.endLine = lineNumber;
                        // state.currentDivision.endColumn = 0;
                    }

                    if (state.currentSection !== undefined) {
                        state.currentSection.endLine = lineNumber;
                        // state.currentSection.endColumn = 0;
                    }
                    state.currentDivision = undefined;
                    state.currentSection = undefined;
                    state.currentParagraph = undefined;
                    state.procedureDivision = undefined;
                    continue;
                }


                if (prevTokenLower !== "end" && currentLower === "declaratives") {
                    state.declaratives = this.newCOBOLToken(COBOLTokenStyle.Declaratives, lineNumber, line, tcurrentCurrentCol, current, current, state.currentDivision);
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
                        if (tcurrent.length !== 0) {
                            if (this.isParagraph(tcurrent)) {
                                if (state.currentSection !== undefined) {
                                    const newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, tcurrentCurrentCol, tcurrent, tcurrent, state.currentSection);
                                    this.paragraphs.set(newToken.tokenNameLower, newToken);
                                } else {
                                    const newToken = this.newCOBOLToken(COBOLTokenStyle.Paragraph, lineNumber, line, tcurrentCurrentCol, tcurrent, tcurrent, state.currentDivision);
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
                        if (prevTokenLower == "cd") {
                            state.pickFields = true;
                        }
                    }
                }


                // are we in the working-storage section?
                if (state.pickFields && prevToken.length > 0) {
                    let tcurrentCurrentCol2 = tcurrentCurrentCol;
                    /* only interesting in things that are after a number */
                    if (this.isNumber(prevToken) && !this.isNumber(current)) {
                        const isFiller: boolean = (currentLower === "filler");
                        let pickUpThisField: boolean = isFiller;
                        let trimToken = COBOLSourceScanner.trimLiteral(current);

                        // what other reasons do we need to pickup this line as a field?
                        if (!pickUpThisField) {
                            const compRegEx = /comp-[0-9]/;
                            // not a complete inclusive list but should cover the normal cases
                            if (currentLower === "pic" || currentLower === "picture" || compRegEx.test(currentLower) || currentLower.startsWith("binary-")) {
                                // fake up the line
                                line = prevToken + " filler ";
                                trimToken = "filler";
                                pickUpThisField = true;
                                tcurrentCurrentCol2 = 0;
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

                                const ctoken = this.newCOBOLToken(style, lineNumber, line, tcurrentCurrentCol2, trimToken, trimToken, state.currentDivision, extraInfo);
                                if (!isFiller) {
                                    this.addVariableOrConstant(currentLower, ctoken);
                                }

                                // place the 88 under the 01 item
                                if (state.currentLevel !== undefined && prevToken === "88") {
                                    state.currentLevel.endLine = ctoken.startLine;
                                    state.currentLevel.endColumn = ctoken.startColumn + ctoken.tokenName.length;
                                }

                                if (prevToken !== "88") {
                                    state.currentLevel = ctoken;
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
                                    state.current01Group.endLine = ctoken.startLine;
                                    state.current01Group.endColumn = ctoken.startColumn + ctoken.tokenName.length;
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
                        const trimToken = COBOLSourceScanner.trimLiteral(current);

                        if (COBOLSourceScanner.isValidLiteral(currentLower)) {
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, tcurrentCurrentCol, trimToken, trimToken, state.currentDivision, prevTokenLower);
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
                            const trimToken = COBOLSourceScanner.trimLiteral(current);
                            const variableToken = this.newCOBOLToken(COBOLTokenStyle.Variable, lineNumber, line, tcurrentCurrentCol, trimToken, trimToken, state.currentDivision, prevTokenLower);
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

                                this.addReference(sharedReferences, currentLower, lineNumber, token.currentCol, sourceStyle);
                                continue;
                            }

                            /* is this a reference to a variable? */
                            const varTokens = this.constantsOrVariables.get(currentLower);
                            if (varTokens !== undefined) {
                                let ctype: COBOLTokenStyle = COBOLTokenStyle.Variable;
                                let addReference = true;
                                for (const varToken of varTokens) {
                                    if (varToken.ignoreInOutlineView === false) {
                                        ctype = (varToken.tokenType === COBOLTokenStyle.Unknown) ? ctype : varToken.tokenType;
                                    } else {
                                        addReference = false;
                                    }
                                }
                                if (addReference) {
                                    this.addReference(this.sourceReferences.constantsOrVariablesReferences, currentLower, lineNumber, token.currentCol, ctype);
                                }
                            } else {
                                let sourceStyle = COBOLTokenStyle.Unknown;
                                let sharedReferences = this.sourceReferences.unknownReferences;
                                if (this.isVisibleSection(currentLower)) {
                                    sourceStyle = COBOLTokenStyle.Section;
                                    sharedReferences = this.sourceReferences.targetReferences;
                                } else if (this.isVisibleParagraph(currentLower)) {
                                    sourceStyle = COBOLTokenStyle.Paragraph;
                                    sharedReferences = this.sourceReferences.targetReferences;
                                }
                                this.addReference(sharedReferences, currentLower, lineNumber, token.currentCol, sourceStyle);
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
                                    if (varToken.ignoreInOutlineView === false) {
                                        ctype = (varToken.tokenType === COBOLTokenStyle.Unknown) ? ctype : varToken.tokenType;
                                    } else {
                                        addReference = false;
                                    }
                                }
                                if (addReference) {
                                    this.addReference(this.sourceReferences.constantsOrVariablesReferences, trimmedCurrentLower, lineNumber, token.currentCol + pos, ctype);
                                }
                            } else {
                                this.addReference(this.sourceReferences.unknownReferences, trimmedCurrentLower, lineNumber, token.currentCol + pos, COBOLTokenStyle.Unknown);
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
            const library_name_or_lit = COBOLSourceScanner.trimLiteral(cbInfo.library_name) + COBOLSourceScanner.trimLiteral(cbInfo.literal2);
            const desc: string = copyVerb + " " + copyBook + middleDesc + library_name_or_lit;
            // trim...
            copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBookInOrOf, lineNumber, line, tcurrentCurrentCol, trimmedCopyBook, desc, insertInSection, library_name_or_lit);
        }
        else {
            copyToken = this.newCOBOLToken(COBOLTokenStyle.CopyBook, lineNumber, line, tcurrentCurrentCol, trimmedCopyBook, copyVerb + " " + copyBook, insertInSection);
        }

        copyToken.endLine = cbInfo.endLineNumber;
        copyToken.endColumn = cbInfo.endCol;

        state.inCopy = false;

        if (this.copyBooksUsed.has(trimmedCopyBook) === false) {
            const copybookToken = new COBOLCopybookToken(copyToken, false, cbInfo);
            this.copyBooksUsed.set(trimmedCopyBook, copybookToken);
            const fileName = this.externalFeatures.expandLogicalCopyBookToFilenameOrEmpty(trimmedCopyBook, copyToken.extraInformation1, this.configHandler);
            if (fileName.length > 0) {
                cbInfo.fileName = fileName;
            }

            if (this.sourceReferences !== undefined && this.parse_copybooks_for_references) {
                if (fileName.length > 0) {
                    cbInfo.fileName = fileName;
                    if (this.copyBooksUsed.has(fileName) === false) {

                        // move the source version of the copybook
                        this.copyBooksUsed.delete(trimmedCopyBook);

                        // add the specific version
                        this.copyBooksUsed.set(fileName, copybookToken);
                        const qfile = new FileSourceHandler(fileName, this.externalFeatures);
                        const currentTopLevel = this.sourceReferences.topLevel;
                        const currentIgnoreInOutlineView: boolean = state.ignoreInOutlineView;
                        state.ignoreInOutlineView = true;
                        this.sourceReferences.topLevel = false;
                        const prevRepMap = this.sourceReferences.state.replaceMap;

                        if (this.configHandler.enable_text_replacement) {
                            this.sourceReferences.state.replaceMap = new Map<string, replaceToken>([...cbInfo.copyReplaceMap, ...prevRepMap]);
                        }
                        // eslint-disable-next-line @typescript-eslint/no-unused-vars
                        const qps = COBOLSourceScanner.ScanUncachedInlineCopybook(qfile, this, this.parse_copybooks_for_references, this.eventHandler, this.externalFeatures);
                        cbInfo.sourceHandler = qps.sourceHandler;
                        this.sourceReferences.state.replaceMap = prevRepMap;
                        this.sourceReferences.topLevel = currentTopLevel;
                        state.ignoreInOutlineView = currentIgnoreInOutlineView;

                        copybookToken.scanComplete = true;
                    }
                } else {
                    if (this.configHandler.linter_ignore_missing_copybook === false) {
                        const diagMessage = `Unable to locate copybook ${trimmedCopyBook}`;
                        this.diagWarnings.set(diagMessage, new COBOLFileSymbol(this.filename, copyToken.startLine));
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
                for (const offset in args) {
                    const filenameTrimmed = args[offset].trim();
                    const fileName = this.externalFeatures.expandLogicalCopyBookToFilenameOrEmpty(filenameTrimmed, "", this.configHandler);
                    if (fileName.length > 0) {
                        if (this.copyBooksUsed.has(fileName) === false) {
                            this.copyBooksUsed.set(fileName, COBOLCopybookToken.Null);

                            const qfile = new FileSourceHandler(fileName, this.externalFeatures);
                            const currentIgnoreInOutlineView: boolean = this.sourceReferences.state.ignoreInOutlineView;
                            this.sourceReferences.state.ignoreInOutlineView = true;
                            this.sourceReferences.topLevel = true;

                            // eslint-disable-next-line @typescript-eslint/no-unused-vars
                            COBOLSourceScanner.ScanUncachedInlineCopybook(qfile, this, this.parse_copybooks_for_references, this.eventHandler, this.externalFeatures);
                            this.sourceReferences.topLevel = true;
                            this.sourceReferences.state.ignoreInOutlineView = currentIgnoreInOutlineView;
                        }
                    } else {
                        if (this.configHandler.linter_ignore_missing_copybook === false) {
                            const diagMessage = `${startOfTokenFor}: Unable to locate copybook ${filenameTrimmed} specified in embedded comment`;
                            this.diagWarnings.set(diagMessage, new COBOLFileSymbol(sourceFilename, sourceLineNumber));
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
            if (commandTrimmed === CobolLinterProviderSymbols.NotReferencedMarker_external) {
                for (const offset in args) {
                    this.sourceReferences.ignoreUnusedSymbol.set(args[offset].toLowerCase(), args[offset]);
                }
            }
        }
    }

    public processComment(sourceHandler: ISourceHandlerLite, commentLine: string, sourceFilename: string, sourceLineNumber: number, startPos: number, format: ESourceFormat): void {
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

    }
}
