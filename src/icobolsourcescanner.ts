import { ISourceHandler, ISourceHandlerLite } from "./isourcehandler";
import { COBOLFileAndColumnSymbol, COBOLFileSymbol, COBOLWorkspaceFile } from "./cobolglobalcache";
import { ICOBOLSettings } from "./iconfiguration";
import { ESourceFormat } from "./externalfeatures";
import { portResult } from "./vsdirectivesconv";
import { CallTargetInformation, COBOLCopybookToken, COBOLToken, COBOLVariable, SharedSourceReferences, SQLDeclare } from "./cobolsourcescanner";


export interface ICOBOLSourceScannerEventer {
    sendMessage(message: string): void;
}

export interface ICOBOLSourceScannerEvents {
    start(qp: ICOBOLSourceScanner): void;
    processToken(token: COBOLToken): void;
    finish(): void;
}

export interface ICOBOLSourceScanner{
    id: string;
    readonly sourceHandler: ISourceHandler;
    filename: string;
    lastModifiedTime: BigInt;
    tokensInOrder: COBOLToken[];
    execTokensInOrder: COBOLToken[];
    readonly execSQLDeclare: Map<string, SQLDeclare>;
    readonly sections: Map<string, COBOLToken>;
    readonly paragraphs: Map<string, COBOLToken>;
    readonly constantsOrVariables: Map<string, COBOLVariable[]>;
    readonly callTargets:  Map<string, CallTargetInformation>;
    readonly functionTargets:  Map<string, CallTargetInformation>;
    readonly classes: Map<string, COBOLToken>;
    readonly methods: Map<string, COBOLToken>;
    readonly copyBooksUsed: Map<string, COBOLCopybookToken>;
    readonly diagMissingFileWarnings: Map<string, COBOLFileSymbol>;
    readonly portWarnings: portResult[];
    readonly generalWarnings: COBOLFileSymbol[];
    readonly commentReferences: COBOLFileAndColumnSymbol[];
    readonly parse4References: boolean;
    readonly sourceReferences: SharedSourceReferences;
    readonly sourceFileId: number;
    cache4PerformTargets: any;
    cache4ConstantsOrVars: any;
    ImplicitProgramId: string;
    ProgramId: string;
    sourceFormat: ESourceFormat;
    sourceIsCopybook: boolean;
    workspaceFile: COBOLWorkspaceFile;
    readonly parse_copybooks_for_references: boolean;
    readonly scan_comments_for_hints: boolean;
    readonly isSourceDepCopyBook: boolean;
    readonly scan_comment_for_ls_control: boolean;
    readonly copybookNestedInSection: boolean;
    readonly configHandler: ICOBOLSettings;
    parseHint_OnOpenFiles: string[];
    parseHint_WorkingStorageFiles: string[];
    parseHint_LocalStorageFiles: string[];
    parseHint_ScreenSectionFiles: string[];
    scanAborted: boolean;
    parseExecStatement(currentExecStype: string, token: COBOLToken, lines: string): void;
    parseSQLDeclareForReferences(fileid: number, _refExecSQLDeclareName: string, refExecToken: COBOLToken, lines: string, sqldeclare: SQLDeclare): void;
    processComment(configHandler: ICOBOLSettings, sourceHandler: ISourceHandlerLite, commentLine: string, sourceFilename: string, sourceLineNumber: number, startPos: number, format: ESourceFormat): void;
    findNearestSectionOrParagraph(line: number): COBOLToken|undefined;
}
