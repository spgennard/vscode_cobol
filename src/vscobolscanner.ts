import { VSCodeSourceHandler } from "./vscodesourcehandler";
import { TextDocument, Uri, debug } from "vscode";
import { COBOLSourceScanner, COBOLToken, COBOLTokenStyle, EmptyCOBOLSourceScannerEventHandler, ICOBOLSourceScanner, ICOBOLSourceScannerEvents, SharedSourceReferences } from "./cobolsourcescanner";
import { InMemoryGlobalCacheHelper, InMemoryGlobalSymbolCache } from "./globalcachehelper";

import { VSLogger } from "./vslogger";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLSymbolTable } from "./cobolglobalcache";
import { COBOLUtils } from "./cobolutils";
import { ScanStats } from "./cobscannerdata";
import { COBOLWorkspaceSymbolCacheHelper, TypeCategory } from "./cobolworkspacecache";
import { VSPreProc } from "./vspreproc";

import { VSExternalFeatures } from "./vsexternalfeatures";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const InMemoryCache: Map<string, COBOLSourceScanner> = new Map<string, COBOLSourceScanner>();

export function clearCOBOLCache(): void {
    InMemoryCache.clear();
    InMemoryGlobalSymbolCache.isDirty = false;
}

export class VSScanStats extends ScanStats {
    directoriesScannedMap: Map<string, Uri> = new Map<string, Uri>();
}

export class COBOLSymbolTableGlobalEventHelper implements ICOBOLSourceScannerEvents {
    private qp: ICOBOLSourceScanner | undefined;
    private st: COBOLSymbolTable | undefined;
    private config: ICOBOLSettings;

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public constructor(config: ICOBOLSettings) {
        this.config = config;
    }

    public start(qp: ICOBOLSourceScanner): void {
        this.qp = qp;
        this.st = new COBOLSymbolTable();
        this.st.fileName = qp.filename;
        this.st.lastModifiedTime = qp.lastModifiedTime;

        if (this.st?.fileName !== undefined && this.st.lastModifiedTime !== undefined) {
            COBOLWorkspaceSymbolCacheHelper.removeAllCopybookReferences(this.st?.fileName);
            COBOLWorkspaceSymbolCacheHelper.removeAllPrograms(this.st?.fileName);
            COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(this.st?.fileName);
            COBOLWorkspaceSymbolCacheHelper.removeAllTypes(this.st?.fileName);
            InMemoryGlobalCacheHelper.addFilename(this.st?.fileName, qp.workspaceFile);
        }
    }

    public processToken(token: COBOLToken): void {
        // a hide token should not be placed in the symbol table, as they from a different file
        if (token.ignoreInOutlineView) {
            return;
        }

        if (this.st === undefined) {
            return;
        }

        switch (token.tokenType) {
            case COBOLTokenStyle.CopyBook:
                COBOLWorkspaceSymbolCacheHelper.addReferencedCopybook(token.tokenName, this.st.fileName);
                break;
            case COBOLTokenStyle.CopyBookInOrOf:
                COBOLWorkspaceSymbolCacheHelper.addReferencedCopybook(token.tokenName, this.st.fileName);
                break;
            case COBOLTokenStyle.File:
                COBOLWorkspaceSymbolCacheHelper.addCalableSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.ImplicitProgramId:
                COBOLWorkspaceSymbolCacheHelper.addCalableSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.ProgramId:
                COBOLWorkspaceSymbolCacheHelper.addCalableSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.EntryPoint:
                COBOLWorkspaceSymbolCacheHelper.addEntryPoint(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.InterfaceId:
                COBOLWorkspaceSymbolCacheHelper.addClass(this.st.fileName, token.tokenName, token.startLine, TypeCategory.InterfaceId);
                break;
            case COBOLTokenStyle.EnumId:
                COBOLWorkspaceSymbolCacheHelper.addClass(this.st.fileName, token.tokenName, token.startLine, TypeCategory.EnumId);
                break;
            case COBOLTokenStyle.ClassId:
                COBOLWorkspaceSymbolCacheHelper.addClass(this.st.fileName, token.tokenName, token.startLine, TypeCategory.ClassId);
                break;
            case COBOLTokenStyle.MethodId:
                // GlobalCachesHelper.addMethodSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
        }
    }

    public finish(): void {
        COBOLUtils.saveGlobalCacheToWorkspace(this.config);
    }
}

export class VSCOBOLSourceScanner {
    private static readonly MAX_MEM_CACHE_SIZE = 30;

    public static removeCachedObject(document: TextDocument, config: ICOBOLSettings): void {
        if (config.enable_source_scanner === false) {
            return undefined;
        }

        const fileName: string = document.fileName;
        const cachedObject = InMemoryCache.get(fileName);
        if (cachedObject !== undefined) {
            InMemoryCache.delete(fileName);
        }
    }

    public static getCachedObject(document: TextDocument, config: ICOBOLSettings): COBOLSourceScanner | undefined {
        if (config.enable_source_scanner === false) {
            return undefined;
        }

        // file is too large to parse
        if (document.lineCount > config.editor_maxTokenizationLineLength) {
            VSLogger.logMessage(` ${document.fileName} is not parsed, line count is ${document.lineCount} and editor.maxTokenizationLineLength is ${config.editor_maxTokenizationLineLength}`);
            return undefined;
        }

        const fileName: string = document.fileName;
        let cachedObject = InMemoryCache.get(fileName);
        if (cachedObject !== undefined) {
            if (cachedObject.sourceHandler.getDocumentVersionId() !== BigInt(document.version)) {
                InMemoryCache.delete(fileName);
                cachedObject = undefined;
            }
        }

        // in memory document is out of sync with the on-disk document, so reparsing it
        // will give in-consistent results, especially with the debugg
        if (document.isDirty && debug.activeDebugSession !== undefined) {
            VSLogger.logMessage("Source code has changed during debugging, in memory scanning suspended");
            VSLogger.logMessage(` ID=${debug.activeDebugSession.id}, Name=${debug.activeDebugSession.name}, Type=${debug.activeDebugSession.type}`);
            return undefined;
        }

        /* grab, the file parse it can cache it */
        if (cachedObject === undefined) {
            try {
                // we have to delay the setup of the pp to avoid a race condition on startup.
                //   eg: if the ext depends on this but its exports are queried before it has
                //        completed the activation.. it just does not work
                if (config.preprocessor_extensions.length !== 0) {
                    async () => {
                        if (await VSPreProc.registerPreProcessors(config) === false) {
                            return undefined;
                        }
                    };
                }

                const startTime = VSExternalFeatures.performance_now();
                const sourceHandler = new VSCodeSourceHandler(document, false);
                const cacheData = sourceHandler.getIsSourceInWorkSpace();
                const cacheDirectory = config.get_depreciated_cache_directory();
                const qcpd = new COBOLSourceScanner(sourceHandler, config,
                    cacheDirectory === undefined ? "" : cacheDirectory, new SharedSourceReferences(config, true),
                    config.parse_copybooks_for_references,
                    cacheData ? new COBOLSymbolTableGlobalEventHelper(config) : EmptyCOBOLSourceScannerEventHandler.Default,
                    VSExternalFeatures);

                VSLogger.logTimedMessage(VSExternalFeatures.performance_now() - startTime, " - Parsing " + fileName);

                if (InMemoryCache.size > VSCOBOLSourceScanner.MAX_MEM_CACHE_SIZE) {
                    // drop the smallest..
                    let smallest = Number.MAX_VALUE;
                    let dropKey = "";
                    for (const [key, val] of InMemoryCache) {
                        if (val.tokensInOrder.length < smallest) {
                            dropKey = key;
                            smallest = val.tokensInOrder.length;
                        }
                    }
                    if (dropKey.length !== 0) {
                        InMemoryCache.delete(dropKey);
                    }
                }

                InMemoryCache.set(fileName, qcpd);
                return qcpd;
            }
            catch (e) {
                VSLogger.logException("getCachedObject", e as Error);
            }
        }

        return cachedObject;
    }
}
