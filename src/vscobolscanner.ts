import { VSCodeSourceHandler } from "./vscodesourcehandler";
import { FileType, TextDocument, Uri, window, workspace } from 'vscode';
import COBOLSourceScanner, { COBOLToken, COBOLTokenStyle, EmptyCOBOLSourceScannerEventHandler, ICOBOLSourceScanner, ICOBOLSourceScannerEvents, SharedSourceReferences } from "./cobolsourcescanner";
import { GlobalCachesHelper } from "./globalcachehelper";

import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import * as crypto from 'crypto';

import { logMessage, logException, logTimedMessage, isDirectory, performance_now, getCurrentContext, logChannelSetPreserveFocus, ExternalFeatures } from "./extension";
import { VSCOBOLConfiguration } from "./configuration";
import { getWorkspaceFolders } from "./cobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLSymbolTableHelper } from "./cobolglobalcache_file";
import { COBOLSymbolTable, InMemoryGlobalSymbolCache } from "./cobolglobalcache";
import { CacheDirectoryStrategy } from "./externalfeatures";
import { COBOLUtils } from "./cobolutils";
import { ScanDataHelper, ScanStats } from "./cobscannerdata";
import { VSCobScanner } from "./vscobscanner";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const InMemoryCache: Map<string, COBOLSourceScanner> = new Map<string, COBOLSourceScanner>();

export function clearCOBOLCache(): void {
    InMemoryCache.clear();
}

export class VSScanStats extends ScanStats {
    directoriesScannedMap: Map<string, Uri> = new Map<string, Uri>();
}

export class COBOLSymbolTableGlobalEventHelper implements ICOBOLSourceScannerEvents {
    private qp: ICOBOLSourceScanner | undefined;
    private st: COBOLSymbolTable | undefined;

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public constructor(config: ICOBOLSettings) {
        //
    }


    public start(qp: ICOBOLSourceScanner): void {
        this.qp = qp;
        this.st = new COBOLSymbolTable();
        this.st.fileName = qp.filename;
        this.st.lastModifiedTime = qp.lastModifiedTime;

        if (this.st?.fileName !== undefined && this.st.lastModifiedTime !== undefined) {
            GlobalCachesHelper.loadGlobalSymbolCache(this.qp.cacheDirectory);
            GlobalCachesHelper.addFilename(this.st?.fileName, this.st?.lastModifiedTime);
        }
    }

    public processToken(token: COBOLToken): void {
        // hident token should not be placed in the symbol table, as they from a different file
        if (token.ignoreInOutlineView) {
            return;
        }

        if (this.st === undefined) {
            return;
        }

        switch (token.tokenType) {
            case COBOLTokenStyle.ImplicitProgramId:
                GlobalCachesHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.ProgramId:
                GlobalCachesHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.EntryPoint:
                GlobalCachesHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.InterfaceId:
                GlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.EnumId:
                GlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.ClassId:
                GlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.MethodId:
                GlobalCachesHelper.addMethodSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
        }
    }

    public finish(): void {
        if (this.st !== undefined && this.qp !== undefined) {
            if (GlobalCachesHelper.isGlobalSymbolCacheLoadRequired(this.qp.cacheDirectory)) {
                //this.features.logMessage(" WARNING: Cache was updated unexpectedly");
            } else {
                COBOLSymbolTableHelper.saveToFile(this.qp.cacheDirectory, this.st);
            }
        }
    }
}

export default class VSCOBOLSourceScanner {

    private static readonly MAX_MEM_CACHE_SIZE = 30;

    public static getCachedObject(document: TextDocument): COBOLSourceScanner | undefined {
        const fileName: string = document.fileName;
        const cacheDirectory: string | undefined = VSCOBOLSourceScanner.getCacheDirectory();

        /* if the document is edited, drop the in cached object */
        if (document.isDirty) {
            InMemoryCache.delete(fileName);
        }

        /* grab, the file parse it can cache it */
        if (InMemoryCache.has(fileName) === false) {
            try {
                const startTime = performance_now();
                const config = VSCOBOLConfiguration.get();

                const qcpd = new COBOLSourceScanner(new VSCodeSourceHandler(document, false), config,
                    cacheDirectory === undefined ? "" : cacheDirectory, new SharedSourceReferences(true),
                    config.parse_copybooks_for_references,
                    config.process_metadata_cache_on_file_save ? new COBOLSymbolTableGlobalEventHelper(config) : EmptyCOBOLSourceScannerEventHandler.Default,
                    ExternalFeatures);

                logTimedMessage(performance_now() - startTime, " - Parsing " + fileName);

                if (InMemoryCache.size > VSCOBOLSourceScanner.MAX_MEM_CACHE_SIZE) {
                    // drop the smallest..
                    let smallest=Number.MAX_VALUE;
                    let dropKey = "";
                    for(const [key,val] of InMemoryCache) {
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
                logException("getCachedObject", e);
            }
        }

        return InMemoryCache.get(fileName);
    }

    public static async checkWorkspaceForMissingCopybookDirs(): Promise<void> {
        logChannelSetPreserveFocus(false);
        logMessage("Checking workspace for folders that are not present in copybookdirs setting");

        const settings = VSCOBOLConfiguration.get();
        const ws = getWorkspaceFolders();
        if (ws !== undefined) {
            for (const folder of ws) {
                try {
                    await VSCOBOLSourceScanner.checkWorkspaceForMissingCopybookDir(settings, folder.uri, folder.uri);
                } catch {
                    continue;       // most likely an invalid directory
                }
            }
        }
        logMessage(" -- Analysis complete");
    }

    public static async checkWorkspaceForMissingCopybookDir(settings: ICOBOLSettings, topLevelFolder: Uri, folder: Uri): Promise<void> {
        const entries = await workspace.fs.readDirectory(folder);

        for (const [entry, fileType] of entries) {
            switch (fileType) {
                case FileType.Directory | FileType.SymbolicLink:
                case FileType.Directory:
                    if (!VSCOBOLSourceScanner.ignoreDirectory(entry)) {
                        const fullDirectory = path.join(folder.fsPath, entry);
                        if (!VSCOBOLSourceScanner.ignoreDirectory(entry)) {
                            const topLevelLength = 1 + topLevelFolder.fsPath.length;
                            const possibleCopydir = fullDirectory.substr(topLevelLength);

                            if (COBOLUtils.inCopybookdirs(settings, possibleCopydir) === false) {
                                logMessage(`  Configuration Recommendation : include : ${possibleCopydir} in coboleditor.copybookdirs to ensure the copybooks in this directory can be found`);
                            }
                            await VSCOBOLSourceScanner.checkWorkspaceForMissingCopybookDir(settings, topLevelFolder, Uri.file(fullDirectory));
                        }
                    }
                    break;
            }
        }
        return;
    }

    public static ignoreDirectory(partialName: string): boolean {
        // do not traverse into . directories
        if (partialName.startsWith('.')) {
            return true;
        }
        return false;
    }

    private static wipeCacheDirectory(cacheDirectory: string) {
        clearCOBOLCache();
        InMemoryGlobalSymbolCache.callableSymbols.clear();
        InMemoryGlobalSymbolCache.classSymbols.clear();
        InMemoryGlobalSymbolCache.isDirty = false;

        for (const file of fs.readdirSync(cacheDirectory)) {
            if (file.endsWith(".sym")) {
                const fileName = path.join(cacheDirectory, file);
                try {
                    fs.unlinkSync(fileName);
                }
                catch {
                    //continue
                }
            }
        }
        const jsonFile = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        try {
            fs.unlinkSync(jsonFile);
        } catch {
            //continue
        }
    }

    public static clearMetaData(settings: ICOBOLSettings, cacheDirectory: string): void {
        if (VSCobScanner.IsScannerActive(cacheDirectory)) {
            window.showInformationMessage(" Unabled to clear metadata while caching is already in progress");
            return;
        }

        window.showQuickPick(["Yes", "No"], { placeHolder: "Are you sure you want to clear the metadata?" }).then(function (data) {
            if (data === 'Yes') {
                VSCOBOLSourceScanner.wipeCacheDirectory(cacheDirectory);
                logMessage("Metadata cache cleared");
            }
        });
    }

    static readonly username = os.userInfo().username;

    private static getStoragePathArea(settings: ICOBOLSettings): string | undefined {
        const storagePath = getCurrentContext().storageUri?.fsPath;
        if (storagePath === undefined) {
            return undefined;
        }

        let storageid = settings.storagearea_id;
        if (storageid.length === 0) {
            storageid = crypto.randomBytes(16).toString("hex");
            workspace.getConfiguration("coboleditor").update("storagearea_id", storageid);

            // cleanup old storage area
            VSCOBOLSourceScanner.wipeCacheDirectory(storagePath);
            logMessage(" Rebuild of metadata cache is required due to moving to storagearea");
            logMessage("  cache into subdirectory to allow multi-workspace caching.");
        }

        const storagePathPlusUser = path.join(storagePath,VSCOBOLSourceScanner.username);
        if (!fs.existsSync(storagePathPlusUser)) {
            fs.mkdirSync(storagePathPlusUser);
        }
        const completePath = path.join(storagePathPlusUser, storageid);
        if (!fs.existsSync(completePath)) {
            fs.mkdirSync(completePath);
        }
        return completePath;
    }

    public static getCacheDirectory(): string | undefined {

        const settings = VSCOBOLConfiguration.get();

        // turned off via the strategy
        if (settings.cache_metadata === CacheDirectoryStrategy.Off) {
            return undefined;
        }

        if (getWorkspaceFolders()) {

            if (settings.cache_metadata === CacheDirectoryStrategy.Storage) {
                const storageDirectory: string | undefined = VSCOBOLSourceScanner.getStoragePathArea(settings);

                /* no storage directory */
                if (storageDirectory === undefined) {
                    return undefined;
                }

                if (!isDirectory(storageDirectory)) {
                    try {
                        fs.mkdirSync(storageDirectory);
                    }
                    catch {
                        // swallow
                    }
                }

                /* not a directory, so ignore */
                if (!isDirectory(storageDirectory)) {
                    return undefined;
                }

                return storageDirectory;
            }

            let firstCacheDir = "";

            const ws = getWorkspaceFolders();
            if (ws !== undefined) {
                for (const folder of ws) {
                    const cacheDir2: string = path.join(folder.uri.fsPath, ".vscode_cobol");
                    if (isDirectory(cacheDir2)) {
                        return cacheDir2;
                    }
                    if (firstCacheDir === "") {
                        firstCacheDir = cacheDir2;
                    }
                }
            }

            if (firstCacheDir.length === 0) {
                return undefined;
            }

            if (isDirectory(firstCacheDir) === false) {
                try {
                    fs.mkdirSync(firstCacheDir);
                }
                catch {
                    return undefined;
                }
            }

            return firstCacheDir;
        }

        return undefined;
    }
}
