import { VSCodeSourceHandler } from "./vscodesourcehandler";
import { FileType, TextDocument, Uri, window, workspace } from 'vscode';
import COBOLSourceScanner, { COBOLToken, COBOLTokenStyle, EmptyCOBOLSourceScannerEventHandler, ICOBOLSourceScanner, ICOBOLSourceScannerEvents, SharedSourceReferences } from "./cobolsourcescanner";
import { GlobalCachesHelper } from "./globalcachehelper";

import * as fs from 'fs';
import * as path from 'path';

import { logMessage, logException, logTimedMessage, isDirectory, performance_now, logChannelSetPreserveFocus, ExternalFeatures } from "./extension";
import { VSCOBOLConfiguration } from "./configuration";
import { getWorkspaceFolders } from "./cobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLSymbolTableHelper } from "./cobolglobalcache_file";
import { COBOLSymbolTable } from "./cobolglobalcache";
import { CacheDirectoryStrategy } from "./externalfeatures";
import { COBOLUtils } from "./cobolutils";
import { ScanDataHelper, ScanStats } from "./cobscannerdata";
import { VSCobScanner } from "./vscobscanner";
import { COBOLFileUtils } from "./opencopybook";
import { COBOLWorkspaceSymbolCacheHelper, InMemoryGlobalSymbolCache, TypeCategory } from "./cobolworkspacecache";

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
            GlobalCachesHelper.addFilename(this.st?.fileName, this.st?.lastModifiedTime);
            COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(this.st?.fileName)
            COBOLWorkspaceSymbolCacheHelper.removeAllTypes(this.st?.fileName);
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
            case COBOLTokenStyle.ImplicitProgramId:
                COBOLWorkspaceSymbolCacheHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.ProgramId:
                COBOLWorkspaceSymbolCacheHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.EntryPoint:
                COBOLWorkspaceSymbolCacheHelper.addEntryPoint(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.InterfaceId:
                COBOLWorkspaceSymbolCacheHelper.addClass(this.st.fileName,token.tokenName, token.startLine, TypeCategory.InterfaceId);
                break;
            case COBOLTokenStyle.EnumId:
                COBOLWorkspaceSymbolCacheHelper.addClass(this.st.fileName,token.tokenName, token.startLine, TypeCategory.EnumId);
                break;
            case COBOLTokenStyle.ClassId:
                COBOLWorkspaceSymbolCacheHelper.addClass(this.st.fileName,token.tokenName, token.startLine, TypeCategory.ClassId);
                break;
            case COBOLTokenStyle.MethodId:
                // GlobalCachesHelper.addMethodSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
        }
    }

    public finish(): void {
        if (this.config.cache_metadata !== CacheDirectoryStrategy.Off &&
            this.qp !== undefined &&
            this.st !== undefined) {
            COBOLSymbolTableHelper.saveToFile(this.qp.cacheDirectory, this.st);
        }
        COBOLUtils.saveGlobalCacheToWorkspace();
    }
}

export default class VSCOBOLSourceScanner {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    static clearCache(settings: ICOBOLSettings):void {
        logMessage("Clearing internal cache");
        clearCOBOLCache();
        // commands.executeCommand("workbench.action.reloadWindow");
    }

    private static readonly MAX_MEM_CACHE_SIZE = 30;

    public static getCachedObject(document: TextDocument): COBOLSourceScanner | undefined {
        const fileName: string = document.fileName;
        const cacheDirectory: string | undefined = VSCOBOLSourceScanner.getCacheDirectory();
        const config = VSCOBOLConfiguration.get();

        // file is too large to parse
        if (document.lineCount > config.editor_maxTokenizationLineLength) {
            logMessage(` ${document.fileName} is not parsed, line count is ${document.lineCount} and editor.maxTokenizationLineLength is ${config.editor_maxTokenizationLineLength}`);
            return undefined;
        }

        /* if the document is edited, drop the in cached object */
        if (document.isDirty) {
            InMemoryCache.delete(fileName);
        }

        /* grab, the file parse it can cache it */
        if (InMemoryCache.has(fileName) === false) {
            try {
                const startTime = performance_now();
                const qcpd = new COBOLSourceScanner(new VSCodeSourceHandler(document, false), config,
                    cacheDirectory === undefined ? "" : cacheDirectory, new SharedSourceReferences(true),
                    config.parse_copybooks_for_references,
                    config.process_metadata_cache_on_file_save ? new COBOLSymbolTableGlobalEventHelper(config) : EmptyCOBOLSourceScannerEventHandler.Default,
                    ExternalFeatures);

                logTimedMessage(performance_now() - startTime, " - Parsing " + fileName);

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
                                const copyBookCount = await VSCOBOLSourceScanner.howManyCopyBooksInDirectory(fullDirectory, settings);
                                if (copyBookCount !== 0) {
                                    logMessage(`  Add: ${possibleCopydir} to coboleditor.copybookdirs (possible copybooks ${copyBookCount})`);
                                }
                            }
                            await VSCOBOLSourceScanner.checkWorkspaceForMissingCopybookDir(settings, topLevelFolder, Uri.file(fullDirectory));
                        }
                    }
                    break;
            }
        }
        return;
    }

    public static async howManyCopyBooksInDirectory(directory: string, settings: ICOBOLSettings): Promise<number> {
        const folder = Uri.file(directory);
        const entries = await workspace.fs.readDirectory(folder);
        let copyBookCount = 0;
        for (const [entry, fileType] of entries) {
            switch (fileType) {
                case FileType.File | FileType.SymbolicLink:
                case FileType.File:
                    if (COBOLFileUtils.isValidCopybookExtension(entry, settings)) {
                        copyBookCount++;
                    }

            }
        }

        return copyBookCount;
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
            window.showInformationMessage(" Unable to clear metadata while caching is already in progress");
            return;
        }

        window.showQuickPick(["Yes", "No"], { placeHolder: "Are you sure you want to clear the metadata?" }).then(function (data) {
            if (data === 'Yes') {
                VSCOBOLSourceScanner.wipeCacheDirectory(cacheDirectory);
                logMessage("Metadata cache cleared");
            }
        });
    }

    public static getCacheDirectory(): string | undefined {

        const settings = VSCOBOLConfiguration.get();

        // turned off via the strategy
        if (settings.cache_metadata === CacheDirectoryStrategy.Off) {
            return undefined;
        }

        if (getWorkspaceFolders()) {

            if (settings.cache_metadata === CacheDirectoryStrategy.UserDefinedDirectory) {
                const storageDirectory: string | undefined = VSCOBOLSourceScanner.getUserStorageDirectory(settings);

                /* no storage directory */
                if (storageDirectory === undefined) {
                    return undefined;
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

    static getUserStorageDirectory(settings: ICOBOLSettings): string | undefined {
        let str = settings.cache_metadata_user_directory;
        if (str === null || str.length === 0) {
            return undefined;
        }

        // if on Windows replace ${HOME} with ${USERPROFILE}
        if (COBOLFileUtils.isWin32) {
            str = str.replace(/\$\{HOME\}/,'${USERPROFILE}');
        }

        const replaced = str.replace(/\$\{([^%]+)\}/g, (_original, matched) => {
            const r = process.env[matched];
            return r ? r : ''
        });

        return replaced;
    }
}
