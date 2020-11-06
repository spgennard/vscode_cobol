import { VSCodeSourceHandler } from "./vscodesourcehandler";
import { FileSystemError, FileType, TextDocument, Uri, window, workspace } from 'vscode';
import COBOLSourceScanner, { COBOLToken, COBOLTokenStyle, EmptyCOBOLSourceScannerEventHandler, ICOBOLSourceScanner, ICOBOLSourceScannerEvents, SharedSourceReferences } from "./cobolsourcescanner";
import { GlobalCachesHelper } from "./globalcachehelper";

import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import * as crypto from 'crypto';

import { logMessage, logException, logTimedMessage, isDirectory, performance_now, getCurrentContext, logChannelSetPreserveFocus, logChannelHide, ExternalFeatures } from "./extension";
import { FileSourceHandler } from "./filesourcehandler";
import { COBOLFileUtils } from "./opencopybook";
import { VSCOBOLConfiguration } from "./configuration";
import { getWorkspaceFolders } from "./cobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLSymbolTableHelper } from "./cobolglobalcache_file";
import { COBOLSymbolTable, InMemoryGlobalSymbolCache } from "./cobolglobalcache";
import { CacheDirectoryStrategy } from "./externalfeatures";
import { COBOLSymbolTableEventHelper } from "./cobolsymboltableeventhelper";
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

    public static async processAllFilesInWorkspaces(viaCommand: boolean): Promise<void> {

        if (VSCOBOLConfiguration.isOnDiskCachingEnabled() === false) {
            logMessage("Metadata cache is off, no action taken");
            return;
        }

        if (!viaCommand) {
            logChannelHide();
        } else {
            logChannelSetPreserveFocus(!viaCommand);
        }

        if (getWorkspaceFolders()) {
            const stats = new VSScanStats();
            const settings = VSCOBOLConfiguration.get();
            stats.showMessage = settings.cache_metadata_show_progress_messages;
            let aborted = false;
            let abortedReason: Error | undefined = undefined;

            if (!viaCommand) {
                stats.timeCap = settings.cache_metadata_time_limit;
            }
            stats.start = performance_now();
            const cacheDirectory = VSCOBOLSourceScanner.getCacheDirectory();
            if (cacheDirectory !== undefined) {
                try {
                    logMessage("");
                    logMessage("Starting to process metadata from workspace folders (" + (viaCommand ? "on demand" : "startup") + ")");
                    logMessage(` Maximum time cap for meta data process set to ${stats.timeCap}`);
                    const promises = new Map<string, Promise<boolean>>();

                    const ws = getWorkspaceFolders();
                    if (ws !== undefined) {
                        for (const folder of ws) {
                            promises.set(folder.uri.fsPath, VSCOBOLSourceScanner.processAllFilesDirectory(settings, cacheDirectory, folder.uri, folder.uri, stats));
                        }
                    }

                    for (const [pathOrFilename, promise] of promises) {
                        try {
                            await promise;
                        } catch (e) {
                            if (e instanceof FileSystemError) {
                                const fse = e as FileSystemError;
                                if (fse.code === 'FileNotFound') {
                                    logMessage(`  Skipping ${pathOrFilename} (Not Found)`);
                                    continue;
                                }
                            }
                            throw e;
                        }
                    }
                    promises.clear();

                    GlobalCachesHelper.saveGlobalCache(cacheDirectory);
                }
                catch (e) {
                    abortedReason = e;
                    aborted = true;
                } finally {
                    const end = performance_now() - stats.start;
                    const completedMessage = (aborted ? `Scan aborted (elapsed time ${end})` : 'Completed scanning all COBOL files in workspace');
                    if (logTimedMessage(end, completedMessage) === false) {
                        logMessage(completedMessage);
                    }
                    if (stats.directoriesScanned !== 0) {
                        logMessage(` Directories scanned : ${stats.directoriesScanned}`);
                    }

                    if (stats.maxDirectoryDepth !== 0) {
                        logMessage(` Directory Depth     : ${stats.maxDirectoryDepth}`);
                    }

                    if (stats.fileCount !== 0) {
                        logMessage(` Files found         : ${stats.fileCount}`);
                    }

                    if (stats.filesScanned !== 0) {
                        logMessage(` Files scanned       : ${stats.filesScanned}`);
                    }

                    if (stats.filesUptodate !== 0) {
                        logMessage(` Files up to date    : ${stats.filesUptodate}`);
                    }

                    if (stats.programsDefined !== 0) {
                        logMessage(` Program Count       : ${stats.programsDefined}`);
                    }

                    if (stats.entryPointsDefined !== 0) {
                        logMessage(` Entry-Point Count   : ${stats.entryPointsDefined}`);
                    }

                    if (end < stats.timeCap && abortedReason !== undefined) {
                        if (abortedReason instanceof Error) {
                            logException("Unexpected abort", abortedReason as Error);
                        } else {
                            logMessage(abortedReason);
                        }
                    }

                }

            }
        } else {
            logMessage(" No workspaces folders present, no metadata processed.");
        }

        if (viaCommand) {
            logChannelSetPreserveFocus(true);
        }
    }

    public static ignoreDirectory(partialName: string): boolean {
        // do not traverse into . directories
        if (partialName.startsWith('.')) {
            return true;
        }
        return false;
    }

    private static async processAllFilesDirectory(settings: ICOBOLSettings, cacheDirectory: string, topLevelFolder: Uri, folder: Uri, stats: VSScanStats): Promise<boolean> {
        try {
            const entries = await workspace.fs.readDirectory(folder);
            stats.directoriesScanned++;
            if (stats.directoriesScannedMap.has(folder.fsPath)) {
                return true;
            }

            if (stats.showMessage) {
                const spaces = " ".repeat(stats.directoryDepth);
                logMessage(` ${spaces}Directory : ${folder.fsPath}`);
            }

            stats.directoriesScannedMap.set(folder.fsPath, folder);

            const dir2scan: Uri[] = [];

            for (const [entry, fileType] of entries) {
                switch (fileType) {
                    case FileType.File | FileType.SymbolicLink:
                        {
                            const spaces4file = " ".repeat(1 + stats.directoryDepth);
                            logMessage(`${spaces4file} File : ${entry} in ${folder.fsPath} is a symbolic link which may cause duplicate data to be cached`);
                        }
                    // eslint-disable-next-line no-fallthrough
                    case FileType.File:
                        {
                            const fullFilename = path.join(folder.fsPath, entry);
                            await VSCOBOLSourceScanner.processFile(settings, cacheDirectory, fullFilename, stats);
                        }
                        break;
                    case FileType.Directory | FileType.SymbolicLink:
                        {
                            const spaces4dir = " ".repeat(1 + stats.directoryDepth);
                            logMessage(`${spaces4dir} Directory : ${entry} in ${folder.fsPath} is a symbolic link which may cause duplicate data to be cached`);
                        }
                    // eslint-disable-next-line no-fallthrough
                    case FileType.Directory:
                        if (!VSCOBOLSourceScanner.ignoreDirectory(entry)) {
                            const fullDirectory = path.join(folder.fsPath, entry);
                            if (!VSCOBOLSourceScanner.ignoreDirectory(entry)) {
                                const topLevelLength = 1 + topLevelFolder.fsPath.length;
                                const possibleCopydir = fullDirectory.substr(topLevelLength);
                                // if (possibleCopydir.length !== 0) {
                                //     continue;
                                // }
                                if (COBOLUtils.inCopybookdirs(settings, possibleCopydir) === false) {
                                    logMessage(`  Configuration Recommendation : include : ${possibleCopydir} in coboleditor.copybookdirs to ensure the copybook`);
                                }
                                try {
                                    dir2scan.push(Uri.file(fullDirectory));
                                } catch (ex) {
                                    logMessage(` Uri.file failed with ${fullDirectory} from ${folder.fsPath} + ${entry}`);
                                    if (ex instanceof Error) {
                                        logException("Unexpected abort during Uri Parse", ex as Error);
                                    } else {
                                        logMessage(ex);
                                    }
                                }
                            }
                        }
                        break;
                }
            }

            if (dir2scan.length !== 0) {
                if (1 + stats.directoryDepth <= settings.cache_metadata_max_directory_scan_depth) {
                    stats.directoryDepth++;
                    for (const directoryUri of dir2scan) {
                        await VSCOBOLSourceScanner.processAllFilesDirectory(settings, cacheDirectory, topLevelFolder, directoryUri, stats);
                    }
                    if (stats.directoryDepth > stats.maxDirectoryDepth) {
                        stats.maxDirectoryDepth = stats.directoryDepth;
                    }
                    stats.directoryDepth--;
                } else {
                    logMessage(` Directories below : ${folder.fsPath} has not been scanned (depth limit is ${settings.cache_metadata_max_directory_scan_depth})`);
                }

            }
        } catch (ex) {
            // do nothing
        }
        return true;
    }

    private static async processFile(settings: ICOBOLSettings, cacheDirectory: string, filename: string, stats: ScanStats): Promise<boolean> {
        let parseThisFilename = false;

        stats.fileCount++;
        const end = performance_now() - stats.start;
        if (end > stats.timeCap) {
            throw new Error("Processing has been aborted");
        }

        // could be an overlap.. so test for valid program first and then copybooks
        parseThisFilename = COBOLFileUtils.isValidProgramExtension(filename, settings);
        if (parseThisFilename === false) {
            parseThisFilename = COBOLFileUtils.isValidCopybookExtension(filename, settings);
            if (parseThisFilename) {
                stats.copyBookExts++;
            }
        }

        if (parseThisFilename) {
            if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, filename)) {
                if (stats.showMessage) {
                    const spaces = " ".repeat(1 + stats.directoryDepth);
                    logMessage(` ${spaces}File : ${filename}`);
                }

                try {
                    const filefs = new FileSourceHandler(filename, false);
                    const symbolCacher = new COBOLSymbolTableEventHelper(settings);
                    // eslint-disable-next-line @typescript-eslint/no-unused-vars
                    // treat each file as the top-level file and don't parse anything below it
                    const qcp = new COBOLSourceScanner(filefs, settings, cacheDirectory, new SharedSourceReferences(true), false, symbolCacher, ExternalFeatures);
                    if (qcp.callTargets.size > 0) {
                        stats.programsDefined++;
                        if (qcp.callTargets !== undefined) {
                            stats.entryPointsDefined += (qcp.callTargets.size - 1);
                        }
                    }
                    stats.filesScanned++;
                } catch (e) {
                    if (e instanceof Error) {
                        logMessage(`Unable to scan ${filename}`, e as Error);
                    } else {
                        logMessage(e);
                    }
                }

            } else {
                stats.filesUptodate++;
            }
        }
        return true;
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
