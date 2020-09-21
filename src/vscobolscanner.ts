import { VSCodeSourceHandler } from "./vscodesourcehandler";
import { FileType, TextDocument, Uri, workspace } from 'vscode';
import COBOLSourceScanner from "./cobolsourcescanner";
import { InMemoryGlobalCachesHelper } from "./imemorycache";

import * as fs from 'fs';
import * as path from 'path';

import { logMessage, logException, logTimedMessage, isDirectory, performance_now, getCurrentContext, logChannelSetPreserveFocus, logChannelHide } from "./extension";
import { FileSourceHandler } from "./filesourcehandler";
import { isValidCopybookExtension, isValidProgramExtension } from "./opencopybook";
import { CacheDirectoryStrategy, VSCOBOLConfiguration } from "./configuration";
import { getWorkspaceFolders } from "./cobolfolders";
import { InMemoryGlobalFileCache, COBOLSymbolTableHelper } from "./cobolglobalcache";
import { ICOBOLSettings } from "./iconfiguration";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const InMemoryCache: Map<string, any> = new Map<string, any>();

export function clearCOBOLCache(): void {
    InMemoryCache.clear();
}

class ScanStats {
    timeCap = 600000;
    directoriesScanned = 0;
    directoryDepth = 0;
    maxDirectoryDepth = 0;
    filesScanned = 0;
    fileCount = 0;
    filesUptodate = 0;
    programsDefined = 0;
    entryPointsDefined = 0;
    start = 0;
    showMessage = false;
    directoriesScannedMap: Map<string, Uri> = new Map<string, Uri>();
}

export default class VSQuickCOBOLParse {

    public static getCachedObject(document: TextDocument): COBOLSourceScanner | undefined {
        const fileName: string = document.fileName;
        const cacheDirectory: string | undefined = VSQuickCOBOLParse.getCacheDirectory();

        /* if the document is edited, drop the in cached object */
        if (document.isDirty) {
            InMemoryCache.delete(fileName);
        }

        /* grab, the file parse it can cache it */
        if (InMemoryCache.has(fileName) === false) {
            try {
                const startTime = performance_now();
                const qcpd = new COBOLSourceScanner(new VSCodeSourceHandler(document, false), fileName, VSCOBOLConfiguration.get(),
                    cacheDirectory === undefined ? "" : cacheDirectory);
                InMemoryCache.set(fileName, qcpd);
                logTimedMessage(performance_now() - startTime, " - Parsing " + fileName);

                if (InMemoryCache.size > 20) {
                    const memKeys = InMemoryCache.keys();
                    let dropKey = memKeys.next().value;
                    if (dropKey === qcpd) {
                        dropKey = memKeys.next().value;
                    }
                    InMemoryCache.delete(dropKey);
                }
                return qcpd;
            }
            catch (e) {
                logException("getCachedObject", e);
            }
        }

        return InMemoryCache.get(fileName);
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
            const stats = new ScanStats();
            const settings = VSCOBOLConfiguration.get();
            stats.showMessage = settings.cache_metadata_show_progress_messages;
            let aborted = false;
            let abortedReason:Error|undefined = undefined;

            if (!viaCommand) {
                stats.timeCap = settings.cache_metadata_time_limit;
            }
            stats.start = performance_now();
            const cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
            if (cacheDirectory !== undefined) {
                try {
                    logMessage("");
                    logMessage("Starting to process metadata from workspace folders (" + (viaCommand ? "on demand" : "startup") + ")");
                    logMessage(` Maximum time cap for meta data process set to ${stats.timeCap}`);
                    const promises: Promise<boolean>[] = [];

                    const ws = getWorkspaceFolders();
                    if (ws !== undefined) {
                        for (const folder of ws) {
                            promises.push(VSQuickCOBOLParse.processAllFilesDirectory(settings, cacheDirectory, folder.uri, stats));
                        }
                    }

                    if (InMemoryGlobalFileCache.copybookFileSymbols.size !== 0) {
                        // eslint-disable-next-line @typescript-eslint/no-unused-vars
                        for (const [i, tag] of InMemoryGlobalFileCache.copybookFileSymbols.entries()) {
                            promises.push(VSQuickCOBOLParse.processFile(settings, cacheDirectory, i, false, stats));
                        }
                    }

                    for (const promise of promises) {
                        await promise;
                    }
                    InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(cacheDirectory);
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
                    logMessage(` Directories scanned : ${stats.directoriesScanned}`);
                    logMessage(` Directory Depth     : ${stats.maxDirectoryDepth}`);
                    logMessage(` Files found         : ${stats.fileCount}`);
                    logMessage(` File scanned        : ${stats.filesScanned}`);
                    logMessage(` File up to date     : ${stats.filesUptodate}`);
                    logMessage(` Program Count       : ${stats.programsDefined}`);
                    logMessage(` Entry-Point Count   : ${stats.entryPointsDefined}`);
                    const scanCopyBooks = !settings.parse_copybooks_for_references;
                    logMessage(` Scan copybooks      : ${scanCopyBooks}`);

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

    private static ignoreDirectory(partialName: string) {
        // do not traverse into . directories
        if (partialName.startsWith('.')) {
            return true;
        }
        return false;
    }

    private static async processAllFilesDirectory(settings: ICOBOLSettings, cacheDirectory: string, folder: Uri, stats: ScanStats): Promise<boolean> {

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
            if (fileType === FileType.File) {
                const fullFilename = path.join(folder.fsPath, entry);
                await VSQuickCOBOLParse.processFile(settings, cacheDirectory, fullFilename, true, stats);
            }
            else if (fileType === FileType.Directory) {
                if (!VSQuickCOBOLParse.ignoreDirectory(entry)) {
                    const fullFilename = path.join(folder.fsPath, entry);
                    if (!VSQuickCOBOLParse.ignoreDirectory(entry)) {
                        const directoryUri = Uri.parse(fullFilename);
                        dir2scan.push(directoryUri);
                    }
                }
            } else {
                logMessage(` Symbolic link ignored : ${folder.fsPath}`);
            }
        }

        if (dir2scan.length !== 0) {
            if (1 + stats.directoryDepth <= settings.cache_metadata_max_directory_scan_depth) {
                stats.directoryDepth++;
                for (const directoryUri of dir2scan) {
                    await VSQuickCOBOLParse.processAllFilesDirectory(settings, cacheDirectory, directoryUri, stats);
                }
                if (stats.directoryDepth > stats.maxDirectoryDepth) {
                    stats.maxDirectoryDepth = stats.directoryDepth;
                }
                stats.directoryDepth--;
            } else {
                logMessage(` Directories below : ${folder.fsPath} has not been scanned (depth limit is ${settings.cache_metadata_max_directory_scan_depth})`);
            }

        }

        return true;
    }

    private static async processFile(settings: ICOBOLSettings, cacheDirectory: string, filename: string, filterOnExtension: boolean, stats: ScanStats): Promise<boolean> {
        let parseThisFilename = false;

        stats.fileCount++;
        const end = performance_now() - stats.start;
        if (end > stats.timeCap) {
            throw new Error("Processing has been aborted");
        }

        if (filterOnExtension) {
            // if we are parsing copybooks, then we are only interested in programs
            if (settings.parse_copybooks_for_references) {
                parseThisFilename = isValidProgramExtension(filename, settings);
            } else {
                // could be an overlap.. so test for valid program first and then copybooks
                parseThisFilename = isValidProgramExtension(filename, settings);
                if (parseThisFilename === false) {
                    parseThisFilename = isValidCopybookExtension(filename, settings);
                }
            }
        }

        if (parseThisFilename) {
            if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, filename)) {
                if (stats.showMessage) {
                    const spaces = " ".repeat(1+stats.directoryDepth);
                    logMessage(` ${spaces}File: ${filename}`);
                }
                
                const filefs = new FileSourceHandler(filename, false);
                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                const qcp = new COBOLSourceScanner(filefs, filename, settings, cacheDirectory);
                if (qcp.callTargets.size > 0) {
                    stats.programsDefined++;
                    stats.entryPointsDefined += (qcp.callTargets.size - 1);
                }
                stats.filesScanned++;
            } else {
                stats.filesUptodate++;
            }
        }
        return true;
    }

    public static getCacheDirectory(): string | undefined {

        const settings = VSCOBOLConfiguration.get();

        // turned off via the strategy
        if (settings.cache_metadata === CacheDirectoryStrategy.Off) {
            return undefined;
        }

        if (getWorkspaceFolders()) {

            if (settings.cache_metadata === CacheDirectoryStrategy.Storage) {
                const storageDirectory: string | undefined = getCurrentContext().storageUri?.fsPath;

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
