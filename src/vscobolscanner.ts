import { VSCodeSourceHandler } from "./vscodesourcehandler";
import { TextDocument } from 'vscode';
import COBOLSourceScanner from "./cobolsourcescanner";
import { InMemoryGlobalCachesHelper } from "./imemorycache";

import * as fs from 'fs';
import * as path from 'path';

import { logMessage, logException, logTimedMessage, isDirectory, performance_now, getCurrentContext, logChannelSetPreserveFocus, logChannelHide } from "./extension";
import { FileSourceHandler } from "./filesourcehandler";
import { isValidCopybookExtension } from "./opencopybook";
import { CacheDirectoryStrategy, VSCOBOLConfiguration } from "./configuration";
import { getWorkspaceFolders } from "./cobolfolders";
import { InMemoryGlobalFileCache, COBOLSymbolTableHelper } from "./cobolglobalcache";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const InMemoryCache: Map<string, any> = new Map<string, any>();

export function clearCOBOLCache(): void {
    InMemoryCache.clear();
}

export default class VSQuickCOBOLParse {

    public static isFile(fileName: string): boolean {
        try {
            const stat: fs.Stats = fs.statSync(fileName);
            if (stat.isFile() && !stat.isDirectory()) {
                return true;
            }

        } catch {
            return false;
        }

        return false;
    }

    public static getCachedObject(document: TextDocument): COBOLSourceScanner | undefined {
        const fileName: string = document.fileName;
        const cacheDirectory: string | undefined = VSQuickCOBOLParse.getCacheDirectory();
        if (cacheDirectory === undefined) {
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
                const qcpd = new COBOLSourceScanner(new VSCodeSourceHandler(document, false), fileName, VSCOBOLConfiguration.get(), cacheDirectory);
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
            const start = performance_now();
            const cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
            if (cacheDirectory !== undefined) {
                try {
                    logMessage("Starting to process metadata from workspace folders (" + (viaCommand ? "on demand" : "startup") + ")");

                    const promises: Promise<boolean>[] = [];

                    const ws = getWorkspaceFolders();
                    if (ws !== undefined) {
                        for (const folder of ws) {
                            const p = VSQuickCOBOLParse.processAllFilesDirectory(cacheDirectory, folder.uri.fsPath, viaCommand);
                            promises.push(p);
                        }
                    }

                    if (InMemoryGlobalFileCache.copybookFileSymbols.size !== 0) {
                        // eslint-disable-next-line @typescript-eslint/no-unused-vars
                        for (const [i, tag] of InMemoryGlobalFileCache.copybookFileSymbols.entries()) {
                            const p = VSQuickCOBOLParse.processFileInDirectory(cacheDirectory, i, false);
                            promises.push(p);
                        }
                    }
                    await Promise.all(promises);
                } finally {
                    InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(cacheDirectory);
                    const end = performance_now() - start;
                    const completedMessage = 'Completed scanning all COBOL files in workspace';
                    if (logTimedMessage(end, completedMessage) === false) {
                        logMessage(completedMessage);
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


    private static async processAllFilesDirectory(cacheDirectory: string, dir: string, showMessage: boolean): Promise<boolean> {
        if (showMessage) {
            logMessage(` Processing metadata for directory ${dir}`);
        }
        for (const file of fs.readdirSync(dir)) {
            VSQuickCOBOLParse.processFileInDirectory(cacheDirectory, path.join(dir, file), true);
        }

        return true;
    }

    private static async processFileInDirectory(cacheDirectory: string, filename: string, filterOnExtension: boolean): Promise<boolean> {
        if (this.isFile(filename) === false) {
            return false;
        }

        const settings = VSCOBOLConfiguration.get();
        const stat = fs.statSync(filename);

        if (stat.isDirectory() === false) {
            const parseThisFilename = filterOnExtension ? isValidCopybookExtension(filename, settings) : true;
            if (parseThisFilename) {
                if (VSQuickCOBOLParse.isFile(filename) === true) {
                    if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, filename)) {

                        const filefs = new FileSourceHandler(filename, false);
                        // eslint-disable-next-line @typescript-eslint/no-unused-vars
                        const qcp = new COBOLSourceScanner(filefs, filename, VSCOBOLConfiguration.get(), cacheDirectory);
                    }
                }
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
