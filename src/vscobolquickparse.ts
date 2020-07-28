import { VSCodeSourceHandler } from "./vscodesourcehandler";
import { TextDocument } from 'vscode';
import COBOLQuickParse, { COBOLSymbolTableHelper, InMemoryGlobalFileCache } from "./cobolquickparse";
import { InMemoryGlobalCachesHelper } from "./imemorycache";

import * as fs from 'fs';
import * as path from 'path';

import { logMessage, logException, showLogChannel, logTimedMessage, isDirectory, performance_now, getCurrentContext } from "./extension";
import { FileSourceHandler } from "./filesourcehandler";
import { isValidExtension } from "./opencopybook";
import { VSCOBOLConfiguration } from "./configuration";
import { getWorkspaceFolders } from "./cobolfolders";

const InMemoryCache: Map<string, any> = new Map<string, any>();

export default class VSQuickCOBOLParse {

    public static isFile(fileName: string): boolean {
        try {
            let stat: fs.Stats = fs.statSync(fileName);
            if (stat.isFile() && !stat.isDirectory()) {
                return true;
            }

        } catch {

        }

        return false;
    }

    public static getCachedObject(document: TextDocument): COBOLQuickParse {
        let fileName: string = document.fileName;
        /* if the document is edited, drop the in cached object */
        if (document.isDirty) {
            InMemoryCache.delete(fileName);
        }

        /* grab, the file parse it can cache it */
        if (InMemoryCache.has(fileName) === false) {
            try {
                var startTime = performance_now();
                let qcpd = new COBOLQuickParse(new VSCodeSourceHandler(document, false), fileName, VSCOBOLConfiguration.get(), VSQuickCOBOLParse.getCacheDirectory());
                InMemoryCache.set(fileName, qcpd);
                logTimedMessage(performance_now() - startTime, " - Parsing " + fileName);

                if (InMemoryCache.size > 20) {
                    let memKeys = InMemoryCache.keys();
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


    public static processAllFilesInWorkspaces() {
        showLogChannel();

        if (VSCOBOLConfiguration.isOnDiskCachingEnabled() === false) {
            logMessage("Metadata cache is off, no action taken");
            return;
        }

        if (getWorkspaceFolders()) {
            let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
            var start = performance_now();

            let ws = getWorkspaceFolders();
            if (ws !== undefined) {
                for (var folder of ws) {
                    try {
                        VSQuickCOBOLParse.processAllFilesDirectory(cacheDirectory, folder.uri.fsPath);
                    }
                    catch (re) {
                        logMessage(`Processing ${folder.uri.fsPath} aborted early due to ${re.name}`);
                    }
                }
            }

            if (InMemoryGlobalFileCache.copybookFileSymbols.size !== 0) {
                for (let [i, tag] of InMemoryGlobalFileCache.copybookFileSymbols.entries()) {

                    try {
                        VSQuickCOBOLParse.processFileInDirectory(cacheDirectory, i, false);
                    }
                    catch (re) {
                        logMessage(`Processing ${i} aborted early due to ${re.name}`);
                    }
                }
            }
            InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(VSQuickCOBOLParse.getCacheDirectory());
            var end = performance_now() - start;
            logTimedMessage(end, 'Completed scanning all COBOL files in workspace');
        }
    }


    private static processAllFilesDirectory(cacheDirectory: string, dir: string) {
        for (var file of fs.readdirSync(dir)) {
            VSQuickCOBOLParse.processFileInDirectory(cacheDirectory, path.join(dir, file), true);
        }
    }

    private static processFileInDirectory(cacheDirectory: string, filename: string, filterOnExtension: boolean) {
        if (this.isFile(filename) === false) {
            return false;
        }

        let stat = fs.statSync(filename);

        if (stat.isDirectory() === false) {
            let parseThisFilename = filterOnExtension ? isValidExtension(filename) : true;
            if (parseThisFilename) {
                if (VSQuickCOBOLParse.isFile(filename) === true) {
                    if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, filename)) {

                        let filefs = new FileSourceHandler(filename, false, false);
                        let qcp = new COBOLQuickParse(filefs, filename, VSCOBOLConfiguration.get(), cacheDirectory);
                        if (qcp.sourceLooksLikeCOBOL) {
                            logMessage(" - Processed file : " + filename);
                        }
                    }
                }
            }
        }
    }

    public static getCacheDirectory(): string {

        if (getWorkspaceFolders() && VSCOBOLConfiguration.isOnDiskCachingEnabled() === true) {

            if (VSCOBOLConfiguration.getCache_directory_strategy() === "storagepath") {
                let storageDirectory: string | undefined = getCurrentContext().storagePath;
                if (storageDirectory !== undefined && !isDirectory(storageDirectory)) {
                    try {
                        fs.mkdirSync(storageDirectory);
                    }
                    catch {
                        // swallow
                    }
                }

                /* no storage directory */
                if (storageDirectory === undefined) {
                    return "";
                }

                /* not a directory, so ignore */
                if (!isDirectory(storageDirectory)) {
                    return "";
                }

                return storageDirectory;
            }

            let firstCacheDir = "";

            let ws = getWorkspaceFolders();
            if (ws !== undefined) {
                for (var folder of ws) {
                    let cacheDir2: string = path.join(folder.uri.fsPath, ".vscode_cobol");
                    if (isDirectory(cacheDir2)) {
                        return cacheDir2;
                    }
                    if (firstCacheDir === "") {
                        firstCacheDir = cacheDir2;
                    }
                }
            }

            if (firstCacheDir.length === 0) {
                return "";
            }

            if (isDirectory(firstCacheDir) === false) {
                try {
                    fs.mkdirSync(firstCacheDir);
                }
                catch {
                    return "";
                }
            }

            return firstCacheDir;
        }

        return "";

    }
}
