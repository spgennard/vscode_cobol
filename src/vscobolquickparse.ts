import { VSCodeSourceHandler } from "./VSCodeSourceHandler";
import { workspace, TextDocument } from 'vscode';
import COBOLQuickParse, { COBOLSymbolTableHelper, COBOLSymbolTable, InMemoryGlobalFileCache, SharedSourceReferences } from "./cobolquickparse";
import { InMemoryGlobalCachesHelper } from "./imemorycache";

import * as fs from 'fs';
import * as path from 'path';

import { logMessage, logException, showLogChannel, logTimedMessage, isDirectory } from "./extension";
import { performance } from "perf_hooks";
import { FileSourceHandler } from "./FileSourceHandler";
import { isValidExtension } from "./opencopybook";
import { VSCOBOLConfiguration } from "./configuration";

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

    public static getCachedObject(document: TextDocument | undefined, fileName: string): COBOLQuickParse | undefined {
        if (this.isFile(fileName) === false) {
            return undefined;
        }

        let cachedObject: COBOLQuickParse | undefined = undefined;

        if (InMemoryCache.has(fileName)) {
            cachedObject = InMemoryCache.get(fileName);
        }

        /* if the document is edited, drop the in cached object */
        if (document !== undefined && document.isDirty) {
            InMemoryCache.delete(fileName);
            let file = new VSCodeSourceHandler(document, false);
            return new COBOLQuickParse(file, document.fileName, VSCOBOLConfiguration.get(), VSQuickCOBOLParse.getCacheDirectory());
        }


        /* does the cache object need to be updated? */
        if (cachedObject !== null && cachedObject !== undefined) {
            let stat: fs.Stats = fs.statSync(fileName);
            if (cachedObject.lastModifiedTime !== stat.mtimeMs) {
                InMemoryCache.delete(fileName);
                cachedObject = undefined;
            }
        }

        /* grab, the file parse it can cache it */
        if (cachedObject === null || cachedObject === undefined) {
            try {

                let file = new FileSourceHandler(fileName, false);

                var startTime = performance.now();
                let qcpd = new COBOLQuickParse(file, fileName, VSCOBOLConfiguration.get(), VSQuickCOBOLParse.getCacheDirectory());
                InMemoryCache.set(fileName, qcpd);
                logTimedMessage(performance.now() - startTime, " - Parsing " + fileName);

                if (InMemoryCache.size > 20) {
                    let firstKey = InMemoryCache.keys().next().value;
                    InMemoryCache.delete(firstKey);
                }
                return qcpd;
            }
            catch (e) {
                logException("getCachedObject", e);
            }
        }

        return cachedObject;
    }


    public static processAllFilesInWorkspaces() {
        showLogChannel();

        if (VSCOBOLConfiguration.isCachingSetToON() === false) {
            logMessage("Metadata cache is off, no action taken");
            return;
        }

        if (workspace.workspaceFolders) {
            let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
            var start = performance.now();

            for (var folder of workspace.workspaceFolders) {
                try {)
                    VSQuickCOBOLParse.processAllFilesDirectory(cacheDirectory, folder.uri.fsPath);
                }
                catch (re) {
                    logException("processAllFilesInWorkspaces/1", re);
                }
            }

            if (InMemoryGlobalFileCache.copybookFileSymbols.size !== 0) {
                for (let [i, tag] of InMemoryGlobalFileCache.copybookFileSymbols.entries()) {
                    try {
                        VSQuickCOBOLParse.processFileInDirectory(cacheDirectory, i, false);
                    }
                    catch (re) {
                        logException("processAllFilesInWorkspaces/2 => " + i, re);
                    }
                }
            }
            InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(VSQuickCOBOLParse.getCacheDirectory());
            var end = performance.now() - start;
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

                        let filefs = new FileSourceHandler(filename, false);
                        let qcp = new COBOLQuickParse(filefs, filename, VSCOBOLConfiguration.get(), cacheDirectory);
                        if (qcp.sourceLooksLikeCOBOL) {
                            logMessage(" - Processing file : " + filename);

                            /* iterater through all the known copybook references */
                            for (let [key, value] of qcp.copyBooksUsed) {
                                try {
                                    qcp.processExternalCopybook(cacheDirectory, false, key);
                                }
                                catch (fe) {
                                    logException("processFileInDirectory/2", fe);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public static getCacheDirectory(): string {

        if (workspace.workspaceFolders && VSCOBOLConfiguration.isCachingSetToON() === true) {
            let firstCacheDir = "";

            for (var folder of workspace.workspaceFolders) {
                let cacheDir2: string = path.join(folder.uri.fsPath, ".vscode_cobol");
                if (isDirectory(cacheDir2)) {
                    return cacheDir2;
                }
                if (firstCacheDir === "") {
                    firstCacheDir = cacheDir2;
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
