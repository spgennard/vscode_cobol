import { VSCodeSourceHandler } from "./VSCodeSourceHandler";
import { workspace, TextDocument } from 'vscode';
import QuickCOBOLParse, { InMemoryGlobalCachesHelper, COBOLSymbolTableHelper, COBOLSymbolTable, InMemoryGlobalFileCache } from "./cobolquickparse";

import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';

import { getColumBParsing, getCopybookNestedInSection, logCOBOLChannelLine, logCOBOLChannelLineException, activateLogChannel, isCachingSetToON } from "./extension";
import { performance } from "perf_hooks";
import { FileSourceHandler } from "./FileSourceHandler";
import { expandLogicalCopyBookToFilenameOrEmpty, isValidExtension } from "./opencopybook";

const util = require('util');

export default class VSQuickCOBOLParse {

    public static isFile(fileName: string): boolean {

        if (fs.existsSync(fileName) === false) {
            return false;
        }

        try {
            let stat: fs.Stats = fs.statSync(fileName);
            if (stat.isFile() && !stat.isDirectory()) {
                return true;
            }

        } catch (err) {

        }

        return false;
    }
    public static getCachedObject(document: TextDocument | undefined, fileName: string): QuickCOBOLParse | undefined {
        if (this.isFile(fileName) === false) {
            return undefined;
        }

        let cachedObject: QuickCOBOLParse | undefined = undefined;

        if (InMemoryCache.has(fileName)) {
            cachedObject = InMemoryCache.get(fileName);
        }

        /* if the document is edited, drop the in cached object */
        if (document !== undefined && document.isDirty) {
            InMemoryCache.delete(fileName);
            let file = new VSCodeSourceHandler(document, false);
            return new QuickCOBOLParse(file, document.fileName, getColumBParsing(), getCopybookNestedInSection(), VSQuickCOBOLParse.getCacheDirectory());
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
                var startTime = performance.now();

                let file = new FileSourceHandler(fileName, false);
                let qcpd = new QuickCOBOLParse(file, fileName, getColumBParsing(), getCopybookNestedInSection(), VSQuickCOBOLParse.getCacheDirectory());

                InMemoryCache.set(fileName, qcpd);
                logCOBOLChannelLine(' - Load/Parse - Execution time: %dms -> ' + fileName, (performance.now() - startTime).toFixed(2));
                if (InMemoryCache.size > 20) {
                    let firstKey = InMemoryCache.keys().next().value;
                    InMemoryCache.delete(firstKey);
                }
                return qcpd;
            }
            catch (e) {
                logCOBOLChannelLineException("getCachedObject", e);
            }
        }

        return cachedObject;
    }


    public static processAllFilesInWorkspaces() {
        activateLogChannel(true);

        if (isCachingSetToON() === false) {
            logCOBOLChannelLine("Metadata cache is off, no action taken");
            return;
        }

        if (workspace.workspaceFolders) {
            let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
            var start = performance.now();

            for (var folder of workspace.workspaceFolders) {
                try {
                    VSQuickCOBOLParse.processAllFilesDirectory(cacheDirectory, folder.uri.fsPath);
                }
                catch (re) {
                    logCOBOLChannelLineException("processAllFilesInWorkspaces/1", re);
                }
            }

            if (InMemoryGlobalFileCache.copybookFileSymbols.size !== 0) {
                for (let [i, tag] of InMemoryGlobalFileCache.copybookFileSymbols.entries()) {
                    try {
                        VSQuickCOBOLParse.processFileInDirectory(cacheDirectory, i, false);
                    }
                    catch (re) {
                        logCOBOLChannelLineException("processAllFilesInWorkspaces/2 => " + i, re);
                    }
                }
            }
            InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(VSQuickCOBOLParse.getCacheDirectory());
            var end = performance.now() - start;
            logCOBOLChannelLine(util.format('Process all files in workspace -> Execution time: %dms', end.toFixed(2)));
        }

    }


    private static processAllFilesDirectory(cacheDirectory: string, dir: string) {
        for (var file of fs.readdirSync(dir)) {
            VSQuickCOBOLParse.processFileInDirectory(cacheDirectory, path.join(dir, file), true);
        }
    }

    private static processFileInDirectory(cacheDirectory:string, filename: string, filterOnExtension: boolean) {
        if (fs.existsSync(filename) === false) {
            return false;
        }

        let stat = fs.statSync(filename);

        if (stat.isDirectory() === false) {
            let parseThisFilename = filterOnExtension ? isValidExtension(filename) : true;
            if (parseThisFilename) {
                if (VSQuickCOBOLParse.isFile(filename) === true) {
                    if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, filename)) {

                        let filefs = new FileSourceHandler(filename, false);
                        let qcp = new QuickCOBOLParse(filefs, filename, getColumBParsing(), getCopybookNestedInSection(),cacheDirectory);
                        if (qcp.sourceLooksLikeCOBOL) {
                            logCOBOLChannelLine(" - Processing file : " + filename);

                            /* iterater through all the known copybook references */
                            for (let [key, value] of qcp.getcopyBooksUsed()) {
                                try {
                                    let copyBookfilename: string = "";
                                    try {
                                        copyBookfilename = expandLogicalCopyBookToFilenameOrEmpty(key);
                                        if (copyBookfilename.length !== 0) {
                                            if (COBOLSymbolTableHelper.cacheUpdateRequired(cacheDirectory, copyBookfilename)) {
                                                let filefs_vb = new FileSourceHandler(copyBookfilename, false);
                                                let qcp_vb = new QuickCOBOLParse(filefs_vb, copyBookfilename, getColumBParsing(), getCopybookNestedInSection(), cacheDirectory);
                                                let qcp_symtable: COBOLSymbolTable = COBOLSymbolTableHelper.getCOBOLSymbolTable(qcp_vb);

                                                COBOLSymbolTableHelper.saveToFile(cacheDirectory, qcp_symtable);
                                            }
                                        }
                                    }
                                    catch (ex) {
                                        if (copyBookfilename !== null) {
                                            logCOBOLChannelLineException("processFileInDirectory/1: " + copyBookfilename, ex);
                                        } else {
                                            logCOBOLChannelLineException("processFileInDirectory/1", ex);
                                        }
                                    }
                                }
                                catch (fe) {
                                    logCOBOLChannelLineException("processFileInDirectory/2", fe);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public static getCacheDirectory(): string {

        if (workspace.workspaceFolders && isCachingSetToON() === true) {
            let firstCacheDir = "";

            for (var folder of workspace.workspaceFolders) {
                let cacheDir2: string = path.join(folder.uri.fsPath, ".vscode_cobol");
                if (fs.existsSync(cacheDir2)) {
                    return cacheDir2;
                }
                if (firstCacheDir === "") {
                    firstCacheDir = cacheDir2;
                }
            }

            if (firstCacheDir.length === 0) {
                return "";
            }

            if (fs.existsSync(firstCacheDir) === false) {
                fs.mkdirSync(firstCacheDir);
            }

            return firstCacheDir;
        }

        return "";

    }


}

const InMemoryCache: Map<string, any> = new Map<string, any>();
