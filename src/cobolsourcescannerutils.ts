/* eslint-disable no-console */
/* eslint-disable @typescript-eslint/no-explicit-any */

import * as fs from "fs";
import * as path from "path";

import { ICOBOLSettings } from "./iconfiguration";
import { COBOLSymbolTableHelper } from "./cobolglobalcache_file";
import { CacheDirectoryStrategy } from "./externalfeatures";
import { window, workspace } from "vscode";

import tempDirectory from "temp-dir";
import { InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { COBOLFileUtils } from "./fileutils";

const myConsoleFile = path.join(tempDirectory, "vscode_" + process.pid) + ".txt";

let myConsoleStream: fs.WriteStream;

let myConsole: Console;

function openLogFile() {
    myConsoleStream = fs.createWriteStream(myConsoleFile);
    myConsole = new console.Console(myConsoleStream);
}

function logMessage(message: string): void {
    myConsole.log(message);
}

export class COBOLSourceScannerUtils {

    public static cleanUpOldMetadataFiles(settings: ICOBOLSettings, cacheDirectory: string): void {
        if (settings.cache_metadata === CacheDirectoryStrategy.Off) {
            logMessage("Metadata is not enabled");
            return;
        }

        if (!settings.parse_copybooks_for_references) {
            try {
                const gPath = path.join(cacheDirectory, "globalsymbols.sym");
                if (COBOLFileUtils.isFile(gPath)) {
                    fs.unlinkSync(gPath);
                }
            }
            catch {
                //
            }

            try {
                const fPath = path.join(cacheDirectory, "filesymbols.sym");

                if (COBOLFileUtils.isFile(fPath)) {
                    fs.unlinkSync(fPath);
                }
            }
            catch {
                //
            }

        }
    }

    public static dumpMetaData(settings: ICOBOLSettings, cacheDirectory: string): void {
        openLogFile();
        if (settings.cache_metadata === CacheDirectoryStrategy.Off) {
            logMessage("Metadata is not enabled");
            myConsoleStream.close();
            workspace.openTextDocument(myConsoleFile);
            return;
        }

        logMessage("Metadata Dump");
        logMessage(" cache_metadata : " + settings.cache_metadata + "\n");
        logMessage(" cache folder             : " + cacheDirectory);
        logMessage(" Dirty flag               : " + InMemoryGlobalSymbolCache.isDirty);
        logMessage("");
        logMessage("Global symbols            : (made lowercase)");

        if (!settings.parse_copybooks_for_references) {
            logMessage("");
            logMessage("Paragraphs/Sections in file :");

            for (const file of fs.readdirSync(cacheDirectory)) {

                // ignore legacy sym files
                if (file === "globalsymbols.sym" || file === "filesymbols.sym") {
                    COBOLSourceScannerUtils.cleanUpOldMetadataFiles(settings, cacheDirectory);
                    continue;
                }

                if (file.endsWith(".sym")) {
                    const symTable = COBOLSymbolTableHelper.getSymbolTable_direct(path.join(cacheDirectory, file));
                    if (symTable !== undefined) {
                        if (!(symTable.labelSymbols.size === 0 && symTable.variableSymbols.size === 0)) {
                            logMessage(" " + symTable.fileName + " in " + file);
                            logMessage("   Label symbol count    : " + symTable.labelSymbols.size);

                            for (const [, value] of symTable.labelSymbols.entries()) {
                                logMessage(`     ${value.symbol}:${value.lnum}`);
                            }

                            logMessage("   Variable symbol count : " + symTable.variableSymbols.size);
                            for (const [, value] of symTable.variableSymbols.entries()) {
                                logMessage(`     ${value.symbol}:${value.lnum}`);
                            }
                        }
                    }
                }
            }
        }

        myConsoleStream.close();
        workspace.openTextDocument(myConsoleFile).then(doc => window.showTextDocument(doc));
        // fs.unlinkSync(myConsoleFile);
    }

    public static cleanup(): void {
        try {
            if (COBOLFileUtils.isFile(myConsoleFile)) {
                fs.unlinkSync(myConsoleFile);
            }
        }
        catch {
            //
        }
    }
}
