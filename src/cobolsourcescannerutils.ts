/* eslint-disable @typescript-eslint/no-explicit-any */

import { String } from 'typescript-string-operations';

import * as fs from 'fs';
import * as path from 'path';

import { ICOBOLSettings } from "./iconfiguration";
import { COBOLFileSymbol } from "./cobolglobalcache";
import { COBOLSymbolTableHelper } from './cobolglobalcache_file';
import { CacheDirectoryStrategy } from './externalfeatures';
import { window, workspace } from 'vscode';

import tempDirectory from 'temp-dir';
import { InMemoryGlobalSymbolCache } from './globalcachehelper';

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
                if (fs.existsSync(gPath)) {
                    fs.unlinkSync(gPath);
                }
            }
            catch {
                //
            }

            try {
                const fPath = path.join(cacheDirectory, "filesymbols.sym");

                if (fs.existsSync(fPath)) {
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

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        for (const [i] of InMemoryGlobalSymbolCache.callableSymbols.entries()) {
            const fileSymbol: COBOLFileSymbol[] | undefined = InMemoryGlobalSymbolCache.callableSymbols.get(i);
            if (fileSymbol === undefined) {
                logMessage("  " + i + " => empty");
            } else {
                fileSymbol.forEach(function (value: COBOLFileSymbol) {
                    // lvalue 0-n-1 but terminal urls are 1-n
                    // logMessage(String.Format(" {0} => {1}:{2}", i.padEnd(40), value.filename, value.lnum === undefined ? 1 : 1+value.lnum));
                    logMessage(String.Format(" {0} => {1}:{2}", i, value.filename, value.lnum === undefined ? 1 : 1 + value.lnum));
                });
            }
        }

        if (!settings.parse_copybooks_for_references) {
            logMessage("");
            logMessage("Paragraphs/Sections in file :");

            for (const file of fs.readdirSync(cacheDirectory)) {
                // ignore legacy sym files
                if (file === "globalsymbols.sym" || file === "filesymbols.sym") {
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
        } else {
            logMessage("");
            logMessage("Paragraphs/Sections in file : none (parse_copybooks_for_references set)");
        }

        myConsoleStream.close();
        workspace.openTextDocument(myConsoleFile).then(doc => window.showTextDocument(doc));
        // fs.unlinkSync(myConsoleFile);
    }

    public static cleanup(): void {
        try {
            if (fs.existsSync(myConsoleFile)) {
                fs.unlinkSync(myConsoleFile);
            }
        }
        catch {
            //
        }
    }
}
