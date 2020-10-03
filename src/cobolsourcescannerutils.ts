/* eslint-disable @typescript-eslint/no-explicit-any */

import { String } from 'typescript-string-operations';

import * as fs from 'fs';
import * as path from 'path';

import { logMessage } from "./extension";

import { ICOBOLSettings } from "./iconfiguration";
import { CacheDirectoryStrategy, } from "./configuration";
import { InMemoryGlobalSymbolCache, globalSymbolFilename, fileSymbolFilename, COBOLFileSymbol } from "./cobolglobalcache";
import { COBOLSymbolTableHelper } from './cobolglobalcache_file';

export class COBOLSourceScannerUtils {

    public static dumpMetaData(settings: ICOBOLSettings, cacheDirectory: string): void {

        if (settings.cache_metadata === CacheDirectoryStrategy.Off) {
            logMessage("Metadata is not enabled");
            return;
        }

        logMessage("Metadata Dump");
        logMessage(" cache_metadata : " + settings.cache_metadata + "\n");
        logMessage(" cache folder             : " + cacheDirectory);
        logMessage(" Last modified            : " + InMemoryGlobalSymbolCache.lastModifiedTime);
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
                    logMessage(String.Format(" {0} => {1}:{2}", i.padEnd(40), value.filename, value.lnum === undefined ? 1 : 1+value.lnum));
                });
            }
        }

        if (!settings.parse_copybooks_for_references) {
            logMessage("");
            logMessage("Paragraphs/Sections in file :");

            for (const file of fs.readdirSync(cacheDirectory)) {
                if (file !== globalSymbolFilename && file !== fileSymbolFilename && file.endsWith(".sym")) {
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
    }
}
