/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import { COBOLFileSymbol, COBOLWorkspaceFile, InMemoryFileSymbolCache } from "./cobolglobalcache";
import { InMemoryGlobalCacheHelper, InMemoryGlobalSymbolCache } from "./globalcachehelper";


export enum TypeCategory{
    ClassId="T",
    InterfaceId="I",
    EnumId="E"
}

export class COBOLWorkspaceSymbolCacheHelper {
    private static removeAllProgramSymbols(srcfilename: string, symbolsCache: Map<string, COBOLFileSymbol[]>): void {
        for (const [key] of symbolsCache) {
            const symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(key);
            if (symbolList !== undefined) {
                const newSymbols:COBOLFileSymbol[] = [];
                for (let i = 0; i < symbolList.length; i++) {
                    if (symbolList[i].filename !== srcfilename) {
                        newSymbols.push(symbolList[i]);
                    }
                }

                if (newSymbols.length !== symbolList.length) {
                    symbolsCache.set(key, newSymbols);
                }
            }
        }
    }

    private static addSymbolToCache(srcfilename: string, symbolUnchanged: string, lineNumber: number, symbolsCache: Map<string, COBOLFileSymbol[]>) {
        const symbol = symbolUnchanged.toLowerCase();
        if (symbolsCache.has(symbol)) {
            const symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(symbol);

            /* search the list of COBOLFileSymbols */
            if (symbolList !== undefined) {
                let foundCount = 0;
                let foundLast = 0;
                let foundLastNonFileSymbol = -1;
                for (let i = 0; i < symbolList.length; i++) {
                    if (symbolList[i].filename === srcfilename) {
                        foundLast = i;
                        foundCount++;

                        // remember last non file line number
                        if (symbolList[i].lnum !== 1) {
                            foundLastNonFileSymbol = i;
                        }
                    }
                }
                // not found?
                if (foundCount === 0) {
                    symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
                    InMemoryGlobalSymbolCache.isDirty = true;
                    return;
                }
                // if we have only one symbol, then we can update it
                if (foundCount === 1) {
                    symbolList[foundLast].lnum = lineNumber;
                    InMemoryGlobalSymbolCache.isDirty = true;
                } else {
                    // if we have multiple, never update the filename symbol which has a line number of 1
                    if (foundLastNonFileSymbol !== -1) {
                        symbolList[foundLastNonFileSymbol].lnum = lineNumber;
                        InMemoryGlobalSymbolCache.isDirty = true;
                    }
                }

                return;
            }
        }
        const symbolList = [];
        symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
        symbolsCache.set(symbol, symbolList);
        InMemoryGlobalSymbolCache.isDirty = true;
        return;
    }

    public static addSymbol(srcfilename: string, symbolUnchanged: string, lineNumber = 1): void {
        COBOLWorkspaceSymbolCacheHelper.addSymbolToCache(
            InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename), symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.callableSymbols);
    }

    public static addEntryPoint(srcfilename: string, symbolUnchanged: string, lineNumber: number): void {
        COBOLWorkspaceSymbolCacheHelper.addSymbolToCache(
            InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename), symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.entryPoints);
    }


    public static addClass(srcfilename: string, symbolUnchanged: string, lineNumber: number, category: TypeCategory): void {
        let map: Map<string, COBOLFileSymbol[]> = InMemoryGlobalSymbolCache.types;

        switch(category) {
            case TypeCategory.ClassId : map = InMemoryGlobalSymbolCache.types; break;
            case TypeCategory.InterfaceId : map = InMemoryGlobalSymbolCache.interfaces; break;
            case TypeCategory.EnumId : map = InMemoryGlobalSymbolCache.enums; break;
        }

        COBOLWorkspaceSymbolCacheHelper.addSymbolToCache(
            InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename), symbolUnchanged, lineNumber, map);
    }

    public static removeAllProgramEntryPoints(srcfilename: string):void {
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramSymbols(InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename),
        InMemoryGlobalSymbolCache.entryPoints);
    }

    public static removeAllTypes(srcfilename: string):void {
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramSymbols(InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename),
        InMemoryGlobalSymbolCache.types);
    }

    public static loadGlobalCacheFromArray(symbols: string[], clear:boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.callableSymbols.clear();
        }

        for (const symbol of symbols) {
            const symbolValues = symbol.split(",");
            if (symbolValues.length === 2) {
                COBOLWorkspaceSymbolCacheHelper.addSymbol(symbolValues[1], symbolValues[0]);
            }
        }
    }

    public static loadGlobalEntryCacheFromArray(symbols: string[], clear:boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.callableSymbols.entries();
        }

        for (const symbol of symbols) {
            const symbolValues = symbol.split(",");
            if (symbolValues.length === 3) {
                COBOLWorkspaceSymbolCacheHelper.addEntryPoint(symbolValues[1], symbolValues[0], Number.parseInt(symbolValues[2],10));
            }
        }
    }

    public static loadGlobalTypesCacheFromArray(symbols: string[], clear:boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.enums.clear();
            InMemoryGlobalSymbolCache.interfaces.clear();
            InMemoryGlobalSymbolCache.types.clear();
        }

        for (const symbol of symbols) {
            const symbolValues = symbol.split(",");
            if (symbolValues.length === 4) {
                let cat = TypeCategory.ClassId;
                switch (symbolValues[0]) {
                    case "I" : cat = TypeCategory.InterfaceId; break;
                    case "T" : cat = TypeCategory.ClassId; break;
                    case "E" : cat = TypeCategory.EnumId; break;
                }
                COBOLWorkspaceSymbolCacheHelper.addClass(symbolValues[2], symbolValues[1], Number.parseInt(symbolValues[3],10), cat);
            }
        }
    }

    public static loadFileCacheFromArray(files: string[], clear:boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.sourceFilenameModified.clear();
        }

        for (const symbol of files) {
            const fileValues = symbol.split(",");
            if (fileValues.length === 3) {
                const ms = BigInt(fileValues[1]);
                const cws = new COBOLWorkspaceFile(ms,fileValues[2]);
                InMemoryGlobalSymbolCache.sourceFilenameModified.set(fileValues[0],cws);
            }
        }
    }
}
