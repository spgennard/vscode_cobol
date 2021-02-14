import * as fs from 'fs';

// eslint-disable-next-line @typescript-eslint/no-var-requires
import { COBOLFileSymbol, InMemoryGlobalSymbolCache, COBOLGlobalSymbolTable } from './cobolglobalcache';
import path from 'path';

export class GlobalCachesHelper {
    private static isFileT(sdir: string): [boolean, fs.Stats | undefined] {
        try {
            if (fs.existsSync(sdir)) {
                const f = fs.statSync(sdir);
                if (f && f.isFile()) {
                    return [true, f];
                }
            }
        }
        catch {
            return [false, undefined];
        }
        return [false, undefined];
    }

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
                } else {
                    // if we have multiple, never update the filename symbol which has a line number of 1
                    if (foundLastNonFileSymbol !== -1) {
                        symbolList[foundLastNonFileSymbol].lnum = lineNumber;
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

    public static getFilenameWithoutPath(fullPath: string): string {
        const lastSlash = fullPath.lastIndexOf(path.sep);
        if (lastSlash === -1) {
            return fullPath;
        }
        return fullPath.substr(1 + lastSlash);
    }

    public static addSymbol(srcfilename: string, symbolUnchanged: string, lineNumber = 1): void {
        GlobalCachesHelper.addSymbolToCache(
            GlobalCachesHelper.getFilenameWithoutPath(srcfilename), symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.callableSymbols);
    }

    public static addEntryPoint(srcfilename: string, symbolUnchanged: string, lineNumber: number): void {
        GlobalCachesHelper.addSymbolToCache(
            GlobalCachesHelper.getFilenameWithoutPath(srcfilename), symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.entryPoints);
    }

    public static removeAllProgramEntryPoints(srcfilename: string):void {
        GlobalCachesHelper.removeAllProgramSymbols(GlobalCachesHelper.getFilenameWithoutPath(srcfilename),
        InMemoryGlobalSymbolCache.entryPoints);
    }

    public static getGlobalSymbolCache(): COBOLGlobalSymbolTable {
        return InMemoryGlobalSymbolCache;
    }

    public static addFilename(filename: string, lastModified: number): void {
        if (InMemoryGlobalSymbolCache.sourceFilenameModified.has(filename)) {
            InMemoryGlobalSymbolCache.sourceFilenameModified.delete(filename);
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, lastModified);
        } else {
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, lastModified);
        }
    }
}
