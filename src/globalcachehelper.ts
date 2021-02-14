import * as fs from 'fs';

// eslint-disable-next-line @typescript-eslint/no-var-requires
import { COBOLFileSymbol, InMemoryGlobalSymbolCache, COBOLGlobalSymbolTable } from './cobolglobalcache';

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

    private static addSymbolToCache(srcfilename: string, symbolUnchanged: string, lineNumber: number, symbolsCache: Map<string, COBOLFileSymbol[]>) {
        const symbol = symbolUnchanged.toLowerCase();
        if (symbolsCache.has(symbol)) {
            const symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(symbol);

            /* search the list of COBOLFileSymbols */
            if (symbolList !== undefined) {
                let found = false;
                for (let i = 0; i < symbolList.length; i++) {
                    if (symbolList[i].filename === srcfilename && symbolList[i].lnum === lineNumber) {
                        found = true;
                        break;
                    }
                }
                // not found?
                if (found === false) {
                    symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
                    InMemoryGlobalSymbolCache.isDirty = true;
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

    public static addSymbol(srcfilename: string, symbolUnchanged: string,  lineNumber = 1): void {
        GlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.callableSymbols);
    }

    public static addEntryPoint(srcfilename: string, symbolUnchanged: string, lineNumber: number): void {
        GlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.entryPoints);
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
