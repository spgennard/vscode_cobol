import * as fs from 'fs';
import * as path from 'path';
import { isFile } from "./extension";
let lzjs = require('lzjs');
import { globalSymbolFilename, InMemoryGlobalSymbolCache, COBOLGlobalSymbolTable, reviver, fileSymbolFilename, InMemoryGlobalFileCache, COBOLGlobalFileTable, replacer, COBOLFileSymbol } from "./cobolquickparse";

export class InMemoryGlobalCachesHelper {
    public static loadInMemoryGlobalSymbolCaches(cacheDirectory: string) {
        //let symbolDir = COBOLSymbolTableHelper.getCacheDirectory();
        let fn: string = path.join(cacheDirectory, globalSymbolFilename);
        if (isFile(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            if (stat4cache.mtimeMs !== InMemoryGlobalSymbolCache.lastModifiedTime) {
                try {

                    let str: string = fs.readFileSync(fn).toString();
                    let cachableTable: COBOLGlobalSymbolTable = JSON.parse(lzjs.decompress(str), reviver);
                    InMemoryGlobalSymbolCache.callableSymbols = cachableTable.callableSymbols;
                    InMemoryGlobalSymbolCache.lastModifiedTime = stat4cache.mtimeMs;
                    InMemoryGlobalSymbolCache.isDirty = false;
                    return true;
                }
                catch (e) {
                    fs.unlinkSync(fn);
                    return false;
                }
            }
            return false;
        }
    }


    public static loadInMemoryGlobalFileCache(cacheDirectory: string) {
        let fn: string = path.join(cacheDirectory, fileSymbolFilename);

        if (isFile(fn)) {
            let stat4cache: fs.Stats = fs.statSync(fn);
            if (stat4cache.mtimeMs !== InMemoryGlobalFileCache.lastModifiedTime) {
                try {

                    let str: string = fs.readFileSync(fn).toString();
                    let cachableTable: COBOLGlobalFileTable = JSON.parse(lzjs.decompress(str), reviver);
                    InMemoryGlobalFileCache.callableFileSymbols = cachableTable.callableFileSymbols;
                    InMemoryGlobalFileCache.copybookFileSymbols = cachableTable.copybookFileSymbols;
                    InMemoryGlobalFileCache.lastModifiedTime = stat4cache.mtimeMs;
                    InMemoryGlobalFileCache.isDirty = false;
                    return true;
                }
                catch (e) {
                    fs.unlinkSync(fn);
                    return false;
                }
            }
            return false;
        }

    }


    public static saveInMemoryGlobalCaches(cacheDirectory: string) {

        if (InMemoryGlobalSymbolCache.isDirty) {
            let fnGlobalSymbolFilename: string = path.join(cacheDirectory, globalSymbolFilename);
            fs.writeFileSync(fnGlobalSymbolFilename, lzjs.compress(JSON.stringify(InMemoryGlobalSymbolCache, replacer)));
            InMemoryGlobalSymbolCache.isDirty = false;
        }

        if (InMemoryGlobalFileCache.isDirty) {
            let fnFileSymbolFilename: string = path.join(cacheDirectory, fileSymbolFilename);
            fs.writeFileSync(fnFileSymbolFilename, lzjs.compress(JSON.stringify(InMemoryGlobalFileCache, replacer)));
            InMemoryGlobalFileCache.isDirty = false;
        }
    }


    private static addSymbolToCache(srcfilename: string, symbolUnchanged: string, lineNumber: number, symbolsCache: Map<string, COBOLFileSymbol[]>) {
        let symbol = symbolUnchanged.toLowerCase();
        if (symbolsCache.has(symbol)) {
            let symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(symbol);

            /* search the list of COBOLFileSymbols */
            if (symbolList !== undefined) {
                let found: boolean = false;
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
            }
        }
        let symbolList = [];
        symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
        symbolsCache.set(symbol, symbolList);
        InMemoryGlobalSymbolCache.isDirty = true;
        return;
    }


    private static addSymbolFileToCache(srcfilename: string, symbolsCache: Map<string, COBOLFileSymbol[]>) {
        let symbol: string = srcfilename, lineNumber: number = 0;
        if (symbolsCache.has(symbol)) {
            let symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(symbol);

            /* search the list of COBOLFileSymbols */
            if (symbolList !== undefined) {
                let found: boolean = false;
                for (let i = 0; i < symbolList.length; i++) {
                    if (symbolList[i].filename === srcfilename && symbolList[i].lnum === lineNumber) {
                        found = true;
                        break;
                    }
                }
                // not found?
                if (found === false) {
                    symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
                    InMemoryGlobalFileCache.isDirty = true;
                }
            }
        }
        let symbolList = [];
        symbolList.push(new COBOLFileSymbol(srcfilename, lineNumber));
        symbolsCache.set(symbol, symbolList);
        InMemoryGlobalFileCache.isDirty = true;
        return;
    }


    public static addClassSymbol(srcfilename: string, symbolUnchanged: string, lineNumber: number) {
        let symbolsCache = InMemoryGlobalSymbolCache.classSymbols;

        InMemoryGlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, symbolsCache);
    }


    public static addMethodSymbol(srcfilename: string, symbolUnchanged: string, lineNumber: number) {
        let symbolsCache = InMemoryGlobalSymbolCache.methodSymbols;
        InMemoryGlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, symbolsCache);
    }


    public static addSymbol(srcfilename: string, symbolUnchanged: string, lineNumber: number) {
        let symbolsCache = InMemoryGlobalSymbolCache.callableSymbols;
        InMemoryGlobalCachesHelper.addSymbolToCache(srcfilename, symbolUnchanged, lineNumber, symbolsCache);
    }


    public static addCopyBookFilename(srcfilename: string) {
        let symbolsCache = InMemoryGlobalFileCache.copybookFileSymbols;
        InMemoryGlobalCachesHelper.addSymbolFileToCache(srcfilename, symbolsCache);
    }


    public static getGlobalSymbolCache(): COBOLGlobalSymbolTable {
        return InMemoryGlobalSymbolCache;
    }
}
