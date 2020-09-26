/* eslint-disable @typescript-eslint/no-explicit-any */
import * as fs from 'fs';
import * as path from 'path';
import * as crypto from 'crypto';

import { logMessage, isFileT } from "./extension";

import { expandLogicalCopyBookToFilenameOrEmpty } from "./opencopybook";
import { Hash } from "crypto";
import { InMemoryGlobalCachesHelper } from "./imemorycache";
import { VSCOBOLConfiguration } from "./configuration";
import { COBOLTokenStyle, ICOBOLSourceScanner } from "./cobolsourcescanner";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const lzjs = require('lzjs');


// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
// eslint-disable-next-line @typescript-eslint/no-explicit-any
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export function reviver(key: any, value: any): any {
    if (typeof value === 'object' && value !== null) {
        if (value.dataType === 'Map') {
            return new Map<string, COBOLSymbol>(value.value);
        }
    }
    return value;
}

// JSON callbacks to Map to something that can be serialised
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export function replacer(this: any, key: any, value: any): any {
    const originalObject = this[key];
    if (originalObject instanceof Map) {
        return {
            dataType: 'Map',
            value: Array.from(originalObject.entries()), // or with spread: value: [...originalObject]
        };
    } else {
        return value;
    }
}


export class COBOLGlobalSymbolTable {
    public lastModifiedTime = 0;
    public callableSymbols: Map<string, COBOLFileSymbol[]>;
    public isDirty = false;
    public classSymbols: Map<string, COBOLFileSymbol[]>;
    public methodSymbols: Map<string, COBOLFileSymbol[]>;

    public constructor() {
        this.callableSymbols = new Map<string, COBOLFileSymbol[]>();
        this.classSymbols = new Map<string, COBOLFileSymbol[]>();
        this.methodSymbols = new Map<string, COBOLFileSymbol[]>();
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLGlobalSymbolTable {
        return Object.assign(new COBOLGlobalSymbolTable(), d);
    }
}

export class COBOLSymbolTable {
    public lastModifiedTime = 0;
    public fileName = "";

    public variableSymbols: Map<string, COBOLSymbol>;
    public labelSymbols: Map<string, COBOLSymbol>;

    public constructor() {
        this.variableSymbols = new Map<string, COBOLSymbol>();
        this.labelSymbols = new Map<string, COBOLSymbol>();
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLSymbolTable {
        return Object.assign(new COBOLSymbolTable(), d);
    }
}

export class COBOLSymbolTableHelper {

    public static getCOBOLSymbolTable(qp: ICOBOLSourceScanner): COBOLSymbolTable {
        const config = VSCOBOLConfiguration.get();
        const st = new COBOLSymbolTable();
        st.fileName = qp.filename;
        st.lastModifiedTime = qp.lastModifiedTime;

        for (let i = 0; i < qp.tokensInOrder.length; i++) {
            const token = qp.tokensInOrder[i];
            switch (token.tokenType) {
                case COBOLTokenStyle.Constant:
                    if (config.parse_copybooks_for_references === false) {
                        st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.Variable:
                    if (config.parse_copybooks_for_references === false) {
                        st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.Paragraph:
                    if (config.parse_copybooks_for_references === false) {
                        st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.Section:
                    if (config.parse_copybooks_for_references === false) {
                        st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.ImplicitProgramId:
                    InMemoryGlobalCachesHelper.addSymbol(st.fileName, token.tokenNameLower, token.startLine);
                    break;
                case COBOLTokenStyle.ProgramId:
                    InMemoryGlobalCachesHelper.addSymbol(st.fileName, token.tokenNameLower, token.startLine);
                    break;
                case COBOLTokenStyle.EntryPoint:
                    InMemoryGlobalCachesHelper.addSymbol(st.fileName, token.tokenNameLower, token.startLine);
                    break;
                case COBOLTokenStyle.InterfaceId:
                    InMemoryGlobalCachesHelper.addClassSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
                case COBOLTokenStyle.EnumId:
                    InMemoryGlobalCachesHelper.addClassSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
                case COBOLTokenStyle.ClassId:
                    InMemoryGlobalCachesHelper.addClassSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
                case COBOLTokenStyle.MethodId:
                    InMemoryGlobalCachesHelper.addMethodSymbol(st.fileName, token.tokenName, token.startLine);
                    break;
            }
        }
        //logCOBOLChannelLine("- Creating symbol table for " + st.fileName + ", symbols found: " + qp.tokensInOrder.length + " -> " + (performance_now() - startTime).toFixed(2));
        return st;
    }


    private static getHashForFilename(filename: string) {
        const hash: Hash = crypto.createHash('sha256');
        hash.update(filename);
        return hash.digest('hex');
    }

    public static saveToFile(cacheDirectory: string, st: COBOLSymbolTable): void {
        const fn = path.join(cacheDirectory, this.getHashForFilename(st.fileName) + ".sym");

        fs.writeFileSync(fn, lzjs.compress(JSON.stringify(st, replacer)));
    }

    public static cacheUpdateRequired(cacheDirectory: string, nfilename: string): boolean {
        const filename = path.normalize(nfilename);

        // check memory first
        if (InMemorySymbolCache.has(filename)) {
            const cachedTable: COBOLSymbolTable | undefined = InMemorySymbolCache.get(filename);
            if (cachedTable !== undefined) {

                /* is the cache table still valid? */
                const stat4src = fs.statSync(filename);
                if (stat4src.mtimeMs === cachedTable.lastModifiedTime) {
                    return false;
                }
                return true;
            }
        }

        const fn: string = path.join(cacheDirectory, this.getHashForFilename(filename) + ".sym");
        const fnStat = isFileT(fn);
        if (fnStat[0]) {
            const stat4cache = fnStat[1];
            const stat4src = fs.statSync(filename);
            if (stat4cache.mtimeMs < stat4src.mtimeMs) {
                return true;
            }
            return false;
        }

        return true;
    }

    public static getSymbolTableGivenFile(cacheDirectory: string, nfilename: string): COBOLSymbolTable | undefined {
        const filename = path.normalize(nfilename);
        if (InMemorySymbolCache.has(filename)) {
            const cachedTable: COBOLSymbolTable | undefined = InMemorySymbolCache.get(filename);
            if (cachedTable !== undefined) {

                /* is the cache table still valid? */
                const stat4src = fs.statSync(filename);
                if (stat4src.mtimeMs === cachedTable.lastModifiedTime) {
                    return cachedTable;
                }
                InMemorySymbolCache.delete(filename);       /* drop the invalid cache */
            }
        }

        const fn: string = path.join(cacheDirectory, this.getHashForFilename(filename) + ".sym");
        const fnStat = isFileT(fn);
        if (fnStat[0]) {
            const stat4cache = fnStat[1];
            const stat4src = fs.statSync(filename);
            if (stat4cache.mtimeMs < stat4src.mtimeMs) {
                // never return a out of date cache
                fs.unlinkSync(fn);
                return undefined;
            }

            const str: string = fs.readFileSync(fn).toString();
            try {
                const cachableTable = JSON.parse(lzjs.decompress(str), reviver);
                InMemorySymbolCache.set(filename, cachableTable);
                return cachableTable;
            } catch {
                try {
                    fs.unlinkSync(fn);
                } catch {
                    logMessage(`Unable to remove symbol file : ${fn}`);
                }
                logMessage(` Symbol file removed : ${fn}`);

                return undefined;
            }
        }
        return undefined;
    }

    public static getSymbolTable_direct(nfilename: string): COBOLSymbolTable | undefined {
        const str: string = fs.readFileSync(nfilename).toString();
        try {
            return JSON.parse(lzjs.decompress(str), reviver);
        } catch {
            try {
                fs.unlinkSync(nfilename);
            } catch {
                logMessage(`Unable to remove symbol file : ${nfilename}`);
            }
            logMessage(`Symbol file removed ${nfilename}`);
        }
    }

}


export class COBOLFileSymbol {
    public filename: string | undefined;
    public lnum: number | undefined;

    public constructor(symbol?: string, lineNumber?: number) {
        this.filename = symbol;
        this.lnum = lineNumber;
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLFileSymbol {
        return Object.assign(new COBOLFileSymbol(), d);
    }
}

export class COBOLSymbol {
    public symbol: string | undefined;
    public lnum: number | undefined;

    public constructor(symbol?: string, lineNumber?: number) {
        this.symbol = symbol;
        this.lnum = lineNumber;
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    static fromJSON(d: any): COBOLSymbol {
        return Object.assign(new COBOLSymbol(), d);
    }
}


export const globalSymbolFilename = "globalsymbols.sym";
export const fileSymbolFilename = "filesymbols.sym";

const InMemorySymbolCache: Map<string, COBOLSymbolTable> = new Map<string, COBOLSymbolTable>();
export const InMemoryGlobalSymbolCache: COBOLGlobalSymbolTable = new COBOLGlobalSymbolTable();
