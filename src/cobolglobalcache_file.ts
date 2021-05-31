/* eslint-disable @typescript-eslint/no-explicit-any */
import * as fs from 'fs';
import * as path from 'path';
import * as crypto from 'crypto';

import { Hash } from "crypto";
import { COBOLSymbol, COBOLSymbolTable, InMemoryFileSymbolCache } from './cobolglobalcache';
import { COBOLFileUtils } from './fileutils';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const lzjs = require('lzjs');

// JSON callbacks to Map to something that can be serialized
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export function replacer(this: any, key: any, value: any): any {
    if (typeof value === 'bigint') {
        return value.toString();
    }

    const originalObject = this[key];
    if (originalObject instanceof Map) {
        return {
            dataType: 'Map',
            value: Array.from(originalObject.entries()), // or with spread: value: [...originalObject]
        };
    }

    return value;
}

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
// eslint-disable-next-line @typescript-eslint/no-explicit-any
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export function reviver(key: any, value: any): any {
    if (typeof value === 'object' && value !== null) {
        if (value.dataType === 'Map') {
            if (key === 'sourceFilenameModified') {
                return new Map<string, number>(value.value);
            }
            return new Map<string, COBOLSymbol>(value.value);
        }
    }
    return value;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function logMessage(mesg: string) {
    return;
}

export class COBOLSymbolTableHelper {
    private static getHashForFilename(filename: string) {
        const hash: Hash = crypto.createHash('sha256');
        hash.update(filename);
        return hash.digest('hex');
    }

    public static saveToFile(cacheDirectory: string, st: COBOLSymbolTable): void {
        const fn = path.join(cacheDirectory, this.getHashForFilename(st.fileName) + ".sym");

        fs.writeFileSync(fn, lzjs.compress(JSON.stringify(st, replacer)));
    }

    public static getSymbolTableGivenFile(cacheDirectory: string, nfilename: string): COBOLSymbolTable | undefined {
        const filename = path.normalize(nfilename);
        if (InMemoryFileSymbolCache.has(filename)) {
            const cachedTable: COBOLSymbolTable | undefined = InMemoryFileSymbolCache.get(filename);
            if (cachedTable !== undefined) {
                /* is the cache table still valid? */
                try {
                    const stat4src = fs.statSync(filename, { bigint: true });
                    if (stat4src.mtimeMs === cachedTable.lastModifiedTime) {
                        return cachedTable;
                    }
                }
                catch (e) {
                    //
                }
                InMemoryFileSymbolCache.delete(filename);       /* drop the invalid cache */
            }
        }

        const fn: string = path.join(cacheDirectory, this.getHashForFilename(filename) + ".sym");
        const fnStat = COBOLFileUtils.isFileT(fn);
        if (fnStat[0]) {
            try {
                const stat4cache = fnStat[1];
                const stat4src = fs.statSync(filename);
                if (stat4cache !== undefined && stat4cache.mtimeMs < stat4src.mtimeMs) {
                    // never return a out of date cache
                    try {
                        fs.unlinkSync(fn);
                    } catch (e) {
                        //
                    }                    return undefined;
                }
            }
            catch (e) {
                // never return a out of date cache
                try {
                    fs.unlinkSync(fn);
                } catch (e) {
                    //
                }
                return undefined;
            }

            const str: string = fs.readFileSync(fn).toString();
            try {
                const cacheableTable = JSON.parse(lzjs.decompress(str), reviver);
                InMemoryFileSymbolCache.set(filename, cacheableTable);
                return cacheableTable;
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
