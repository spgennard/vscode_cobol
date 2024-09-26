/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import path from "path";
import { COBOLFileSymbol, COBOLWorkspaceFile } from "./cobolglobalcache";
import { IExternalFeatures } from "./externalfeatures";
import { InMemoryGlobalCacheHelper, InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { ICOBOLSettings } from "./iconfiguration";

export enum TypeCategory {
    ClassId = "T",
    InterfaceId = "I",
    EnumId = "E"
}

export class COBOLWorkspaceSymbolCacheHelper {
    private static removeAllProgramSymbols(srcfilename: string, symbolsCache: Map<string, COBOLFileSymbol[]>): void {
        for (const [key] of symbolsCache) {
            const symbolList: COBOLFileSymbol[] | undefined = symbolsCache.get(key);
            if (symbolList !== undefined) {
                const newSymbols: COBOLFileSymbol[] = [];
                for (let i = 0; i < symbolList.length; i++) {
                    if (symbolList[i].filename !== srcfilename) {
                        newSymbols.push(symbolList[i]);
                    }
                }

                if (newSymbols.length !== symbolList.length) {
                    if (newSymbols.length !== 0) {
                        symbolsCache.set(key, newSymbols);
                    } else {
                        symbolsCache.delete(key);
                    }
                }
            }
        }
    }

    private static addSymbolToCache(srcfilename: string, symbolUnchanged: string, lineNumber: number, symbolsCache: Map<string, COBOLFileSymbol[]>): void {
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
                        if (symbolList[i].linenum !== 1) {
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
                    symbolList[foundLast].linenum = lineNumber;
                    InMemoryGlobalSymbolCache.isDirty = true;
                } else {
                    // if we have multiple, never update the filename symbol which has a line number of 1
                    if (foundLastNonFileSymbol !== -1) {
                        symbolList[foundLastNonFileSymbol].linenum = lineNumber;
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


    private static readonly literalRegex = /^([a-zA-Z-0-9_]*[a-zA-Z0-9]|([#]?[0-9a-zA-Z]+[a-zA-Z-0-9_]*[a-zA-Z0-9]))$/g;

    private static isValidLiteral(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }

        if (id.match(COBOLWorkspaceSymbolCacheHelper.literalRegex)) {
            return true;
        }

        return false;
    }

    public static addCalableSymbol(srcfilename: string, symbolUnchanged: string, lineNumber: number): void {
        if (srcfilename.length === 0 || symbolUnchanged.length === 0) {
            return;
        }
        
        const fileName = InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename);
        const fileNameNoExt = path.basename(fileName, path.extname(fileName));
        const callableSymbolFromFilenameLower = fileNameNoExt.toLowerCase();

        if (COBOLWorkspaceSymbolCacheHelper.isValidLiteral(symbolUnchanged) === false) {
            return;
        }
        
        let addSymbol = lineNumber !== 0;
        if (symbolUnchanged.toLowerCase() === callableSymbolFromFilenameLower) {
            InMemoryGlobalSymbolCache.defaultCallableSymbols.set(callableSymbolFromFilenameLower, srcfilename);
            return;
        } else {
            addSymbol = true;
        }
        
        // drop the defaultCallableSymbols if it has a real one
        if (addSymbol) {
            if (InMemoryGlobalSymbolCache.defaultCallableSymbols.has(callableSymbolFromFilenameLower)) {
                InMemoryGlobalSymbolCache.defaultCallableSymbols.delete(callableSymbolFromFilenameLower);
                COBOLWorkspaceSymbolCacheHelper.addSymbolToCache(fileName, symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.callableSymbols);
                return;
            }
        }

        if (InMemoryGlobalSymbolCache.defaultCallableSymbols.has(callableSymbolFromFilenameLower) === false) {
            COBOLWorkspaceSymbolCacheHelper.addSymbolToCache(fileName, symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.callableSymbols);
        }
    }

    public static addEntryPoint(srcfilename: string, symbolUnchanged: string, lineNumber: number): void {
        if (srcfilename.length === 0 || symbolUnchanged.length === 0) {
            return;
        }
        COBOLWorkspaceSymbolCacheHelper.addSymbolToCache(
            InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename), symbolUnchanged, lineNumber, InMemoryGlobalSymbolCache.entryPoints);
    }

    public static addReferencedCopybook(copybook: string, fullInFilename: string) {
        const inFilename = InMemoryGlobalCacheHelper.getFilenameWithoutPath(fullInFilename);
        const encodedKey = `${copybook},${inFilename}`;
        if (!InMemoryGlobalSymbolCache.knownCopybooks.has(encodedKey)) {
            InMemoryGlobalSymbolCache.knownCopybooks.set(encodedKey, copybook);
            InMemoryGlobalSymbolCache.isDirty = true;
        }
    }

    public static removeAllCopybookReferences(fullInFilename: string) {
        const inFilename = InMemoryGlobalCacheHelper.getFilenameWithoutPath(fullInFilename);
        const keysToRemove = [];
        for (const [encodedKey,] of InMemoryGlobalSymbolCache.knownCopybooks) {
            const keys = encodedKey.split(",");
            if (keys[1] === inFilename) {
                keysToRemove.push(encodedKey);
            }
        }

        for (const key of keysToRemove) {
            InMemoryGlobalSymbolCache.knownCopybooks.delete(key);
        }
    }

    public static addClass(srcfilename: string, symbolUnchanged: string, lineNumber: number, category: TypeCategory): void {
        let map: Map<string, COBOLFileSymbol[]> = InMemoryGlobalSymbolCache.types;

        switch (category) {
            case TypeCategory.ClassId: map = InMemoryGlobalSymbolCache.types; break;
            case TypeCategory.InterfaceId: map = InMemoryGlobalSymbolCache.interfaces; break;
            case TypeCategory.EnumId: map = InMemoryGlobalSymbolCache.enums; break;
        }

        COBOLWorkspaceSymbolCacheHelper.addSymbolToCache(
            InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename), symbolUnchanged, lineNumber, map);
    }

    public static removeAllPrograms(srcfilename: string): void {
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramSymbols(InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename),
            InMemoryGlobalSymbolCache.callableSymbols);
    }

    public static removeAllProgramEntryPoints(srcfilename: string): void {
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramSymbols(InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename),
            InMemoryGlobalSymbolCache.entryPoints);
    }

    public static removeAllTypes(srcfilename: string): void {
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramSymbols(InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename),
            InMemoryGlobalSymbolCache.types);
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramSymbols(InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename),
            InMemoryGlobalSymbolCache.enums);
        COBOLWorkspaceSymbolCacheHelper.removeAllProgramSymbols(InMemoryGlobalCacheHelper.getFilenameWithoutPath(srcfilename),
            InMemoryGlobalSymbolCache.interfaces);
    }

    public static loadGlobalCacheFromArray(settings: ICOBOLSettings, symbols: string[], clear: boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.callableSymbols.clear();
        }

        for (const symbol of symbols) {
            const symbolValues = symbol.split(",");
            if (symbolValues.length === 2) {
                COBOLWorkspaceSymbolCacheHelper.addCalableSymbol(symbolValues[1], symbolValues[0], 0);
            }
            
            if (symbolValues.length === 3) {
                COBOLWorkspaceSymbolCacheHelper.addCalableSymbol(symbolValues[1], symbolValues[0], Number.parseInt(symbolValues[2],10));
            }
        }
        
    }

    public static loadGlobalEntryCacheFromArray(settings: ICOBOLSettings, symbols: string[], clear: boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.entryPoints.clear();
        }

        for (const symbol of symbols) {
            const symbolValues = symbol.split(",");
            if (symbolValues.length === 3) {
                COBOLWorkspaceSymbolCacheHelper.addEntryPoint(symbolValues[1], symbolValues[0], Number.parseInt(symbolValues[2], 10));
            }
        }
        
    }

    public static loadGlobalKnownCopybooksFromArray(settings: ICOBOLSettings, copybookValues: string[], clear: boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.knownCopybooks.clear();
        }

        for (const symbol of copybookValues) {
            const encodedKey = symbol.split(",");
            if (encodedKey.length === 2) {
                InMemoryGlobalSymbolCache.knownCopybooks.set(symbol, encodedKey[1]);
            }
        }
        
    }

    public static loadGlobalTypesCacheFromArray(settings: ICOBOLSettings, symbols: string[], clear: boolean): void {
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
                    case "I": cat = TypeCategory.InterfaceId; break;
                    case "T": cat = TypeCategory.ClassId; break;
                    case "E": cat = TypeCategory.EnumId; break;
                }
                COBOLWorkspaceSymbolCacheHelper.addClass(symbolValues[2], symbolValues[1], Number.parseInt(symbolValues[3], 10), cat);
            }
        }
        
    }

    public static loadFileCacheFromArray(settings: ICOBOLSettings, externalFeatures: IExternalFeatures, files: string[], clear: boolean): void {
        if (clear) {
            InMemoryGlobalSymbolCache.sourceFilenameModified.clear();
        }

        for (const symbol of files) {
            const fileValues = symbol.split(",");
            if (fileValues.length === 2) {
                const ms = BigInt(fileValues[0]);
                const fullDir = externalFeatures.getFullWorkspaceFilename(fileValues[1], ms);
                if (fullDir !== undefined) {
                    const cws = new COBOLWorkspaceFile(ms, fileValues[1]);
                    InMemoryGlobalSymbolCache.sourceFilenameModified.set(fullDir as string, cws);
                } else {
                    COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(fileValues[1]);
                    COBOLWorkspaceSymbolCacheHelper.removeAllTypes(fileValues[1]);
                    // externalFeatures.logMessage(`loadFileCacheFromArray, could not ${fileValues[1]} with ${ms}`);
                }
            }
        }
    }
}
