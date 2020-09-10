import * as vscode from 'vscode';
import { expandLogicalCopyBookToFilenameOrEmpty } from './opencopybook';

import COBOLSourceScanner, { COBOLSymbolTableHelper, COBOLSymbolTable, COBOLSymbol, COBOLGlobalSymbolTable } from './cobolsourcescanner';
import { InMemoryGlobalCachesHelper } from "./imemorycache";
import VSQuickCOBOLParse from './vscobolscanner';
import { VSCOBOLConfiguration } from './configuration';

const sectionRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
const variableRegEx = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');
const callRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');

const enableUrlOpenBodge = false;

function openFileViaCommand(filename: string, linnumber: number, locations: vscode.Location[]): boolean {
    const uri = vscode.Uri.file(filename);

    // just do it quickly
    if (enableUrlOpenBodge === false) {
        const loc = new vscode.Location(
            uri,
            new vscode.Position(linnumber, 0)
        );
        locations.push(loc);
        return false;
    } else {
        const l = linnumber;
        vscode.workspace
            .openTextDocument(uri)
            .then(vscode.window.showTextDocument)
            .then(() => {
                const a = vscode.window.activeTextEditor;
                if (a !== undefined) {
                    a.selection = new vscode.Selection(new vscode.Position(l, 0), new vscode.Position(l, 0));
                }
            });
        return true;
    }
}

export function provideDefinition(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
    const locations: vscode.Location[] = [];

    if (VSCOBOLConfiguration.isOnDiskCachingEnabled() === false) {
        return locations;
    }

    const theline = document.lineAt(position.line).text;
    if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
        const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        const cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
        if (qcp === undefined) {
            return locations;
        }

        const wordRange = document.getWordRangeAtPosition(position, sectionRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        const wordLower = word.toLocaleLowerCase();

        if (wordLower.length > 0) {
            /* iterater through all the known copybook references */
            for (const [key, value] of qcp.copyBooksUsed) {
                try {
                    const fileName = expandLogicalCopyBookToFilenameOrEmpty(key, value.extraInformation);
                    if (fileName.length > 0) {
                        const symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                        if (symboleTable !== undefined) {
                            const symbol: COBOLSymbol | undefined = symboleTable.labelSymbols.get(wordLower);
                            if (symbol !== undefined && symbol.lnum !== undefined) {
                                if (openFileViaCommand(fileName, symbol.lnum, locations)) {
                                    return locations;
                                }
                            }
                        }
                    }
                }
                catch (fe) {
                    console.log(fe.message);
                    console.log(fe.stacktrace);
                }
            }
            InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(cacheDirectory);
        }
    }

    if (theline.match(/.*(invoke\s*|::)(.*$)/i)) {
        const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }

        const wordRange = document.getWordRangeAtPosition(position, callRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        if (word !== "") {
            const img: COBOLGlobalSymbolTable = InMemoryGlobalCachesHelper.getGlobalSymbolCache();
            const wordLower = word.toLocaleLowerCase();
            if (img.classSymbols.has(wordLower)) {
                const symbols = img.classSymbols.get(wordLower);
                if (symbols !== undefined) {
                    for (let i = 0; i < symbols.length; i++) {
                        const symbol = symbols[i];
                        if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                            if (openFileViaCommand(symbol.filename, symbol.lnum, locations)) {
                                return locations;
                            }
                        }
                    }
                }
            }
            const cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
            InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(cacheDirectory);
        }
    }

    if (theline.match(/.*(call|cancel|chain).*$/i)) {
        const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }
        const cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
        const wordRange = document.getWordRangeAtPosition(position, callRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        if (word !== "") {
            const img: COBOLGlobalSymbolTable = InMemoryGlobalCachesHelper.getGlobalSymbolCache();
            const wordLower = word.toLocaleLowerCase();
            if (img.callableSymbols.has(wordLower) === false) {
                InMemoryGlobalCachesHelper.loadInMemoryGlobalSymbolCaches(cacheDirectory);
                InMemoryGlobalCachesHelper.loadInMemoryGlobalFileCache(cacheDirectory);
            }
            if (img.callableSymbols.has(wordLower)) {
                const symbols = img.callableSymbols.get(wordLower);
                if (symbols !== undefined) {
                    for (let i = 0; i < symbols.length; i++) {
                        const symbol = symbols[i];
                        if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                            if (openFileViaCommand(symbol.filename, symbol.lnum, locations)) {
                                return locations;
                            }
                        }
                    }
                }
            }
            InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(cacheDirectory);
        }
    }

    /*
     * search inside on disk copybooks referenced by the current program
     * for variables
     */
    const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
    if (qcp === undefined) {
        return locations;
    }

    const cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
    const wordRange = document.getWordRangeAtPosition(position, variableRegEx);
    const word = wordRange ? document.getText(wordRange) : '';
    const wordLower = word.toLowerCase();

    if (wordLower.length > 0) {
        /* iterater through all the known copybook references */
        for (const [key, value] of qcp.copyBooksUsed) {
            try {

                const fileName = expandLogicalCopyBookToFilenameOrEmpty(key, value.extraInformation);
                if (fileName.length > 0) {
                    const symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                    if (symboleTable !== undefined) {
                        const symbol: COBOLSymbol | undefined = symboleTable.variableSymbols.get(wordLower);
                        if (symbol !== undefined && symbol.lnum !== undefined) {
                            if (openFileViaCommand(fileName, symbol.lnum, locations)) {
                                return locations;
                            }
                        }
                    }
                }
            }
            catch
            {
                // should not happen but if it does, continue on to the next copybook reference
            }
        }
        InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(cacheDirectory);
        return locations;
    }


    return locations;
}
