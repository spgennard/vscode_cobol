import * as vscode from 'vscode';

import COBOLSourceScanner from './cobolsourcescanner';

import { GlobalCachesHelper } from "./globalcachehelper";
import VSCOBOLSourceScanner from './vscobolscanner';
import { VSCOBOLConfiguration } from './configuration';
import { COBOLSymbol, COBOLGlobalSymbolTable, COBOLSymbolTable } from './cobolglobalcache';
import { COBOLCopyBookProvider } from './opencopybook';
import { COBOLSymbolTableHelper } from './cobolglobalcache_file';

export class CachedCOBOLSourceDefinition implements vscode.DefinitionProvider {
    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        return this.resolveDefinitions(document, position, token);
    }

    readonly sectionRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
    readonly variableRegEx = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');
    readonly callRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
    // readonly enableUrlOpenBodge = false;

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private resolveDefinitions(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        const locations: vscode.Location[] = [];
        const config = VSCOBOLConfiguration.get();

        if (VSCOBOLConfiguration.isOnDiskCachingEnabled() === false) {
            return locations;
        }

        const theline = document.lineAt(position).text;

        if (theline.match(/.*(call|cancel|chain).*$/i)) {
            const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document);
            if (qcp === undefined) {
                return locations;
            }
            const cacheDirectory = VSCOBOLSourceScanner.getCacheDirectory();
            const wordRange = document.getWordRangeAtPosition(position, this.callRegEx);
            const word = wordRange ? document.getText(wordRange) : '';
            if (cacheDirectory !== undefined && word !== "") {
                const img: COBOLGlobalSymbolTable = GlobalCachesHelper.getGlobalSymbolCache();
                const wordLower = word.toLocaleLowerCase();
                if (img.callableSymbols.has(wordLower) === false) {
                    GlobalCachesHelper.loadGlobalSymbolCache(cacheDirectory);
                }
                if (img.callableSymbols.has(wordLower)) {
                    const symbols = img.callableSymbols.get(wordLower);
                    if (symbols !== undefined) {
                        for (let i = 0; i < symbols.length; i++) {
                            const symbol = symbols[i];
                            if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                                if (this.getLocationGivenFile(symbol.filename, symbol.lnum, locations)) {
                                    return locations;
                                }
                            }
                        }
                    }
                }
            }
        }

        if (theline.match(/.*(invoke\s*|::)(.*$)/i)) {
            const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document);
            if (qcp === undefined) {
                return locations;
            }

            const wordRange = document.getWordRangeAtPosition(position, this.callRegEx);
            const word = wordRange ? document.getText(wordRange) : '';
            if (word !== "") {
                const img: COBOLGlobalSymbolTable = GlobalCachesHelper.getGlobalSymbolCache();
                const wordLower = word.toLocaleLowerCase();
                if (img.classSymbols.has(wordLower)) {
                    const symbols = img.classSymbols.get(wordLower);
                    if (symbols !== undefined) {
                        for (let i = 0; i < symbols.length; i++) {
                            const symbol = symbols[i];
                            if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                                if (this.getLocationGivenFile(symbol.filename, symbol.lnum, locations)) {
                                    return locations;
                                }
                            }
                        }
                    }
                }
            }
        }

        if (!config.parse_copybooks_for_references) {
            if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
                const cacheDirectory = VSCOBOLSourceScanner.getCacheDirectory();
                if (cacheDirectory === undefined) {
                    return locations;
                }
                const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document);
                if (qcp === undefined) {
                    return locations;
                }

                const wordRange = document.getWordRangeAtPosition(position, this.sectionRegEx);
                const word = wordRange ? document.getText(wordRange) : '';
                const wordLower = word.toLocaleLowerCase();

                if (wordLower.length > 0) {
                    /* iterater through all the known copybook references */
                    for (const [key, value] of qcp.copyBooksUsed) {
                        try {
                            const fileName = COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(key, value.token.extraInformation, config);
                            if (fileName.length > 0) {
                                const symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                                if (symboleTable !== undefined) {
                                    const symbol: COBOLSymbol | undefined = symboleTable.labelSymbols.get(wordLower);
                                    if (symbol !== undefined && symbol.lnum !== undefined) {
                                        if (this.getLocationGivenFile(fileName, symbol.lnum, locations)) {
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
                }
            }

            if (!config.parse_copybooks_for_references) {
                /*
                 * search inside on disk copybooks referenced by the current program
                 * for variables
                 */
                const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document);
                if (qcp === undefined) {
                    return locations;
                }

                const cacheDirectory = VSCOBOLSourceScanner.getCacheDirectory();
                const wordRange = document.getWordRangeAtPosition(position, this.variableRegEx);
                const word = wordRange ? document.getText(wordRange) : '';
                const wordLower = word.toLowerCase();

                if (wordLower.length > 0 && cacheDirectory !== undefined) {
                    /* iterater through all the known copybook references */
                    for (const [key, value] of qcp.copyBooksUsed) {
                        try {
                            const fileName = COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(key, value.token.extraInformation, config);
                            if (fileName.length > 0) {
                                const symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                                if (symboleTable !== undefined) {
                                    const symbol: COBOLSymbol | undefined = symboleTable.variableSymbols.get(wordLower);
                                    if (symbol !== undefined && symbol.lnum !== undefined) {
                                        if (this.getLocationGivenFile(fileName, symbol.lnum, locations)) {
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
                    return locations;
                }
            }

            return locations;
        }
    }

    private getLocationGivenFile(filename: string, linnumber: number, locations: vscode.Location[]): boolean {
        const uri = vscode.Uri.file(filename);

        // just do it quickly
        // if (this.enableUrlOpenBodge === false) {
        const loc = new vscode.Location(
            uri,
            new vscode.Position(linnumber, 0)
        );
        locations.push(loc);
        return false;
        // }
        // else {
        //     const l = linnumber;
        //     vscode.workspace
        //         .openTextDocument(uri)
        //         .then(vscode.window.showTextDocument)
        //         .then(() => {
        //             const a = vscode.window.activeTextEditor;
        //             if (a !== undefined) {
        //                 a.selection = new vscode.Selection(new vscode.Position(l, 0), new vscode.Position(l, 0));
        //             }
        //         });
        //     return true;
        // }
    }

}
