import * as vscode from 'vscode';
import COBOLSourceScanner from './cobolsourcescanner';

import VSCOBOLSourceScanner from './vscobolscanner';
import { VSCOBOLConfiguration } from './configuration';
import { COBOLSymbol, COBOLSymbolTable } from './cobolglobalcache';
import { COBOLCopyBookProvider } from './opencopybook';
import { COBOLSymbolTableHelper } from './cobolglobalcache_file';
import { logException } from './extension';

export class CachedCOBOLSourceDefinition implements vscode.DefinitionProvider {
    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        return this.resolveDefinitions(document, position, token);
    }

    readonly sectionRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
    readonly variableRegEx = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');
    // readonly enableUrlOpenBodge = false;

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private resolveDefinitions(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        const locations: vscode.Location[] = [];
        const config = VSCOBOLConfiguration.get();

        if (config.parse_copybooks_for_references) {
            return locations;
        }

        const cacheDirectory = VSCOBOLSourceScanner.getCacheDirectory();
        if (cacheDirectory === undefined) {
            return locations;
        }

        const theline = document.lineAt(position).text;
        let qcp: COBOLSourceScanner | undefined = undefined;

        if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
            qcp = VSCOBOLSourceScanner.getCachedObject(document);
            if (qcp === undefined) {
                return locations;
            }

            const wordRange = document.getWordRangeAtPosition(position, this.sectionRegEx);
            const word = wordRange ? document.getText(wordRange) : '';
            const wordLower = word.toLocaleLowerCase();

            if (wordLower.length > 0) {
                /* iterate through all the known copybook references */
                for (const [key, value] of qcp.copyBooksUsed) {
                    try {
                        const fileName = COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(key, value.token.extraInformation, config);
                        if (fileName.length > 0) {
                            const symbolTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                            if (symbolTable !== undefined) {
                                const symbol: COBOLSymbol | undefined = symbolTable.labelSymbols.get(wordLower);
                                if (symbol !== undefined && symbol.lnum !== undefined) {
                                    this.getLocationGivenFile(fileName, symbol.lnum, locations);
                                }
                            }
                        }
                    }
                    catch (fe) {
                        logException(fe.message, fe);
                    }
                }
            }
        }

        /*
         * search inside on disk copybooks referenced by the current program
         * for variables
         */
        if (qcp === undefined) {
            qcp = VSCOBOLSourceScanner.getCachedObject(document);
        }

        if (qcp === undefined) {
            return locations;
        }

        const wordRange = document.getWordRangeAtPosition(position, this.variableRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        const wordLower = word.toLowerCase();

        if (wordLower.length > 0 && cacheDirectory !== undefined) {
            /* iterate through all the known copybook references */
            for (const [key, value] of qcp.copyBooksUsed) {
                try {
                    const fileName = COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(key, value.token.extraInformation, config);
                    if (fileName.length > 0) {
                        const symbolTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                        if (symbolTable !== undefined) {
                            const symbol: COBOLSymbol | undefined = symbolTable.variableSymbols.get(wordLower);
                            if (symbol !== undefined && symbol.lnum !== undefined) {
                                this.getLocationGivenFile(fileName, symbol.lnum, locations);
                            }
                        }
                    }
                }
                catch (fe) {
                    logException(fe.message, fe);
                }
            }
        }

        return locations;
    }

    private getLocationGivenFile(filename: string, linenumber: number, locations: vscode.Location[]): void {
        const uri = vscode.Uri.file(filename);

        const loc = new vscode.Location(
            uri,
            new vscode.Position(linenumber, 0)
        );
        locations.push(loc);
    }

}
