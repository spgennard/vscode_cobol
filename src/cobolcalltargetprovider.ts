import * as vscode from "vscode";

import { VSCOBOLConfiguration } from "./vsconfiguration";
import { COBOLGlobalSymbolTable } from "./cobolglobalcache";
import { COBOLCopyBookProvider } from "./opencopybook";
import { InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { IExternalFeatures } from "./externalfeatures";

export class COBOLCallTargetProvider implements vscode.DefinitionProvider {

    private features: IExternalFeatures;

    constructor(features: IExternalFeatures) {
        this.features = features;
    }

    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        return this.resolveDefinitions(document, position, token);
    }

    readonly callRegEx = new RegExp("[0-9a-zA-Z][a-zA-Z0-9-_]*");
    // readonly enableUrlOpenBodge = false;

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private resolveDefinitions(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        const locations: vscode.Location[] = [];
        const config = VSCOBOLConfiguration.get();

        const theline = document.lineAt(position).text;

        if (theline.match(/.*(call|cancel|chain).*$/i)) {
            const wordRange = document.getWordRangeAtPosition(position, this.callRegEx);
            const word = wordRange ? document.getText(wordRange) : "";
            if (word !== "") {
                const img: COBOLGlobalSymbolTable = InMemoryGlobalSymbolCache;
                const wordLower = word.toLowerCase();

                if (img.defaultCallableSymbols.has(wordLower)) {
                    const fullfileName = img.defaultCallableSymbols.get(wordLower);
                    if (fullfileName !== undefined) {
                        this.getLocationGivenFile(fullfileName, 0, locations);
                   }
                }

                if (img.callableSymbols.has(wordLower)) {
                    const symbols = img.callableSymbols.get(wordLower);
                    if (symbols !== undefined) {
                        for (let i = 0; i < symbols.length; i++) {
                            const symbol = symbols[i];
                            if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                                const fullFilename = COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(symbol.filename, "", config, this.features);
                                if (fullFilename.length !== 0) {
                                    this.getLocationGivenFile(fullFilename, symbol.lnum, locations);
                                }
                            }
                        }
                    }
                }

                if (img.entryPoints.has(wordLower)) {
                    const symbols = img.entryPoints.get(wordLower);
                    if (symbols !== undefined) {
                        for (let i = 0; i < symbols.length; i++) {
                            const symbol = symbols[i];
                            if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                                const fullFilename = COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(symbol.filename, "", config, this.features);
                                if (fullFilename.length !== 0) {
                                    this.getLocationGivenFile(fullFilename, symbol.lnum, locations);
                                }
                            }
                        }
                    }
                }

            }
        }

        return locations;
    }

    private getLocationGivenFile(filename: string, linnumber: number, locations: vscode.Location[]): void {
        const uri = vscode.Uri.file(filename);
        const loc = new vscode.Location(
            uri,
            new vscode.Position(linnumber, 0)
        );
        locations.push(loc);

    }

}
