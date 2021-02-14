import * as vscode from 'vscode';

import { GlobalCachesHelper } from "./globalcachehelper";
import { VSCOBOLConfiguration } from './configuration';
import {  COBOLGlobalSymbolTable } from './cobolglobalcache';
import { COBOLCopyBookProvider } from './opencopybook';

export class COBOLCallTargetProvider implements vscode.DefinitionProvider {
    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        return this.resolveDefinitions(document, position, token);
    }

   readonly callRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
    // readonly enableUrlOpenBodge = false;

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private resolveDefinitions(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        const locations: vscode.Location[] = [];
        const config = VSCOBOLConfiguration.get();

        // if (VSCOBOLConfiguration.isOnDiskCachingEnabled() === false) {
        //     return locations;
        // }

        const theline = document.lineAt(position).text;

        if (theline.match(/.*(call|cancel|chain).*$/i)) {
            const wordRange = document.getWordRangeAtPosition(position, this.callRegEx);
            const word = wordRange ? document.getText(wordRange) : '';
            if (word !== "") {
                const img: COBOLGlobalSymbolTable = GlobalCachesHelper.getGlobalSymbolCache();
                const wordLower = word.toLocaleLowerCase();
                if (img.callableSymbols.has(wordLower)) {
                    const symbols = img.callableSymbols.get(wordLower);
                    if (symbols !== undefined) {
                        for (let i = 0; i < symbols.length; i++) {
                            const symbol = symbols[i];
                            if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                                const fullFilename = COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(symbol.filename,"",config);
                                if (this.getLocationGivenFile(fullFilename, symbol.lnum, locations)) {
                                    return locations;
                                }
                            }
                        }
                    }
                }
            }
        }

        return locations;
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
