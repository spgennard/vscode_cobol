import { TextDocument, Definition, Position, CancellationToken, ProviderResult, workspace, Uri, Range } from 'vscode';
import * as vscode from 'vscode';
import QuickCOBOLParse, { COBOLTokenStyle, COBOLToken, COBOLSymbolTableHelper, COBOLSymbolTable, COBOLSymbol, InMemoryGlobalSymbolCacheHelper } from './cobolquickparse';
import { expandLogicalCopyBookToFilenameOrEmpty } from './opencopybook';
import { isOutlineEnabled, logCOBOLChannelLine } from './extension';
import path = require("path");

function getFuzzyVariableSearch(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var fuzzyVarOn = editorConfig.get<boolean>('fuzzy_variable_search');
    if (fuzzyVarOn === undefined || fuzzyVarOn === null) {
        fuzzyVarOn = false;
    }
    return fuzzyVarOn;
}

export function getCopyBookSearch(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var copybookSearch = editorConfig.get<boolean>('copybook_search');
    if (copybookSearch === undefined || copybookSearch === null) {
        copybookSearch = false;
    }
    return copybookSearch;
}

function getFuzzyVariable(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, new RegExp("[a-zA-Z0-9_-]+"));
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }
    let workLower = word.toLocaleLowerCase();

    for (let i = 1; i < document.lineCount; i++) {
        let lineText = document.lineAt(i).text;

        // TODO - need to handle inline comments too
        if (lineText.length > 7 && lineText[6] === '*') {
            continue;
        }

        // time to leave
        if (lineText.match(/.*(procedure\s+division).*$/i)) {
            return undefined;
        }

        let wordIndex = lineText.toLowerCase().indexOf(workLower);
        if (wordIndex !== -1) {
            let leftOfWord = lineText.substr(0, wordIndex).trim();

            // fuzzy match for variable
            if (leftOfWord.match(/^[0-9 ]+$/i)) {
                return new vscode.Location(
                    document.uri,
                    new vscode.Position(i, wordIndex)
                );
            }

            // fuzzy match for a FD declaration
            if (leftOfWord.toLocaleLowerCase() === "fd") {
                return new vscode.Location(
                    document.uri,
                    new vscode.Position(i, wordIndex)
                );
            }
        }
    }
    return undefined;
}

const sectionRegEx: RegExp = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');

function getSectionOrParaLocation(document: vscode.TextDocument, uri: vscode.Uri, sf: QuickCOBOLParse, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, sectionRegEx);
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    try {
        if (sf.sections.has(word)) {
            let token = sf.sections.get(word);
            if (token !== undefined) {
                let srange = new vscode.Position(token.startLine, token.startColumn);
                return new vscode.Location(uri, srange);
            }
        }
    }
    catch (e) {
        logCOBOLChannelLine(e);
    }

    try {
        if (sf.paragraphs.has(word)) {
            let token = sf.paragraphs.get(word);
            if (token !== undefined) {
                let srange = new vscode.Position(token.startLine, token.startColumn);
                return new vscode.Location(uri, srange);
            }
        }
    }
    catch (e) {
        logCOBOLChannelLine(e);
    }
    return undefined;
}

const variableRegEx: RegExp = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');

function getVariableInCurrentDocument(locations: vscode.Location[], document: vscode.TextDocument, uri: vscode.Uri, sf: QuickCOBOLParse, position: vscode.Position): boolean {
    let wordRange = document.getWordRangeAtPosition(position, variableRegEx);
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return false;
    }

    let tokenLower: string = word.toLowerCase();
    if (sf.constantsOrVariables.has(tokenLower) === false) {
        return false;
    }

    let tokens: COBOLToken[] | undefined = sf.constantsOrVariables.get(tokenLower);
    if (tokens === undefined) {
        return false;
    }

    for (var i = 0; i < tokens.length; i++) {
        let token: COBOLToken = tokens[i];

        if (word === token.tokenName || word === token.description) {
            switch (token.tokenType) {
                case COBOLTokenStyle.Constant:
                    {
                        let srange = new vscode.Position(token.startLine, token.startColumn);
                        locations.push(new vscode.Location(uri, srange));
                        break;
                    }
                case COBOLTokenStyle.Variable:
                    {
                        let srange = new vscode.Position(token.startLine, token.startColumn);
                        locations.push(new vscode.Location(uri, srange));
                        break;
                    }
            }
        }
    }

    if (locations.length === 0) {
        return false;
    }
    return true;
}

const callRegEx: RegExp = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');

function getCallTarget(document: vscode.TextDocument, sf: QuickCOBOLParse, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, callRegEx);
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let workLower = word.toLowerCase();
    if (sf.callTargets.has(workLower)) {
        let token: COBOLToken | undefined = sf.callTargets.get(workLower);
        if (token !== undefined) {
            let srange = new vscode.Position(token.startLine, token.startColumn);
            return new vscode.Location(document.uri, srange);
        }
    }
    return undefined;
}

function delay(ms: number) {
    if (ms > 0) {
        return new Promise(resolve => setTimeout(resolve, ms));
    }
}

const enableUrlOpenBodge: boolean = false;

function openFileViaCommand(filename: string, linnumber: number, locations: vscode.Location[]): boolean {
    let uri = vscode.Uri.file(filename);

    // just do it quickly
    if (enableUrlOpenBodge === false) {
        let startPos = new vscode.Position(linnumber, 0);
        locations.push(new vscode.Location(uri, new vscode.Range(startPos, startPos)));
        return false;
    }

    if (enableUrlOpenBodge && filename.indexOf(".") === -1) {
        let l = linnumber;
        vscode.workspace
            .openTextDocument(uri)
            .then(vscode.window.showTextDocument)
            .then(() => {
                let a = vscode.window.activeTextEditor;
                if (a !== undefined) {
                    a.selection = new vscode.Selection(new vscode.Position(l, 0), new vscode.Position(l, 0));
                }
            });
    }

    return true;
}

export function provideDefinition(document: TextDocument, position: Position, token: CancellationToken): ProviderResult<Definition> {
    let locations: vscode.Location[] = [];
    let loc;
    let qcp: QuickCOBOLParse | undefined = QuickCOBOLParse.getCachedObject(document, document.fileName);

    if (qcp === undefined) {
        return locations;
    }

    let theline = document.lineAt(position.line).text;
    if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
        loc = getSectionOrParaLocation(document, document.uri, qcp, position);
        if (loc) {
            locations.push(loc);
            return locations;
        }

        /* search for targets in a copybook */
        if (getCopyBookSearch()) {
            QuickCOBOLParse.processOneFile(qcp);    /* ensure we have all the copybooks in the symbol cache */

            let wordRange = document.getWordRangeAtPosition(position, sectionRegEx);
            let word = wordRange ? document.getText(wordRange) : '';

            if (word.length > 0) {
                /* iterater through all the known copybook references */
                for (let [key, value] of qcp.copyBooksUsed) {
                    try {

                        let fileName = expandLogicalCopyBookToFilenameOrEmpty(key);
                        if (fileName.length > 0) {
                            let symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(fileName);
                            if (symboleTable !== undefined) {
                                let symbol: COBOLSymbol | undefined = symboleTable.labelSymbols.get(word);
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
                InMemoryGlobalSymbolCacheHelper.saveInMemoryGlobalSymbolCache();
                return locations;
            }
        }
    }

    if (theline.match(/.*(call|cancel|chain).*$/i)) {
        loc = getCallTarget(document, qcp, position);
        if (loc !== undefined) {
            locations.push(loc);
            return locations;
        }
    }

    /* is it a known variable? */
    if (getVariableInCurrentDocument(locations, document, document.uri, qcp, position)) {
        return locations;
    }

    /* search inside on disk copybooks referenced by the current program
     * for variables
     */
    if (getCopyBookSearch()) {
        QuickCOBOLParse.processOneFile(qcp);    /* ensure we have all the copybooks in the symbol cache */

        let wordRange = document.getWordRangeAtPosition(position, variableRegEx);
        let word = wordRange ? document.getText(wordRange) : '';

        if (word.length > 0) {
            /* iterater through all the known copybook references */
            for (let [key, value] of qcp.copyBooksUsed) {
                try {

                    let fileName = expandLogicalCopyBookToFilenameOrEmpty(key);
                    if (fileName.length > 0) {
                        let symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(fileName);
                        if (symboleTable !== undefined) {
                            let symbol: COBOLSymbol | undefined = symboleTable.variableSymbols.get(word);
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
            InMemoryGlobalSymbolCacheHelper.saveInMemoryGlobalSymbolCache();
            return locations;
        }

    }

    /* fuzzy search is not using the parser and it give false positive's, so lets
     * disable it by default but allow it to be re-enabled via config setting
     */
    if (getFuzzyVariableSearch()) {
        /* let's see if is a variable via our fuzzy matcher (catch all/brute force) */
        loc = getFuzzyVariable(document, position);
        if (loc) {
            locations.push(loc);
            return locations;
        }
    }

    return locations;

}

