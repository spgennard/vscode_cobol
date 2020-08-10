import * as vscode from 'vscode';
import { expandLogicalCopyBookToFilenameOrEmpty } from './opencopybook';
import { logMessage } from './extension';
import COBOLSourceScanner, { COBOLTokenStyle, COBOLToken, COBOLSymbolTableHelper, COBOLSymbolTable, COBOLSymbol, COBOLGlobalSymbolTable } from './cobolsourcescanner';
import { InMemoryGlobalCachesHelper } from "./imemorycache";
import VSQuickCOBOLParse from './vscobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';

const sectionRegEx: RegExp = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
const variableRegEx: RegExp = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');
const callRegEx: RegExp = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
const classRegEx: RegExp = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
const methodRegEx: RegExp = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');

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


function getSectionOrParaLocation(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, sectionRegEx);
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let wordLower = word.toLowerCase();

    try {

        if (sf.sections.has(wordLower)) {
            let token = sf.sections.get(wordLower);
            if (token !== undefined) {
                let srange = new vscode.Position(token.startLine, token.startColumn);
                let uri = vscode.Uri.file(token.filename);
                return new vscode.Location(uri, new vscode.Range(srange, srange));
            }
        }

    }
    catch (e) {
        logMessage(e);
    }

    try {
        if (sf.paragraphs.has(wordLower)) {
            let token = sf.paragraphs.get(wordLower);
            if (token !== undefined) {
                let srange = new vscode.Position(token.startLine, token.startColumn);
                let uri = vscode.Uri.file(token.filename);
                return new vscode.Location(uri, new vscode.Range(srange, srange));
            }
        }
    }
    catch (e) {
        logMessage(e);
    }
    return undefined;
}

function isValidKeyword(keyword: string): boolean {
    return cobolKeywordDictionary.containsKey(keyword);
}

function getVariableInCurrentDocument(locations: vscode.Location[], document: vscode.TextDocument, position: vscode.Position): boolean {
    let wordRange = document.getWordRangeAtPosition(position, variableRegEx);
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return false;
    }

    let tokenLower: string = word.toLowerCase();
    if (isValidKeyword(tokenLower)) {
        return false;
    }

    let sf: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
    if (sf === undefined) {
        return false;
    }

    let tokens: COBOLToken[] | undefined = sf.constantsOrVariables.get(tokenLower);
    if (tokens === undefined || tokens.length === 0) {
        return false;
    }

    for (let i = 0; i < tokens.length; i++) {
        let token: COBOLToken = tokens[i];

        switch (token.tokenType) {
            case COBOLTokenStyle.Constant:
                {
                    let srange = new vscode.Position(token.startLine, token.startColumn);
                    let uri = vscode.Uri.file(token.filename);
                    locations.push(new vscode.Location(uri, srange));
                    break;
                }
            case COBOLTokenStyle.Variable:
                {
                    let srange = new vscode.Position(token.startLine, token.startColumn);
                    let uri = vscode.Uri.file(token.filename);
                    locations.push(new vscode.Location(uri, srange));
                    break;
                }
        }
    }

    if (locations.length === 0) {
        return false;
    }
    return true;
}

function getGenericTarget(queryRegEx: RegExp, tokenMap: Map<string, COBOLToken>, document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, queryRegEx);
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let workLower = word.toLowerCase();
    if (tokenMap.has(workLower)) {
        let token: COBOLToken | undefined = tokenMap.get(workLower);
        if (token !== undefined) {
            let srange = new vscode.Position(token.startLine, token.startColumn);
            let uri = vscode.Uri.file(token.filename);
            return new vscode.Location(uri, srange);
        }
    }
    return undefined;
}

function getClassTarget(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
    return getGenericTarget(classRegEx, sf.classes, document, position);
}

function getMethodTarget(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
    return getGenericTarget(methodRegEx, sf.methods, document, position);
}

function getCallTarget(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
    return getGenericTarget(callRegEx, sf.callTargets, document, position);
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
        let loc =  new vscode.Location(
            uri,
            new vscode.Position(linnumber, 0)
        );
        locations.push(loc);
        return false;
    } else {
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
        return true;
    }
}

export function provideDefinition(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
    let locations: vscode.Location[] = [];
    let loc;

    let theline = document.lineAt(position.line).text;
    if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
        let qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
        if (qcp === undefined) {
            return locations;
        }

        loc = getSectionOrParaLocation(document, qcp, position);
        if (loc) {
            locations.push(loc);
            return locations;
        }

        /* search for targets in a copybook */
        if (VSCOBOLConfiguration.isCachingEnabled()) {
            let wordRange = document.getWordRangeAtPosition(position, sectionRegEx);
            let word = wordRange ? document.getText(wordRange) : '';
            let wordLower = word.toLocaleLowerCase();

            if (wordLower.length > 0) {
                /* iterater through all the known copybook references */
                for (let [key, value] of qcp.copyBooksUsed) {
                    try {
                        let fileName = expandLogicalCopyBookToFilenameOrEmpty(key, value.extraInformation);
                        if (fileName.length > 0) {
                            let symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                            if (symboleTable !== undefined) {
                                let symbol: COBOLSymbol | undefined = symboleTable.labelSymbols.get(wordLower);
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
    }

    if (theline.match(/.*(new\s*|type).*$/i)) {
        let qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }

        loc = getClassTarget(document, qcp, position);
        if (loc !== undefined) {
            locations.push(loc);
            return locations;
        }
    }

    if (theline.match(/.*(invoke\s*|::)(.*$)/i)) {
        let qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }
        loc = getMethodTarget(document, qcp, position);
        if (loc !== undefined) {
            locations.push(loc);
            return locations;
        }

        if (VSCOBOLConfiguration.isCachingEnabled()) {
            let wordRange = document.getWordRangeAtPosition(position, callRegEx);
            let word = wordRange ? document.getText(wordRange) : '';
            if (word !== "") {
                let img: COBOLGlobalSymbolTable = InMemoryGlobalCachesHelper.getGlobalSymbolCache();
                let wordLower = word.toLocaleLowerCase();
                if (img.classSymbols.has(wordLower)) {
                    let symbols = img.classSymbols.get(wordLower);
                    if (symbols !== undefined) {
                        for (let i = 0; i < symbols.length; i++) {
                            let symbol = symbols[i];
                            if (symbol !== undefined && symbol.filename !== undefined && symbol.lnum !== undefined) {
                                if (openFileViaCommand(symbol.filename, symbol.lnum, locations)) {
                                    return locations;
                                }
                            }
                        }
                    }
                }
                let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
                InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(cacheDirectory);
            }
        }
    }

    if (theline.match(/.*(call|cancel|chain).*$/i)) {
        let qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }
        loc = getCallTarget(document, qcp, position);
        if (loc !== undefined) {
            locations.push(loc);
            return locations;
        }

        /* search for targets in a copybook */
        if (VSCOBOLConfiguration.isCachingEnabled()) {
            let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
            let wordRange = document.getWordRangeAtPosition(position, callRegEx);
            let word = wordRange ? document.getText(wordRange) : '';
            if (word !== "") {
                let img: COBOLGlobalSymbolTable = InMemoryGlobalCachesHelper.getGlobalSymbolCache();
                let wordLower = word.toLocaleLowerCase();
                if (img.callableSymbols.has(wordLower) === false) {
                    InMemoryGlobalCachesHelper.loadInMemoryGlobalSymbolCaches(cacheDirectory);
                    InMemoryGlobalCachesHelper.loadInMemoryGlobalFileCache(cacheDirectory);
                }
                if (img.callableSymbols.has(wordLower)) {
                    let symbols = img.callableSymbols.get(wordLower);
                    if (symbols !== undefined) {
                        for (let i = 0; i < symbols.length; i++) {
                            let symbol = symbols[i];
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
    }

    /* is it a known variable? */
    if (getVariableInCurrentDocument(locations, document, position)) {
        return locations;
    }

    /* search inside on disk copybooks referenced by the current program
     * for variables
     */
    if (VSCOBOLConfiguration.isCachingEnabled()) {
        let qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }

        let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();

        let wordRange = document.getWordRangeAtPosition(position, variableRegEx);
        let word = wordRange ? document.getText(wordRange) : '';
        let wordLower = word.toLowerCase();

        if (wordLower.length > 0) {
            /* iterater through all the known copybook references */
            for (let [key, value] of qcp.copyBooksUsed) {
                try {

                    let fileName = expandLogicalCopyBookToFilenameOrEmpty(key, value.extraInformation);
                    if (fileName.length > 0) {
                        let symboleTable: COBOLSymbolTable | undefined = COBOLSymbolTableHelper.getSymbolTableGivenFile(cacheDirectory, fileName);
                        if (symboleTable !== undefined) {
                            let symbol: COBOLSymbol | undefined = symboleTable.variableSymbols.get(wordLower);
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
    }

    /* fuzzy search is not using the parser and it give false positive's, so lets
     * disable it by default but allow it to be re-enabled via config setting
     */
    if (VSCOBOLConfiguration.getFuzzyVariableSearch()) {
        /* let's see if is a variable via our fuzzy matcher (catch all/brute force) */
        loc = getFuzzyVariable(document, position);
        if (loc) {
            locations.push(loc);
            return locations;
        }
    }

    return locations;

}
