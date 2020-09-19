import * as vscode from 'vscode';
import { logMessage } from './extension';
import COBOLSourceScanner, { COBOLTokenStyle, COBOLToken, CallTargetInformation } from './cobolsourcescanner';
import VSQuickCOBOLParse from './vscobolscanner';
import { VSCOBOLConfiguration } from './configuration';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';
import { ICOBOLSettings } from './iconfiguration';

const sectionRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
const variableRegEx = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');
const callRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
const classRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
const methodRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');

function getFuzzyVariable(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    const wordRange = document.getWordRangeAtPosition(position, new RegExp("[a-zA-Z0-9_-]+"));
    const word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }
    const workLower = word.toLocaleLowerCase();

    for (let i = 1; i < document.lineCount; i++) {
        const lineText = document.lineAt(i).text;

        // TODO - need to handle inline comments too
        if (lineText.length > 7 && lineText[6] === '*') {
            continue;
        }

        // time to leave
        if (lineText.match(/.*(procedure\s+division).*$/i)) {
            return undefined;
        }

        const wordIndex = lineText.toLowerCase().indexOf(workLower);
        if (wordIndex !== -1) {
            const leftOfWord = lineText.substr(0, wordIndex).trim();

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
    const wordRange = document.getWordRangeAtPosition(position, sectionRegEx);
    const word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    const wordLower = word.toLowerCase();

    try {

        if (sf.sections.has(wordLower)) {
            const token = sf.sections.get(wordLower);
            if (token !== undefined) {
                const srange = new vscode.Position(token.startLine, token.startColumn);
                const uri = vscode.Uri.file(token.filename);
                return new vscode.Location(uri, new vscode.Range(srange, srange));
            }
        }

    }
    catch (e) {
        logMessage(e);
    }

    try {
        if (sf.paragraphs.has(wordLower)) {
            const token = sf.paragraphs.get(wordLower);
            if (token !== undefined) {
                const srange = new vscode.Position(token.startLine, token.startColumn);
                const uri = vscode.Uri.file(token.filename);
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
    const wordRange = document.getWordRangeAtPosition(position, variableRegEx);
    const word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return false;
    }

    const tokenLower: string = word.toLowerCase();
    if (isValidKeyword(tokenLower)) {
        return false;
    }

    const sf: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
    if (sf === undefined) {
        return false;
    }

    const tokens: COBOLToken[] | undefined = sf.constantsOrVariables.get(tokenLower);
    if (tokens === undefined || tokens.length === 0) {
        return false;
    }

    for (let i = 0; i < tokens.length; i++) {
        const token: COBOLToken = tokens[i];
        if (token.tokenNameLower === "filler") {
            continue;
        }

        switch (token.tokenType) {
            case COBOLTokenStyle.Constant:
                {
                    const srange = new vscode.Position(token.startLine, token.startColumn);
                    const uri = vscode.Uri.file(token.filename);
                    locations.push(new vscode.Location(uri, srange));
                    break;
                }
            case COBOLTokenStyle.Variable:
                {
                    const srange = new vscode.Position(token.startLine, token.startColumn);
                    const uri = vscode.Uri.file(token.filename);
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
    const wordRange = document.getWordRangeAtPosition(position, queryRegEx);
    const word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    const workLower = word.toLowerCase();
    if (tokenMap.has(workLower)) {
        const token: COBOLToken | undefined = tokenMap.get(workLower);
        if (token !== undefined) {
            const srange = new vscode.Position(token.startLine, token.startColumn);
            const uri = vscode.Uri.file(token.filename);
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
    const wordRange = document.getWordRangeAtPosition(position, callRegEx);
    const word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    const workLower = word.toLowerCase();
    if (sf.callTargets.has(workLower)) {
        const targetInfo: CallTargetInformation | undefined = sf.callTargets.get(workLower);
        if (targetInfo !== undefined) {
            const token = targetInfo.Token;
            const srange = new vscode.Position(token.startLine, token.startColumn);
            const uri = vscode.Uri.file(token.filename);
            return new vscode.Location(uri, srange);
        }
    }
    return undefined;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function provideDefinition(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
    const locations: vscode.Location[] = [];
    let loc;
    const settings: ICOBOLSettings = VSCOBOLConfiguration.get();

    const theline = document.lineAt(position.line).text;
    if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
        const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }

        loc = getSectionOrParaLocation(document, qcp, position);
        if (loc) {
            locations.push(loc);
            return locations;
        }
    }

    if (theline.match(/.*(new\s*|type).*$/i)) {
        const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
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
        const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }
        loc = getMethodTarget(document, qcp, position);
        if (loc !== undefined) {
            locations.push(loc);
            return locations;
        }
    }

    if (theline.match(/.*(call|cancel|chain).*$/i)) {
        const qcp: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (qcp === undefined) {
            return locations;
        }
        loc = getCallTarget(document, qcp, position);
        if (loc !== undefined) {
            locations.push(loc);
            return locations;
        }
    }

    /* is it a known variable? */
    if (getVariableInCurrentDocument(locations, document, position)) {
        return locations;
    }

    /* fuzzy search is not using the parser and it give false positive's, so lets
     * disable it by default but allow it to be re-enabled via config setting
     */
    if (settings.fuzzy_variable_search) {
        /* let's see if is a variable via our fuzzy matcher (catch all/brute force) */
        loc = getFuzzyVariable(document, position);
        if (loc) {
            locations.push(loc);
            return locations;
        }
    }

    return locations;
}
