import { TextDocument, Definition, Position, CancellationToken, ProviderResult } from 'vscode';
import * as vscode from 'vscode';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';
import QuickCOBOLParse, { COBOLTokenStyle } from './cobolquickparse';
import { VSCodeSourceHandler } from './VSCodeSourceHandler';


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
            let leftOfWord = lineText.substr(0, wordIndex);

            // fuzzy match for variable
            if (leftOfWord.match(/^[0-9 ]+$/i)) {
                return new vscode.Location(
                    document.uri,
                    new vscode.Position(i, wordIndex)
                );
            }

            // fuzzy match for a FD declaration
            if (leftOfWord.match(/^[ ]+(FD)[ ]+$/i)) {
                return new vscode.Location(
                    document.uri,
                    new vscode.Position(i, wordIndex)
                );
            }
        }
    }
    return undefined;
}

function getSectionOrParaLocation(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*'));
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let sf = new QuickCOBOLParse(new VSCodeSourceHandler(document, true));

    for (var i = 0; i < sf.tokensInOrder.length; i++) {
        let token = sf.tokensInOrder[i];

        if (word === token.description) {
            switch (token.tokenType) {
                case COBOLTokenStyle.Paragraph:
                case COBOLTokenStyle.Section:
                    let srange = new vscode.Position(token.startLine, token.startColumn);
                    return new vscode.Location(document.uri, srange);
                    break;
            }
        }
    }
    return undefined;
}

function getCallTarget(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*'));
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let sf = new QuickCOBOLParse(new VSCodeSourceHandler(document, true));

    for (var i = 0; i < sf.tokensInOrder.length; i++) {
        let token = sf.tokensInOrder[i];

        if (word === token.token) {
            switch (token.tokenType) {
                case COBOLTokenStyle.EntryPoint:
                case COBOLTokenStyle.ProgramId:
                    let srange = new vscode.Position(token.startLine, token.startColumn);
                    return new vscode.Location(document.uri, srange);
                    break;
            }
        }
    }
    return undefined;
}


export function provideDefinition(document: TextDocument, position: Position, token: CancellationToken): ProviderResult<Definition> {
    let location: vscode.Location[] = [];
    let loc;

    let theline = document.lineAt(position.line).text;


    if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
        loc = getSectionOrParaLocation(document, position);
        if (loc) {
            location.push(loc);
            return location;
        }
    }

    if (theline.match(/.*(call|cancel|chain).*$/i)) {
        loc = getCallTarget(document, position);
        if (loc) {
            location.push(loc);
            return location;
        }
    }

    loc = getFuzzyVariable(document, position);
    if (loc) {
        location.push(loc);
        return location;
    }

    return location;

}

