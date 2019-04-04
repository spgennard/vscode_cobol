import { TextDocument, Definition, Position, CancellationToken, ProviderResult } from 'vscode';
import * as vscode from 'vscode';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';

function isValidKeyword(keyword: string): boolean {
    return cobolKeywordDictionary.containsKey(keyword);
}

function getSectionOrParaLocation(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*'));
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let wordLower = word.toLocaleLowerCase();
    let paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ].*$/g;
    let paraPrefixRegex2 = /^[ \t]*$/g;

    for (let i = 1; i < document.lineCount; i++) {
        let lineTextLower = document.lineAt(i).text.toLocaleLowerCase();

        // TODO - need to handle inline comments too
        if (lineTextLower.length > 7 && lineTextLower[6] === '*') {
            continue;
        }

        // Does the line include the word?
        let wordIndex = lineTextLower.indexOf(wordLower);
        if (wordIndex !== -1) {

            //does it have the section after it?  
            //it's a bit fuzzy.. so it could break but it should be good enough
            let sectionIndex = lineTextLower.substr(wordIndex).indexOf("section");
            if (sectionIndex !== -1) {
                return new vscode.Location(
                    document.uri,
                    new vscode.Position(i, wordIndex)
                );
            }
            let prefixLine = lineTextLower.substr(0, wordIndex).trim();
            let postLine = lineTextLower.substr(wordIndex + word.length);

            //if it is not a section, it might be a paragraph.. does it have a "." after it and
            //does it have whitespace or numbers (column a)?
            if (postLine[0] === '.') {

                // is the code observing fixed format rules, if so then the before field should be a keyword
                // and we have a space seperator
                if (lineTextLower.length > 7 && isValidKeyword(prefixLine) === false && lineTextLower[6] === ' ') {
                    return new vscode.Location(
                        document.uri,
                        new vscode.Position(i, wordIndex)
                    );
                }
                // nothing before it and it ends in a ., then it could be a paragraph
                if (prefixLine.length === 0) {
                    return new vscode.Location(
                        document.uri,
                        new vscode.Position(i, wordIndex)
                    );
                }
            }
        }
    }
    return undefined;
}

function getCallTarget(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, new RegExp('["\'][a-zA-Z0-9-_]*["\']'));
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let wordLower = word.substr(1, word.length - 2).toLocaleLowerCase();

    for (let i = 1; i < document.lineCount; i++) {
        let lineTextLower = document.lineAt(i).text.toLocaleLowerCase();

        // TODO - need to handle inline comments too
        if (lineTextLower.length > 7 && lineTextLower[6] === '*') {
            continue;
        }

        // Does the line include the word?
        let wordIndex = lineTextLower.indexOf(wordLower);
        if (wordIndex !== -1) {

            //does it have the enttry before it?  
            //it's a bit fuzzy.. so it could break but it should be good enough
            let index = lineTextLower.substr(0, wordIndex).indexOf("entry");
            if (index === -1) {
                index = lineTextLower.substr(0, wordIndex).indexOf("program-id");
            }
            if (index !== -1) {
                return new vscode.Location(
                    document.uri,
                    new vscode.Position(i, wordIndex)
                );
            }

        }
    }
    return undefined;
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

    if (theline.match(/.*(call|cancel).*$/i)) {
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

