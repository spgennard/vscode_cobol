import { TextDocument, Definition, Position, CancellationToken, ProviderResult } from 'vscode';
import * as vscode from 'vscode';

function getSectionOrParaLocation(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, new RegExp('[a-zA-Z][a-zA-Z0-9-_]*'));
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let wordLower = word.toLocaleLowerCase();
    let paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ].*$/g;
    let paraPrefixRegex2 = /^[ \\t]*$/g;

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
            let prefixLine = lineTextLower.substr(0, wordIndex);
            let postLine = lineTextLower.substr(wordIndex + word.length);

            //if it is not a section, it might be a paragrapg.. does it have a "." after it and
            //does it have whitespace or numbers (column a)?
            if (postLine[0] === '.') {
                let isOkay: boolean = false;

                if (prefixLine.match(paraPrefixRegex1) || prefixLine.match(paraPrefixRegex2)) {
                    isOkay = true;
                }

                if (isOkay) {
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

export function provideDefinition(document: TextDocument, position: Position, token: CancellationToken): ProviderResult<Definition> {
    let location: vscode.Location[] = [];
    let loc;

    let theline = document.lineAt(position.line).text;

    if (theline.match(/.*(perform|thru|go\\s+to).*$/i)) {
        loc = getSectionOrParaLocation(document, position);
        if (loc) {
            location.push(loc);
        }
    }

    return location;

}

