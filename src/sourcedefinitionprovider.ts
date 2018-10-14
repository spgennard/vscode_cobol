import { TextDocument, Definition, Position, CancellationToken, ProviderResult } from 'vscode';
import * as vscode from 'vscode';

function getSectionLocation(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
    let wordRange = document.getWordRangeAtPosition(position, new RegExp('[a-zA-Z][a-zA-Z0-9-_]*'));
    let word = wordRange ? document.getText(wordRange) : '';
    if (word === "") {
        return undefined;
    }

    let wordLower = word.toLocaleLowerCase();

    for (let i = 1; i < document.lineCount; i++) {
        let lineTextLower = document.lineAt(i).text.toLocaleLowerCase();

        let wordIndex = lineTextLower.indexOf(wordLower);
        if (wordIndex !== -1) {
            let sectionIndex = lineTextLower.substr(wordIndex).indexOf("section");
            if (sectionIndex !== -1) {
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

    if (theline.match(/.*(perform|thru|go\\s+to).*$/i)) {
        loc = getSectionLocation(document, position);
        if (loc) {
            location.push(loc);
        }
        return location;
    }

    return location;

}

