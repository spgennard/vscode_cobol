'use strict';

import { CancellationToken, FormattingOptions, languages, TextDocument, TextEdit, Position, ProviderResult } from "vscode";

export class DocComment {
    static register() {
        const langPlusSchema =  { scheme: 'file', language: 'COBOL' };

        // Insert *>> when RETURN is pressed
        return languages.registerOnTypeFormattingEditProvider(langPlusSchema, {
            provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]> {
                const line = document.lineAt(position.line - 1);
                if (line && line.text.trim().startsWith("*>>")) {
                    return [
                        TextEdit.insert(position, "*>> ")
                    ]
                } else {
                    return []
                }
            }
        }, '\n');
    }
}
