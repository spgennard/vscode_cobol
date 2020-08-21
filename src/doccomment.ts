'use strict';

import { CancellationToken, FormattingOptions, languages, TextDocument, TextEdit, Position, ProviderResult } from "vscode";

export class DocComment {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    static register():any {
        const langPlusSchema = { scheme: 'file', language: 'COBOL' };

        return languages.registerOnTypeFormattingEditProvider(langPlusSchema, {
            provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]> {
                const line = document.lineAt(position.line - 1);
                if (line) {

                    // micro focus xml doc
                    if (line.text.trim().startsWith("*>>")) {
                        return [
                            // Insert *>> when RETURN is pressed
                            TextEdit.insert(position, "*>> ")
                        ];
                    }

                    // coboldoc
                    if (line.text.trim().startsWith("*>**")) {
                        return [
                            // Insert *>> when RETURN is pressed
                            TextEdit.insert(position, "*>> ")
                        ];
                    }
                }



                return [];

            }
        }, '\n');
    }
}
