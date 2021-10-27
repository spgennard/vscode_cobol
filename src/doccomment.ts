"use strict";

import { CancellationToken, FormattingOptions, languages, TextDocument, TextEdit, Position, ProviderResult } from "vscode";

export class COBOLDocumentationCommentHandler {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    static register(): any {
        const langPlusSchemas = [{ scheme: "file", language: "COBOL" }];

        return languages.registerOnTypeFormattingEditProvider(langPlusSchemas, {
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]> {

                // only do something if we are just pressed RETURN
                if (ch !== "\n") {
                    return;
                }

                const line = document.lineAt(position.line - 1);
                if (line) {
                    const lineTrimmed = line.text.trim();

                    if (lineTrimmed.length > 3) {
                        // micro focus xml doc
                        if (lineTrimmed.startsWith("*>>")) {
                            return [
                                // Insert *>> when RETURN is pressed
                                TextEdit.insert(position, "*>> ")
                            ];
                        }

                        // // coboldoc
                        // if (lineTrimmed.startsWith("*>**")) {
                        //     return [
                        //         // Insert *>> when RETURN is pressed
                        //         TextEdit.insert(position, "*>> ")
                        //     ];
                        // }
                    }
                }
                return [];
            }
        }, "\n");
    }
}
