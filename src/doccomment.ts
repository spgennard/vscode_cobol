"use strict";

import { CancellationToken, FormattingOptions,  TextDocument, TextEdit, Position } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";

export class COBOLDocumentationCommentHandler {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    constructor(settings: ICOBOLSettings)
    {

    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): TextEdit[]|undefined {
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
            }
        }
        return [];
    }
}
