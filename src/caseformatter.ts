"use strict";

import * as vscode from "vscode";
import { CancellationToken, FormattingOptions, languages, TextDocument, TextEdit, Position, ProviderResult, OnTypeFormattingEditProvider } from "vscode";
import { COBOLSourceScanner } from "./cobolsourcescanner";
import { COBOLUtils, FoldAction } from "./cobolutils";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSExtensionUtils } from "./vsextutis";

export class COBOLCaseFormatter implements OnTypeFormattingEditProvider {

    private settings: ICOBOLSettings;

    public constructor(settings: ICOBOLSettings) {
        this.settings = settings;
    }

    private convertLine(line: string, current: COBOLSourceScanner, foldConstantToUpper: boolean, langid: string) {
        const oldText = line;
        let newText = COBOLUtils.foldTokenLine(oldText, current, FoldAction.Keywords, foldConstantToUpper, langid, this.settings);
        newText = COBOLUtils.foldTokenLine(newText, current, FoldAction.ConstantsOrVariables, foldConstantToUpper, langid, this.settings);
        return COBOLUtils.foldTokenLine(newText, current, FoldAction.PerformTargets, foldConstantToUpper, langid, this.settings);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]> {

        // only do something if we are just pressed RETURN
        if (ch !== "\n") {
            return;
        }

        if (this.settings.format_on_return === false) {
            return;
        }

        const langid = document.languageId;

        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, this.settings);
        if (current === undefined) {
            return;
        }
        const l = position.line - 1;
        const line = document.lineAt(l);
        if (line) {
            const oldText = line.text;
            const newText = this.convertLine(oldText, current, this.settings.format_constants_to_uppercase, langid);

            if (newText !== oldText) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l, newText.length);
                const range = new vscode.Range(startPos, endPos);

                return [TextEdit.replace(range, newText)];
            }
        }
        return [];
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    static register(settings: ICOBOLSettings): any {
        const langPlusSchemas = VSExtensionUtils.getAllCobolSelectors(settings);

        return languages.registerOnTypeFormattingEditProvider(langPlusSchemas, new COBOLCaseFormatter(settings), "\n");

    }
}
