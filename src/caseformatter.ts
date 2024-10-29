"use strict";

import * as vscode from "vscode";
import { CancellationToken, FormattingOptions, TextDocument, TextEdit, Position } from "vscode";
import { COBOLSourceScanner } from "./cobolsourcescanner";
import { COBOLUtils, FoldAction } from "./vscobolutils";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSExternalFeatures } from "./vsexternalfeatures";

export class COBOLCaseFormatter {

    private settings: ICOBOLSettings;

    public constructor(settings: ICOBOLSettings) {
        this.settings = settings;
    }

    private convertLine(line: string, current: COBOLSourceScanner, foldConstantToUpper: boolean, langid: string) {
        const oldText = line;
        const defaultStyle = this.settings.intellisense_style;
        let newText = COBOLUtils.foldTokenLine(oldText, current, FoldAction.Keywords, foldConstantToUpper, langid, this.settings,defaultStyle);
        newText = COBOLUtils.foldTokenLine(newText, current, FoldAction.ConstantsOrVariables, foldConstantToUpper, langid, this.settings, defaultStyle);
        return COBOLUtils.foldTokenLine(newText, current, FoldAction.PerformTargets, foldConstantToUpper, langid, this.settings, defaultStyle);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): TextEdit[]|undefined {

        // only do something if we are just pressed RETURN
        if (ch !== "\n") {
            return;
        }

        if (this.settings.format_on_return === false) {
            return;
        }

        const langid = document.languageId;
        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document,config);
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
}
