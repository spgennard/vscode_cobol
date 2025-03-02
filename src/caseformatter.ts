"use strict";

import * as vscode from "vscode";
import { CancellationToken, FormattingOptions, TextDocument, TextEdit, Position } from "vscode";
import { VSCOBOLUtils, FoldAction } from "./vscobolutils";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { ICOBOLSourceScanner } from "./icobolsourcescanner";

export class COBOLCaseFormatter {

    private convertLine(settings: ICOBOLSettings, line: string, current: ICOBOLSourceScanner, foldConstantToUpper: boolean, langid: string) {
        const oldText = line;
        const defaultStyle = settings.intellisense_style;
        let newText = VSCOBOLUtils.foldTokenLine(oldText, current, FoldAction.Keywords, foldConstantToUpper, langid, settings,defaultStyle);
        newText = VSCOBOLUtils.foldTokenLine(newText, current, FoldAction.ConstantsOrVariables, foldConstantToUpper, langid, settings, defaultStyle);
        return VSCOBOLUtils.foldTokenLine(newText, current, FoldAction.PerformTargets, foldConstantToUpper, langid, settings, defaultStyle);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): TextEdit[]|undefined {

        // only do something if we are just pressed RETURN
        if (ch !== "\n") {
            return;
        }

        const settings = VSCOBOLConfiguration.get_resource_settings(document,VSExternalFeatures);

        if (settings.format_on_return === false) {
            return;
        }

        const langid = document.languageId;
        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
        const current: ICOBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document,config);
        if (current === undefined) {
            return;
        }
        const l = position.line - 1;
        const line = document.lineAt(l);
        if (line) {
            const oldText = line.text;
            const newText = this.convertLine(settings, oldText, current, settings.format_constants_to_uppercase, langid);

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
