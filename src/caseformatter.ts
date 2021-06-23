'use strict';

import * as vscode from 'vscode';
import { CancellationToken, FormattingOptions, languages, TextDocument, TextEdit, Position, ProviderResult, OnTypeFormattingEditProvider } from "vscode";
import COBOLSourceScanner from './cobolsourcescanner';
import { COBOLUtils, FoldAction, FoldStyle } from './cobolutils';
import { VSCOBOLConfiguration } from './vsconfiguration';
import { formatOnReturn } from './iconfiguration';
import VSCOBOLSourceScanner from './vscobolscanner';

export class COBOLCaseFormatter implements OnTypeFormattingEditProvider{

    private convertLine(line: string, foldStyle: FoldStyle, current: COBOLSourceScanner, foldConstantToUpper: boolean) {
        const oldText = line;
        let newText = COBOLUtils.foldTokenLine(oldText, current, FoldAction.Keywords, foldStyle, foldConstantToUpper);
        newText = COBOLUtils.foldTokenLine(newText, current, FoldAction.ConstantsOrVariables, foldStyle, foldConstantToUpper);
        return COBOLUtils.foldTokenLine(newText, current, FoldAction.PerformTargets, foldStyle, foldConstantToUpper);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]> {

        // only do something if we are just pressed RETURN
        if (ch !== '\n') {
            return;
        }

        const settings = VSCOBOLConfiguration.get();
        if (settings.format_on_return === formatOnReturn.Off) {
            return;
        }


        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (current === undefined) {
            return;
        }
        const l = position.line - 1;
        const line = document.lineAt(l);
        if (line) {
            const oldText = line.text;
            const newText = settings.format_on_return === formatOnReturn.CamelCase ?
                this.convertLine(oldText,FoldStyle.CamelCase,current, settings.format_constants_to_uppercase) :
                this.convertLine(oldText,FoldStyle.UpperCase,current, settings.format_constants_to_uppercase);

            if (newText !== oldText) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l,newText.length);
                const range = new vscode.Range(startPos, endPos);

                return [ TextEdit.replace(range, newText) ];
            }
        }
        return [];
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    static register(): any {
        const langPlusSchemas = [
            { scheme: 'file', language: 'COBOL' },
            { scheme: 'file', language: 'COBOLIT' },
            { scheme: 'file', language: 'ACUCOBOL' }
        ];

        return languages.registerOnTypeFormattingEditProvider(langPlusSchemas, new COBOLCaseFormatter(), '\n');

    }
}
