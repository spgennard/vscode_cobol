'use strict';

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEdit } from 'vscode';
import { enableMarginStatusBar, hideMarginStatusBar } from './extension';

export const sourceformat_unknown: number = 0;
export const sourceformat_fixed: number = 1;
export const sourceformat_variable: number = 2;
export const sourceformat_free: number = 3;


let trailingSpacesDecorationType = window.createTextEditorDecorationType({
    light: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)",
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
    },
    dark: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)"
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
    }

});

function isEnabledViaWorkspace(): boolean {
    let editorConfig = workspace.getConfiguration('coboleditor');
    let marginOn = editorConfig.get<boolean>('margin');
    if (marginOn !== undefined) {
        return marginOn;
    }
    return true;
}

function isNumber(value: string | number): boolean {
    if (value.toString().length === 0) {
        return false;
    }
    return !isNaN(Number(value.toString()));
}

function getEnabledFiles(): string[] {
    let editorConfig = workspace.getConfiguration('coboleditor');
    let files = editorConfig.get<string[]>('fixedformatfiles');
    if (!files || (files !== null)) {
        files = [];
    }
    return files;
}

const inline_sourceformat: string[] = ['sourceformat', '>>source format'];

function isActivateForThisDocument(doc: TextDocument): number {

    if (doc.languageId.toLowerCase() === "jcl") {
        return sourceformat_fixed;
    }

    let linesWithJustNumbers = 0;
    let maxLines = doc.lineCount > 10 ? 10 : doc.lineCount;

    for (let i = 0; i < maxLines; i++) {
        let lineText = doc.lineAt(i);
        let line = lineText.text.toLocaleLowerCase();

        let pos4sourceformat_after = 0;
        for (let isf = 0; isf < inline_sourceformat.length; isf++) {
            let pos4sourceformat = line.indexOf(inline_sourceformat[isf]);
            if (pos4sourceformat !== -1) {
                pos4sourceformat_after = pos4sourceformat + inline_sourceformat[isf].length + 1;
                break;
            }
        }

        if (pos4sourceformat_after === 0) {
            if (line.length > 72) {
                let rightMargin = line.substr(72).trim();
                if (isNumber(rightMargin)) {
                    linesWithJustNumbers++;
                }
            }
            continue;
        }
        let line2right = line.substr(pos4sourceformat_after);

        if (line2right.indexOf("fixed") !== -1) {
            return sourceformat_fixed;
        }
        if (line2right.indexOf("variable") !== -1) {
            return sourceformat_variable;
        }
        if (line2right.indexOf("free") !== -1) {
            return sourceformat_free;
        }
    }

    //it might well be...
    if (linesWithJustNumbers > 7) {
        return sourceformat_fixed;
    }
    return sourceformat_unknown;
}

function isSupportedLanguage(document: TextDocument): boolean {

    let activate: boolean = false;
    switch (document.languageId.toLowerCase()) {
        case "cobol": activate = true; break;
        case "opencobol": activate = true; break;
        case "acucobol": activate = true; break;
        case "jcl": activate = true; break;
    }

    /* not a supported language? */
    if (!activate) {
        return false;
    }

    return true;
}

export default function updateDecorations(activeTextEditor: TextEditor | undefined) {
    if (!activeTextEditor) {
        hideMarginStatusBar();
        return;
    }

    const doc: TextDocument = activeTextEditor.document;

    if (!isSupportedLanguage(doc)) {
        hideMarginStatusBar();
        return;
    }

    /* is it enabled? */
    if (!isEnabledViaWorkspace()) {
        hideMarginStatusBar();
        return;
    }
    /* does it include sourceformat"free"? */
    let sourceformatStyle = isActivateForThisDocument(doc);
    enableMarginStatusBar(sourceformatStyle);
    if (sourceformatStyle !== sourceformat_fixed) {
        return;
    }

    const decorationOptions: DecorationOptions[] = [];
    for (let i = 0; i < doc.lineCount; i++) {
        let lineText = doc.lineAt(i);
        let line = lineText.text;

        if (line.length > 72) {
            let startPos = new Position(i, 72);
            let endPos = new Position(i, line.length);
            // const decoration = { range: new Range(startPos, endPos), hoverMessage: "Number **" + i + "**" };
            const decoration = { range: new Range(startPos, endPos) };
            decorationOptions.push(decoration);
        }
    }
    activeTextEditor.setDecorations(trailingSpacesDecorationType, decorationOptions);
}

