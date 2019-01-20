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
    let editorConfig =  workspace.getConfiguration('coboleditor');
    let marginOn = editorConfig.get<boolean>('margin');
    if (marginOn !== undefined) {
        return marginOn;
    }
    return true;
}

function getEnabledFiles(): string[] {
    let editorConfig =  workspace.getConfiguration('coboleditor');
    let files = editorConfig.get<string[]>('fixedformatfiles');
    if (!files || (files !== null)) {
        files = [];
    }
    return files;
}

function isActivateForThisDocument(doc: TextDocument): number {

    if (doc.languageId.toLowerCase() === "jcl") {
        return sourceformat_fixed;
    }

    for (let i = 0; i < doc.lineCount; i++) {
        let lineText = doc.lineAt(i);
        let line = lineText.text;

        let pos4sourceformat = line.indexOf("sourceformat");
        if (pos4sourceformat === -1) {
            continue;
        }
        let line2right = line.substr(pos4sourceformat + 12);
        if (line2right.indexOf("fixed") !== -1) {
            return sourceformat_fixed;
        }
        if (line2right.indexOf("variable") !== -1) {
            return sourceformat_variable;
        }
        if (line2right.indexOf("free") !== -1) {
            return sourceformat_free;
        }
        if (i > 10) {
            return sourceformat_unknown;
        }
    }

    return sourceformat_unknown;
}

function isActive(document: TextDocument): boolean {

    let activate: boolean = false;
    switch (document.languageId.toLowerCase()) {
        case "cobol": activate = true; break;
        case "opencobol": activate = true; break;
        case "acucobol": activate = true; break;
        case "jcl": activate = true; break;
    }

    /* not a supported language? */
    if (!activate) {
        console.log("Margin not active for "+document.languageId);
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

    if (!isActive(doc)) {
        enableMarginStatusBar(sourceformat_unknown);
        return;
    }

    /* is it enabled? */
    if (!isEnabledViaWorkspace()) {
        enableMarginStatusBar(sourceformat_unknown);
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
            const decoration = { range: new Range(startPos, endPos), hoverMessage: "Number **" + i + "**" };
            decorationOptions.push(decoration);
        }
    }
    activeTextEditor.setDecorations(trailingSpacesDecorationType, decorationOptions);
}

