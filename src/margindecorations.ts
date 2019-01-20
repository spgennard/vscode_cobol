'use strict';

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEdit } from 'vscode';
import { enableMarginStatusBar, hideMarginStatusBar } from './extension';

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

function isActivateForThisDocument(doc: TextDocument): boolean {

    for (let i = 0; i < doc.lineCount; i++) {
        let lineText = doc.lineAt(i);
        let line = lineText.text;

        let pos4sourceformat = line.indexOf("sourceformat");
        if (pos4sourceformat === -1) {
            continue;
        }
        let line2right = line.substr(pos4sourceformat + 12);
        if (line2right.indexOf("fixed") !== -1) {
            return true;
        }
        if (i > 10) {
            return false;
        }
    }

    return false;
}

function isActive(document: TextDocument): boolean {

    let activate: boolean = false;
    switch (document.languageId.toLowerCase()) {
        case "cobol": activate = true; break;
        case "opencobol": activate = true; break;
        case "acucobol": activate = true; break;
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

    if (!isActive(doc)) {
        enableMarginStatusBar(false);
        return;
    }

    /* is it enabled? */
    if (!isEnabledViaWorkspace()) {
        enableMarginStatusBar(false);
        return;
    }
    /* does it include sourceformat"free"? */
    if (!isActivateForThisDocument(doc)) {
        enableMarginStatusBar(false);
        return;
    }

    enableMarginStatusBar(true);
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

