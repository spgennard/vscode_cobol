'use strict';

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEditorDecorationType, QuickPickItem, TextEditorRevealType } from 'vscode';
import { enableMarginStatusBar, hideMarginStatusBar } from './extension';

import minimatch = require('minimatch');
import path = require('path');

var trailingSpacesDecoration: TextEditorDecorationType = window.createTextEditorDecorationType({
    light: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)",
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
        textDecoration: 'solid'
    },
    dark: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)"
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
        textDecoration: 'solid'
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


export enum ESourceFormat {
    unknown = 'unknown',
    fixed = 'fixed',
    free = 'free',
    variable = 'variable',
    jcl = 'jcl'
}

export const sourceformatMessages: string[] = ['unknown', 'fixed', 'free', 'variable'];

interface IEditorMarginFiles {
    pattern: string;
    sourceformat: ESourceFormat;
}

function isNumber(value: string | number): boolean {
    if (value.toString().length === 0) {
        return false;
    }
    return !isNaN(Number(value.toString()));
}

function getFixedFilenameConfiguration(): IEditorMarginFiles[] {
    let editorConfig = workspace.getConfiguration('coboleditor');
    let files: IEditorMarginFiles[] | undefined = editorConfig.get<IEditorMarginFiles[]>("fileformat");
    if (files === undefined || files === null) {
        return [];
    }

    return files;
}

const inline_sourceformat: string[] = ['sourceformat', '>>source format'];

function isActivateForThisDocument(doc: TextDocument): ESourceFormat {

    if (doc.languageId.toLowerCase() === "jcl") {
        return ESourceFormat.jcl;
    }

    let filesFilter = getFixedFilenameConfiguration();
    if (filesFilter.length >= 1) {
        let docFilename: string = doc.fileName;
        for (let i = 0; i < filesFilter.length; i++) {
            let filter: IEditorMarginFiles = filesFilter[i];

            if (minimatch(docFilename, filter.pattern, { nocase: true })) {
                return ESourceFormat[filter.sourceformat];
            }
        }
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
            return ESourceFormat.fixed;
        }
        if (line2right.indexOf("variable") !== -1) {
            return ESourceFormat.variable;
        }
        if (line2right.indexOf("free") !== -1) {
            return ESourceFormat.free;
        }
    }

    //it might well be...
    if (linesWithJustNumbers > 7) {
        return ESourceFormat.free;
    }
    return ESourceFormat.unknown;
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
    const decorationOptions: DecorationOptions[] = [];

    if (!isSupportedLanguage(doc)) {
        hideMarginStatusBar();
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    /* is it enabled? */
    if (!isEnabledViaWorkspace()) {
        hideMarginStatusBar();
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    /* does it include sourceformat"free"? */
    let sourceformatStyle: ESourceFormat = isActivateForThisDocument(doc);
    enableMarginStatusBar(sourceformatStyle);
    switch (sourceformatStyle) {
        case ESourceFormat.free:
        case ESourceFormat.variable:
        case ESourceFormat.unknown:
            activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
            return;
    }

    for (let i = 0; i < doc.lineCount; i++) {
        let lineText = doc.lineAt(i);
        let line = lineText.text;

        if (sourceformatStyle === ESourceFormat.fixed) {
            if (line.length > 6) {
                let startPos = new Position(i, 0);
                let endPos = new Position(i, 6);
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }
        }

        if (line.length > 72) {
            let startPos = new Position(i, 72);
            let endPos = new Position(i, line.length);
            const decoration = { range: new Range(startPos, endPos) };
            decorationOptions.push(decoration);
        }
    }
    activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
}

