'use strict';

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEditorDecorationType } from 'vscode';
import { VSCOBOLConfiguration } from './vsconfiguration';
import { ESourceFormat } from './externalfeatures';
import VSCOBOLSourceScanner from './vscobolscanner';
import { getWorkspaceFolders } from './cobolfolders';
import { VSCodeSourceHandler } from './vscodesourcehandler';
import { getCOBOLSourceFormat } from './sourceformat';

const trailingSpacesDecoration: TextEditorDecorationType = window.createTextEditorDecorationType({
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

function isEnabledViaWorkspace4jcl(): boolean {
    if (getWorkspaceFolders() === undefined) {
        return false;
    }

    const editorConfig = workspace.getConfiguration('jcleditor');
    const marginOn = editorConfig.get<boolean>('margin');
    if (marginOn !== undefined) {
        return marginOn;
    }
    return true;
}

export enum TextLanguage {
    Unknown = 0,
    COBOL = 1,
    JCL = 2
}

export function isSupportedLanguage(document: TextDocument): TextLanguage {

    switch (document.languageId.toLowerCase()) {
        case "cobolit":
        case "cobol":
        case "acucobol":
            return TextLanguage.COBOL;
        case "jcl":
            return TextLanguage.JCL;
    }

    /* not a supported language? */
    return TextLanguage.Unknown;
}

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export default async function updateDecorations(activeTextEditor: TextEditor | undefined) {
    if (!activeTextEditor) {
        return;
    }

    const doc: TextDocument = activeTextEditor.document;
    const decorationOptions: DecorationOptions[] = [];

    const textLanguage: TextLanguage = isSupportedLanguage(doc);

    if (textLanguage === TextLanguage.Unknown) {
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    /* is it enabled? (COBOL) */
    if (textLanguage === TextLanguage.JCL && !isEnabledViaWorkspace4jcl()) {
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    const configHandler = VSCOBOLConfiguration.get();
    if (textLanguage === TextLanguage.COBOL) {
        if (configHandler.fileformat_strategy !== "always_fixed") {
            const gcp = VSCOBOLSourceScanner.getCachedObject(doc, configHandler);
            let sf: ESourceFormat = ESourceFormat.unknown;
            if (gcp === undefined) {
                const vsfile = new VSCodeSourceHandler(doc, false);
                sf = getCOBOLSourceFormat(vsfile, configHandler);
            } else {
                sf = gcp.sourceFormat;
            }
            // use the known file format from the scan itself
            switch (sf) {
                case ESourceFormat.free:
                case ESourceFormat.variable:
                case ESourceFormat.unknown:
                    activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
                    return;
            }
        }

        // fixed format from here onwards...
        const lineLimit = configHandler.editor_maxTokenizationLineLength;
        const maxLinesInFile = doc.lineCount;
        let maxLines = maxLinesInFile;
        if (maxLines > lineLimit) {
            maxLines = lineLimit;
        }

        for (let i = 0; i < maxLines; i++) {
            const lineText = doc.lineAt(i);
            const line = lineText.text;

            // only do it, if we have no tabs on the line..
            const containsTab = line.indexOf('\t');
            
            if (containsTab === -1 && line.length > 6) {
                const startPos = new Position(i, 0);
                const endPos = new Position(i, 6);
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }

            if (containsTab === -1 && line.length > 72) {
                const startPos = new Position(i, 72);
                // only colour 72-80
                const endPos = new Position(i, (line.length < 80 ? line.length : 80));
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }
        }
    }

    if (textLanguage === TextLanguage.JCL) {
        for (let i = 0; i < doc.lineCount; i++) {
            const lineText = doc.lineAt(i);
            const line = lineText.text;

            if (line.length > 72) {
                const startPos = new Position(i, 72);
                const endPos = new Position(i, line.length);
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }
        }
    }
    activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
}
