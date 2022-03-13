"use strict";

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEditorDecorationType } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ESourceFormat } from "./externalfeatures";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSWorkspaceFolders } from "./cobolfolders";
import { VSCodeSourceHandlerLite } from "./vscodesourcehandler";
import { SourceFormat } from "./sourceformat";
import { TextLanguage, VSExtensionUtils } from "./vsextutis";
import { ColourTagHandler } from "./vscolourcomments";

const trailingSpacesDecoration: TextEditorDecorationType = window.createTextEditorDecorationType({
    light: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)",
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
        textDecoration: "solid"
    },
    dark: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)"
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
        textDecoration: "solid"
    }

});

export class VSmargindecorations extends ColourTagHandler {

    private tags = new Map<string, TextEditorDecorationType>();

    constructor() {
        super();
        this.setupTags();
    }

    public setupTags() {
        super.setupTags("columns_tags",this.tags);
    }

    private static isEnabledViaWorkspace4jcl(): boolean {
        if (VSWorkspaceFolders.get() === undefined) {
            return false;
        }

        const editorConfig = workspace.getConfiguration("jcleditor");
        const marginOn = editorConfig.get<boolean>("margin");
        if (marginOn !== undefined) {
            return marginOn;
        }
        return true;
    }


    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    public static async updateDecorations(activeTextEditor: TextEditor | undefined) {
        if (!activeTextEditor) {
            return;
        }

        const doc: TextDocument = activeTextEditor.document;
        const decorationOptions: DecorationOptions[] = [];

        const textLanguage: TextLanguage = VSExtensionUtils.isSupportedLanguage(doc);

        if (textLanguage === TextLanguage.Unknown) {
            activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
            return;
        }

        /* is it enabled? (COBOL) */
        if (textLanguage === TextLanguage.JCL && !VSmargindecorations.isEnabledViaWorkspace4jcl()) {
            activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
            return;
        }

        if (textLanguage === TextLanguage.COBOL) {
            const configHandler = VSCOBOLConfiguration.get();
            if (configHandler.fileformat_strategy !== "always_fixed") {
                const gcp = VSCOBOLSourceScanner.getCachedObject(doc, configHandler);
                let sf: ESourceFormat = ESourceFormat.unknown;
                if (gcp === undefined) {
                    const vsfile = new VSCodeSourceHandlerLite(doc);
                    sf = SourceFormat.get(vsfile, configHandler);
                } else {
                    sf = gcp.sourceFormat;
                }
                // use the known file format from the scan itself
                switch (sf) {
                    case ESourceFormat.free:
                    case ESourceFormat.variable:
                    case ESourceFormat.unknown:
                    case ESourceFormat.terminal:
                        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
                        return;
                }
            }

            const maxLineLength = configHandler.editor_maxTokenizationLineLength;
            const maxLines = doc.lineCount;

            for (let i = 0; i < maxLines; i++) {
                const lineText = doc.lineAt(i);
                const line = lineText.text;
                if (line.length > maxLineLength) {
                    continue;
                }

                // only do it, if we have no tabs on the line..
                const containsTab = line.indexOf("\t");

                if (containsTab === -1 || containsTab >= 6 ) {
                    if (line.length >= 6) {
                        const startPos = new Position(i, 0);
                        const endPos = new Position(i, 6);
                        const decoration = { range: new Range(startPos, endPos) };
                        decorationOptions.push(decoration);
                    }
                }

                if (containsTab === -1 || containsTab > 80) {
                    if (line.length > 72) {
                        const startPos = new Position(i, 72);
                        // only colour 72-80
                        const endPos = new Position(i, (line.length < 80 ? line.length : 80));
                        const decoration = { range: new Range(startPos, endPos) };
                        decorationOptions.push(decoration);
                    }
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
}
