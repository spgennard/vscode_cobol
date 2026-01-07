"use strict";

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEditorDecorationType } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ESourceFormat } from "./externalfeatures";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { VSCodeSourceHandlerLite } from "./vscodesourcehandler";
import { SourceFormat } from "./sourceformat";
import { TextLanguage, VSExtensionUtils } from "./vsextutis";
import { ColourTagHandler } from "./vscolourcomments";
import { fileformatStrategy, ICOBOLSettings } from "./iconfiguration";
import { VSExternalFeatures } from "./vsexternalfeatures";

const defaultTrailingSpacesDecoration: TextEditorDecorationType = window.createTextEditorDecorationType({
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

    public setupTags(): void {
        super.setupTags("columns_tags", this.tags);
    }

    private isEnabledViaWorkspace4jcl(settings: ICOBOLSettings): boolean {
        if (VSWorkspaceFolders.get(settings) === undefined) {
            return false;
        }

        const editorConfig = workspace.getConfiguration("jcleditor");
        const marginOn = editorConfig.get<boolean>("margin");
        if (marginOn !== undefined) {
            return marginOn;
        }
        return true;
    }

    private async updateJCLDecorations(doc: TextDocument, activeTextEditor: TextEditor, defaultTrailingSpacesDecoration: TextEditorDecorationType) {
        const defaultDecorationOptions: DecorationOptions[] = [];
        for (let i = 0; i < doc.lineCount; i++) {
            const lineText = doc.lineAt(i);
            const line = lineText.text;

            if (line.length > 72) {
                const startPos = new Position(i, 72);
                const endPos = new Position(i, line.length);
                const decoration = { range: new Range(startPos, endPos) };
                defaultDecorationOptions.push(decoration);
            }
        }
        activeTextEditor.setDecorations(defaultTrailingSpacesDecoration, defaultDecorationOptions);
    }

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    public async updateDecorations(activeTextEditor: TextEditor | undefined) {
        if (!activeTextEditor) {
            return;
        }

        const doc: TextDocument = activeTextEditor.document;
        const defaultDecorationOptions: DecorationOptions[] = [];
        const configHandler = VSCOBOLConfiguration.get_resource_settings(doc, VSExternalFeatures);
        const textLanguage: TextLanguage = VSExtensionUtils.isSupportedLanguage(doc);

        if (textLanguage === TextLanguage.Unknown) {
            activeTextEditor.setDecorations(defaultTrailingSpacesDecoration, defaultDecorationOptions);
            return;
        }

        /* is it enabled? */
        if (textLanguage === TextLanguage.JCL) {
            if (!this.isEnabledViaWorkspace4jcl(configHandler)) {
                activeTextEditor.setDecorations(defaultTrailingSpacesDecoration, defaultDecorationOptions);
            } else {
                await this.updateJCLDecorations(doc, activeTextEditor, defaultTrailingSpacesDecoration);
            }
            return;
        }

        /* is it enabled? */
        if (configHandler.margin === false) {
            activeTextEditor.setDecorations(defaultTrailingSpacesDecoration, defaultDecorationOptions);
            return;
        }

        const declsMap = new Map<string, DecorationOptions[]>();
        let sf: ESourceFormat = ESourceFormat.unknown;

        // check overrides..
        switch (configHandler.fileformat_strategy) {
            case fileformatStrategy.AlwaysFixed:
                sf = ESourceFormat.fixed;
                break;
            case fileformatStrategy.AlwaysVariable:
                sf = ESourceFormat.variable;
                break;
            default:
                {
                    const gcp = VSCOBOLSourceScanner.getCachedObject(doc, configHandler, !configHandler.codelens_copybook_refresh);
                    if (gcp === undefined) {
                        const vsfile = new VSCodeSourceHandlerLite(doc);
                        sf = SourceFormat.get(vsfile, configHandler);
                    } else {
                        sf = gcp.sourceFormat;
                    }
                    break;
                }
        }

        let decSequenceNumber=true;
        let decArea73_80=configHandler.margin_identification_area;

        // use the known file format from the scan itself
        switch (sf) {
            case ESourceFormat.free:
            case ESourceFormat.unknown:
            case ESourceFormat.terminal:
                activeTextEditor.setDecorations(defaultTrailingSpacesDecoration, defaultDecorationOptions);
                return;
        }

        if (sf == ESourceFormat.variable) {
            decArea73_80 = false;
        }


        const maxLineLength = configHandler.editor_maxTokenizationLineLength;
        const maxLines = doc.lineCount;
        for (const [tag,] of this.tags) {
            declsMap.set(tag, []);
        }

        for (let i = 0; i < maxLines; i++) {
            const lineText = doc.lineAt(i);
            const line = lineText.text;
            if (line.length > maxLineLength) {
                continue;
            }

            // only do it, if we have no tabs on the line..
            const containsTab = line.indexOf("\t");

            if (decSequenceNumber && (containsTab === -1 || containsTab >= 6) && line.length >= 6) {
                const startPos = new Position(i, 0);
                const endPos = new Position(i, 6);
                const rangePos = new Range(startPos, endPos);
                const decoration = { range: rangePos };
                let useDefault = true;

                if (configHandler.enable_columns_tags) {
                    const text = doc.getText(rangePos);
                    if (text.length !== 0) {

                        for (const [tag,] of this.tags) {
                            // ignore tags too large for the left margin
                            if (tag.length > 6) {
                                continue;
                            }

                            const tagIndex = text.indexOf(tag);
                            if (tagIndex !== -1) {
                                const items = declsMap.get(tag);
                                if (items !== undefined) {
                                    useDefault = false;

                                    if (tagIndex === 0 && tag.length === 6) {
                                        items.push(decoration);
                                    }
                                    else {
                                        const rangePosX = new Range(new Position(i, tagIndex), new Position(i, tagIndex + tag.length));
                                        items.push({ range: rangePosX });

                                        // add left colour
                                        if (tagIndex !== 0) {
                                            const rangePosL = new Range(new Position(i, 0), new Position(i, tagIndex));
                                            defaultDecorationOptions.push({ range: rangePosL });
                                        }

                                        // add right color
                                        if (tagIndex + tag.length !== 6) {
                                            const rangePosR = new Range(new Position(i, tagIndex + tag.length), new Position(i, 6));
                                            defaultDecorationOptions.push({ range: rangePosR });
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    if (useDefault) {
                        defaultDecorationOptions.push(decoration);
                    }
                }
            }

            if (decArea73_80 && (containsTab === -1 || containsTab > 80) && line.length > 72) {
                // only colour 72-80
                const rangePos = new Range(new Position(i, 72), new Position(i, (line.length < 80 ? line.length : 80)));
                const decoration = { range: rangePos };
                let useDefault = true;

                if (configHandler.enable_columns_tags) {
                    const text = doc.getText(rangePos);
                    if (text.length !== 0) {

                        for (const [tag,] of this.tags) {
                            // ignore tags too large for the left margin
                            if (tag.length > 8) {
                                continue;
                            }
                            const tagIndex = text.indexOf(tag);
                            if (tagIndex !== -1) {
                                const items = declsMap.get(tag);
                                if (items !== undefined) {
                                    useDefault = false;

                                    if (tagIndex === 0 && tag.length === 8) {
                                        items.push(decoration);
                                    }
                                    else {
                                        const rangePosX = new Range(new Position(i, 72 + tagIndex), new Position(i, 72 + tagIndex + tag.length));
                                        items.push({ range: rangePosX });

                                        // add left colour
                                        if (tagIndex !== 0) {
                                            const rangePosL = new Range(new Position(i, 72), new Position(i, 72 + tagIndex));
                                            defaultDecorationOptions.push({ range: rangePosL });
                                        }

                                        // add right color
                                        if (tagIndex + tag.length !== 8) {
                                            const rangePosR = new Range(new Position(i, 72 + tagIndex + tag.length), new Position(i, 72 + 8));
                                            defaultDecorationOptions.push({ range: rangePosR });
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    if (useDefault) {
                        defaultDecorationOptions.push(decoration);
                    }
                }
            }
        }

        for (const [tag, decls] of declsMap) {
            const typeDecl = this.tags.get(tag);
            if (typeDecl !== undefined) {
                activeTextEditor.setDecorations(typeDecl, decls);
            }

        }
        activeTextEditor.setDecorations(defaultTrailingSpacesDecoration, defaultDecorationOptions);
    }
}

export const vsMarginHandler = new VSmargindecorations();
