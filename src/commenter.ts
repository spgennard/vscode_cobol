"use strict";

import { Position, Range, TextDocument, TextEditor, TextEditorEdit, Selection, window } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ESourceFormat } from "./externalfeatures";
import { VSCOBOLSourceScanner } from "./vscobolscanner";

export class commentUtils {
    private static commentLine(editor: TextEditor, doc: TextDocument, sel: readonly Selection[], format: ESourceFormat) {
        editor.edit(edit => {
            for (let x = 0; x < sel.length; x++) {
                if (sel[x].start.line === sel[x].end.line) {
                    const position = sel[x].start;
                    commentUtils.toggleLine(edit, doc, position.line, format);
                } else {
                    commentUtils.multipleToggleLine(edit, doc, sel[x], format);
                }
            }
        });
    }

    static readonly var_free_insert_at_comment_column = true;

    private static toggleLine(editor: TextEditorEdit, d: TextDocument, l: number, format: ESourceFormat) {
        const line = d.lineAt(l);
        const lineContents = line.text;

        if (format === ESourceFormat.terminal) {
            const firstInlineIndex = lineContents.indexOf("|");
            if (firstInlineIndex !== -1) {
                const r = new Range(new Position(l, firstInlineIndex), new Position(l, 2 + firstInlineIndex));
                editor.delete(r);
                return;
            }
            let defpos = -1;
            for (let w = 0; w < lineContents.length; w++) {
                if (lineContents[w] === " ") {
                    defpos++;
                }
                else {
                    break;
                }
            }

            if (defpos !== -1) {
                editor.insert(new Position(l, 1 + defpos), "| ");
                return;
            }

            return;
        }

        if (format !== ESourceFormat.fixed) {
            let firstInlineIndex = lineContents.indexOf("*> ");
            if (firstInlineIndex !== -1 && firstInlineIndex !== 6) {
                const r = new Range(new Position(l, firstInlineIndex), new Position(l, 3 + firstInlineIndex));
                editor.delete(r);
                return;
            }

            firstInlineIndex = lineContents.indexOf("*>");
            if (firstInlineIndex !== -1) {
                const r = new Range(new Position(l, firstInlineIndex), new Position(l, 2 + firstInlineIndex));
                editor.delete(r);
                return;
            }

            let defpos = -1;
            for (let w = 0; w < lineContents.length; w++) {
                if (lineContents[w] === " ") {
                    defpos++;
                }
                else {
                    break;
                }
            }

            if (defpos !== -1) {
                // if we have a fixed column comment, convert it..
                if (defpos === 5 && lineContents[6] === "*") {
                    editor.insert(new Position(l, 7), "> ");
                    return;
                }

                // we can use the comment column
                if (defpos === 6) {   // defpos started at -1....
                    editor.insert(new Position(l, 6), "*>");
                    return;
                }

                if (commentUtils.var_free_insert_at_comment_column && defpos > 6) {
                    if (lineContents[6] === " " && lineContents[7] === " " && lineContents[8] === " ") {
                        editor.insert(new Position(l, 6), "*>");
                        return;
                    }
                }

                editor.insert(new Position(l, 1 + defpos), "*> ");
                return;
            }

            if (lineContents.length > 6 && lineContents[6] === " " && lineContents[7] === " ") {
                const r = new Range(new Position(l, 6), new Position(l, 8));
                editor.replace(r, "*>");
                return;
            }

            return;
        }

        /* remove * from column 6 */
        if (lineContents.length > 6 &&
            (lineContents[6] === "*" || lineContents[6] === "/")) {
            const r = new Range(new Position(l, 6), new Position(l, 7));
            editor.replace(r, " ");
            return;
        }

        if (lineContents.length > 6 && lineContents[6] === " ") {
            const r = new Range(new Position(l, 6), new Position(l, 7));
            editor.replace(r, "*");
            return;
        }

    }

    private static multipleToggleLine(edit: TextEditorEdit, d: TextDocument, sel: Selection, format: ESourceFormat) {
        for (let line = sel.start.line; line <= sel.end.line; line++) {
            commentUtils.toggleLine(edit, d, line, format);
        }
    }

    public static processCommentLine(): void {
        const editor = window.activeTextEditor;
        if (editor) {
            const doc = editor.document;
            const sel = editor.selections;
            let sourceformatStyle: ESourceFormat = ESourceFormat.variable;

            const settings = VSCOBOLConfiguration.get();
            const gcp = VSCOBOLSourceScanner.getCachedObject(doc, settings);
            if (gcp !== undefined) {
                sourceformatStyle = gcp.sourceFormat;
            }

            commentUtils.commentLine(editor, doc, sel, sourceformatStyle);
        }
    }
}
