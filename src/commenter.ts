'use strict';

import { Position, Range, TextDocument, TextEditor, TextEditorEdit, Selection, window, commands } from 'vscode';
import { VSCOBOLConfiguration } from './configuration';
import { getSourceFormat, ESourceFormat } from './margindecorations';

function commentLine(editor: TextEditor, doc: TextDocument, sel: Selection[], format: ESourceFormat) {
    editor.edit(edit => {
        for (var x = 0; x < sel.length; x++) {
            if (sel[x].start.line === sel[x].end.line) {
                var position = sel[x].start;
                toggleLine(edit, doc, position.line, format);
            } else {
                multipleSelectionTab(edit, doc, sel[x], format);
            }
        }
    });
}

function toggleLine(editor: TextEditorEdit, d: TextDocument, l: number, format: ESourceFormat) {
    let line = d.lineAt(l);
    let lineContents = line.text;

    if (format === ESourceFormat.terminal) {
        let firstInlineIndex = lineContents.indexOf("| ");
        if (firstInlineIndex !== -1) {
            let r = new Range(new Position(l, firstInlineIndex), new Position(l, 2 + firstInlineIndex));
            editor.delete(r);
            return;
        }
        let defpos = -1;
        for (let w = 0; w < lineContents.length; w++) {
            if (lineContents[w] === ' ') {
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
        if (firstInlineIndex !== -1) {
            let r = new Range(new Position(l, firstInlineIndex), new Position(l, 3 + firstInlineIndex));
            editor.delete(r);
            return;
        }

        firstInlineIndex = lineContents.indexOf("*>");
        if (firstInlineIndex !== -1) {
            let r = new Range(new Position(l, firstInlineIndex), new Position(l, 2 + firstInlineIndex));
            editor.delete(r);
            return;
        }

        let defpos = -1;
        for (let w = 0; w < lineContents.length; w++) {
            if (lineContents[w] === ' ') {
                defpos++;
            }
            else {
                break;
            }
        }

        if (defpos !== -1) {
            editor.insert(new Position(l, 1 + defpos), "*> ");
            return;
        }
        else if (lineContents.length > 6 &&
            lineContents[6] === ' ' &&
            lineContents[7] === ' ') {
            let r = new Range(new Position(l, 6), new Position(l, 8));
            editor.replace(r, "*>");
            return;
        }
    }

    /* remove * from column 6 */
    if (lineContents.length > 6 &&
        lineContents[6] === '*') {
        let r = new Range(new Position(l, 6), new Position(l, 7));
        editor.replace(r, " ");
        return;
    }

    if (lineContents.length > 6 && lineContents[6] === ' ') {
        let r = new Range(new Position(l, 6), new Position(l, 7));
        editor.replace(r, "*");
        return;
    }

}


function multipleSelectionTab(edit: TextEditorEdit, d: TextDocument, sel: Selection, format: ESourceFormat) {
    for (let line = sel.start.line; line <= sel.end.line; line++) {
        if (sel.start.character === 0) {
            toggleLine(edit, d, line, format);
        }
    }
}

export function processCommentLine() {
    const editor = window.activeTextEditor;
    if (editor) {
        const doc = editor.document;
        const sel = editor.selections;
        const format: ESourceFormat = getSourceFormat(doc, VSCOBOLConfiguration.get());
        commentLine(editor, doc, sel, format);
    }
}
