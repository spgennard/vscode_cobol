'use strict';

import { Position, Range, TextDocument, TextEditor, TextEditorEdit, Selection, window, workspace } from 'vscode';
import { VSCOBOLConfiguration } from './configuration';



function executeTab(editor: TextEditor, doc: TextDocument, sel: Selection[], inserting: boolean) {
    editor.edit(edit => {
        for (var x = 0; x < sel.length; x++) {
            if (sel[x].start.line === sel[x].end.line) {
                var position = sel[x].start;
                if (inserting) {
                    singleSelectionTab(edit, doc, position);
                } else {
                    singleSelectionUnTab(edit, doc, position);
                }
            } else {
                if (inserting) {
                    multipleSelectionTab(edit, doc, sel[x]);
                } else {
                    multipleSelectionUnTab(edit, doc, sel[x]);
                }
            }
        }
    });
}

function singleSelectionTab(edit: TextEditorEdit, d: TextDocument, pos: Position) {
    const size = tabSize(pos.character);
    edit.insert(pos, ' '.repeat(size));
}

function singleSelectionUnTab(edit: TextEditorEdit, d: TextDocument, pos: Position)  {
    const size = unTabSize(pos.character);
    const range = new Range(pos.line, pos.character - size, pos.line, pos.character);
    const txt = d.getText(range);
    if (txt === ' '.repeat(size)) {
        edit.delete(range);
    }
}

function multipleSelectionTab(edit: TextEditorEdit, d: TextDocument, sel: Selection) {
    for (let line = sel.start.line; line <= sel.end.line; line++) {
        const pos = new Position(line, sel.start.character);
        singleSelectionTab(edit, d, pos);
    }
}

const multipleSelectionUnTabPttrn = /^\s*/;

function multipleSelectionUnTab(edit: TextEditorEdit, d: TextDocument, sel: Selection) {
    for (let line = sel.start.line; line <= sel.end.line; line++) {
        var charpos =  sel.start.character;
        if (charpos === 0) {
            var selline = d.getText(sel);
            if (selline !== null) {
                var match = selline.match(multipleSelectionUnTabPttrn);
                if (match !== null) {
                    charpos = match[0].length;
                }
            }
        }

        const pos = new Position(line, charpos);
        singleSelectionUnTab(edit, d, pos);
    }
}

function tabSize(pos: number) {
    var tabs = VSCOBOLConfiguration.getTabStops();
    var tab = 0;
    for (var index = 0; index < tabs.length; index++) {
        tab = tabs[index];

        if(tab > pos) {
            return tab - pos;
        }
    }
    // outside range?
    return 4 - ((pos - tabs[tabs.length - 1]) % 4);
}


function unTabSize(pos: number) {
    var tabs = VSCOBOLConfiguration.getTabStops();

    // outside range?
    if (pos > tabs[tabs.length - 1]) {
        if ((pos - tabs[tabs.length - 1]) % 4 === 0) {
            return 4;
        }
        return (pos - tabs[tabs.length - 1]) % 4;
    }

    for (var index = tabs.length - 1; index > -1; index--) {
        var tab = tabs[index];
        if (tab < pos) {
            return pos - tab;
        }
    }
    return 0;
}

export function processTabKey(inserting: boolean) {
    const editor = window.activeTextEditor;
    if (editor) {
        const doc = editor.document;
        const sel = editor.selections;
        executeTab(editor, doc, sel, inserting);
    }
}
