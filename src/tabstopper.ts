'use strict';

import { Position, Range, TextDocument, TextEditor, TextEditorEdit, Selection, window, commands } from 'vscode';
import { VSCOBOLConfiguration } from './vsconfiguration';

function executeTab(editor: TextEditor, doc: TextDocument, sel: Selection[], inserting: boolean) {
    const tabs = VSCOBOLConfiguration.get().tabstops;
    editor.edit(edit => {
        for (let x = 0; x < sel.length; x++) {
            if (sel[x].start.line === sel[x].end.line) {
                const position = sel[x].start;
                if (inserting) {
                    singleSelectionTab(edit, doc, position, tabs);
                } else {
                    singleSelectionUnTab(edit, doc, position, tabs);
                }
            } else {
                if (inserting) {
                    multipleSelectionTab(edit, doc, sel[x], tabs);
                } else {
                    multipleSelectionUnTab(edit, doc, sel[x], tabs);
                }
            }
        }
    });
}

function singleSelectionTab(edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: number[]) {
    const size = tabSize(pos.character, tabs);
    edit.insert(pos, ' '.repeat(size));
}

function singleSelectionUnTab(edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: number[])  {
    const size = unTabSize(pos.character, tabs);
    const range = new Range(pos.line, pos.character - size, pos.line, pos.character);
    const txt = d.getText(range);
    if (txt === ' '.repeat(size)) {
        edit.delete(range);
    } else {
        for(let x=0; x<size; x++) {
            commands.executeCommand("cursorMove", { to: "left" });
        }
    }
}

function multipleSelectionTab(edit: TextEditorEdit, d: TextDocument, sel: Selection, tabs: number[]) {
    for (let line = sel.start.line; line <= sel.end.line; line++) {
        const pos = new Position(line, sel.start.character);
        singleSelectionTab(edit, d, pos, tabs);
    }
}

const multipleSelectionUnTabPttrn = /^\s*/;

function multipleSelectionUnTab(edit: TextEditorEdit, d: TextDocument, sel: Selection, tabs: number[]) {
    for (let line = sel.start.line; line <= sel.end.line; line++) {
        let charpos =  sel.start.character;
        if (charpos === 0) {
            const selline = d.getText(sel);
            if (selline !== null) {
                const match = selline.match(multipleSelectionUnTabPttrn);
                if (match !== null) {
                    charpos = match[0].length;
                }
            }
        }

        const pos = new Position(line, charpos);
        singleSelectionUnTab(edit, d, pos, tabs);
    }
}

function tabSize(pos: number, tabs: number[]) {
    let tab = 0;
    for (let index = 0; index < tabs.length; index++) {
        tab = tabs[index];

        if(tab > pos) {
            return tab - pos;
        }
    }
    // outside range?
    return 4 - ((pos - tabs[tabs.length - 1]) % 4);
}


function unTabSize(pos: number, tabs: number[]) {

    // outside range?
    if (pos > tabs[tabs.length - 1]) {
        if ((pos - tabs[tabs.length - 1]) % 4 === 0) {
            return 4;
        }
        return (pos - tabs[tabs.length - 1]) % 4;
    }

    for (let index = tabs.length - 1; index > -1; index--) {
        const tab = tabs[index];
        if (tab < pos) {
            return pos - tab;
        }
    }
    return 0;
}

export function processTabKey(inserting: boolean):void {
    const editor = window.activeTextEditor;
    if (editor) {
        const doc = editor.document;
        const sel = editor.selections;
        executeTab(editor, doc, sel, inserting);
    }
}
