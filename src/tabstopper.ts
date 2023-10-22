"use strict";

import { Position, Range, TextDocument, TextEditor, TextEditorEdit, Selection, window, commands } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";


export class TabUtils {

    // private static lineTabs = new Map<string, number[]>(
    //     [
    //         ["$SET", [7]]
    //     ]
    // );

    private static getTabsForLine(line:string, settings: ICOBOLSettings) {
        return settings.tabstops;    
    }

    public static executeTab(editor: TextEditor, doc: TextDocument, sel: readonly Selection[], inserting: boolean): void {
        const settings = VSCOBOLConfiguration.get();
        editor.edit(edit => {
            for (let x = 0; x < sel.length; x++) {
                if (sel[x].start.line === sel[x].end.line) {
                    const position = sel[x].start;
                    const thisLine = editor.document.lineAt(position.line).text;
                    const tabs = this.getTabsForLine(thisLine,settings);
                    if (inserting) {
                        TabUtils.singleSelectionTab(edit, doc, position, tabs);
                    } else {
                        TabUtils.singleSelectionUnTab(edit, doc, position, tabs);
                    }
                } else {
                    if (inserting) {
                        TabUtils.multipleSelectionTab(edit, doc, sel[x], settings);
                    } else {
                        TabUtils.multipleSelectionUnTab(edit, doc, sel[x], settings);
                    }
                }
            }
           
        });
    }


    private static singleSelectionTab(edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: number[]): void {
        const size = TabUtils.cobolTabSize(pos.character, tabs);
        edit.insert(pos, " ".repeat(size));
    }

    private static singleSelectionUnTab(edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: number[]): void {
        const size = TabUtils.cobolUnTabSize(pos.character, tabs);
        const range = new Range(pos.line, pos.character - size, pos.line, pos.character);
        const txt = d.getText(range);
        if (txt === " ".repeat(size)) {
            edit.delete(range);
        } else {
            for (let x = 0; x < size; x++) {
                commands.executeCommand("cursorMove", { to: "left" });
            }
        }
    }

    private static multipleSelectionTab(edit: TextEditorEdit, d: TextDocument, sel: Selection, settings: ICOBOLSettings): void {

        for (let line = sel.start.line; line <= sel.end.line; line++) {
            const pos = new Position(line, sel.start.character);
            const thisLine = d.lineAt(line).text;
            const tabs = this.getTabsForLine(thisLine,settings);
            TabUtils.singleSelectionTab(edit, d, pos, tabs);
        }
    }

    private static readonly multipleSelectionUnTabPttrn = /^\s*/;

    private static multipleSelectionUnTab(edit: TextEditorEdit, d: TextDocument, sel: Selection, settings: ICOBOLSettings): void {
        for (let line = sel.start.line; line <= sel.end.line; line++) {
            let charpos = sel.start.character;
            if (charpos === 0) {
                const selline = d.getText(sel);
                if (selline !== null) {
                    const match = selline.match(TabUtils.multipleSelectionUnTabPttrn);
                    if (match !== null) {
                        charpos = match[0].length;
                    }
                }
            }

            const pos = new Position(line, charpos);
            const lineText = d.lineAt(line).text;
            const tabs = this.getTabsForLine(lineText,settings);
            TabUtils.singleSelectionUnTab(edit, d, pos, tabs);
        }
    }

    private static cobolTabSize(pos: number, tabs: number[]): number {
        let tab = 0;
        for (let index = 0; index < tabs.length; index++) {
            tab = tabs[index];

            if (tab > pos) {
                return tab - pos;
            }
        }
        // outside range?
        return 4 - ((pos - tabs[tabs.length - 1]) % 4);
    }


    public static cobolUnTabSize(pos: number, tabs: number[]): number {

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

    public static processTabKey(inserting: boolean): void {
        const editor = window.activeTextEditor;
        if (editor) {
            const doc = editor.document;
            const sels = editor.selections;
            TabUtils.executeTab(editor, doc, sels, inserting);
        }
    }
}



