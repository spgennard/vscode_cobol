"use strict";

import { Position, Range, TextDocument, TextEditor, TextEditorEdit, Selection, window, commands } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";


export class TabUtils {

    private static lineTabs = new Map<string, number[]>(
        [
            ["$set", [7]]
        ]
    );

    private static getTabsForLine(line: string, settings: ICOBOLSettings) {
        const lineU = line.toLowerCase();
        for (const [key, tabs] of TabUtils.lineTabs) {
            if (lineU.indexOf(key) !== -1) {
                return tabs;
            }
        }
        return settings.tabstops;
    }

    public static async executeTab(editor: TextEditor, doc: TextDocument, sel: readonly Selection[], inserting: boolean): Promise<void> {
        const settings = VSCOBOLConfiguration.get();
        editor.edit(edit => {
            for (let x = 0; x < sel.length; x++) {
                if (sel[x].isSingleLine) {
                    const position = sel[x].start;
                    const thisLine = editor.document.lineAt(position.line).text;
                    const tabs = this.getTabsForLine(thisLine, settings);
                    if (inserting) {
                        TabUtils.singleSelectionTab(settings, edit, doc, position, tabs);
                    } else {
                        TabUtils.singleSelectionUnTab(settings, edit, doc, position, tabs);
                    }
                } else {
                    if (inserting) {
                        TabUtils.multipleSelectionTab(settings, edit, doc, sel[x]);
                    } else {
                        TabUtils.multipleSelectionUnTab(settings, edit, doc, sel[x]);
                    }
                }
            }

        });
    }


    private static async singleSelectionTab(settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: number[]): Promise<void> {
        const size = TabUtils.cobolTabSize(settings, pos.character, tabs);
        edit.insert(pos, " ".repeat(size));
    }

    private static async singleSelectionUnTab(settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: number[]): Promise<void> {
        const size = TabUtils.cobolUnTabSize(settings, pos.character, tabs);
        const range = new Range(pos.line, pos.character - size, pos.line, pos.character);
        const txt = d.getText(range);

        if (txt === " ".repeat(size)) {
            edit.delete(range);
        } else {
            // no text, so no movement required
            if (txt.length > 0) {
                for (let x = 0; x < size; x++) {
                    await commands.executeCommand("cursorMove", { to: "left" });
                }
            }
        }
    }

    private static multipleSelectionTab(settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, sel: Selection): void {
        for (let line = sel.start.line; line <= sel.end.line; line++) {
            const pos = new Position(line, sel.start.character);
            const thisLine = d.lineAt(line).text;
            const tabs = this.getTabsForLine(thisLine, settings);
            TabUtils.singleSelectionTab(settings, edit, d, pos, tabs);
        }
    }

    private static readonly multipleSelectionUnTabPttrn = /^\s*/;

    private static multipleSelectionUnTab(settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, sel: Selection): void {
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
            const tabs = this.getTabsForLine(lineText, settings);
            TabUtils.singleSelectionUnTab(settings, edit, d, pos, tabs);
        }
    }

    private static cobolTabSize(settings: ICOBOLSettings, pos: number, tabs: number[]): number {
        let tab = 0;
        for (let index = 0; index < tabs.length; index++) {
            tab = tabs[index];

            if (tab > pos) {
                return tab - pos;
            }
        }

        // outside range?
        const out_of_range_tabstop_size = settings.out_of_range_tabstop_size;
        return out_of_range_tabstop_size === 0 ? 0 : out_of_range_tabstop_size - ((pos - tabs[tabs.length - 1]) % out_of_range_tabstop_size);
    }


    public static cobolUnTabSize(settings: ICOBOLSettings, pos: number, tabs: number[]): number {

        // outside range?
        if (pos > tabs[tabs.length - 1]) {
            const out_of_range_tabstop_size = settings.out_of_range_tabstop_size;
            if (out_of_range_tabstop_size === 0) {
                return pos - tabs[tabs.length - 1];
            }

            if ((pos - tabs[tabs.length - 1]) % out_of_range_tabstop_size === 0) {
                return out_of_range_tabstop_size;
            }
            return (pos - tabs[tabs.length - 1]) % out_of_range_tabstop_size;
        }

        for (let index = tabs.length - 1; index > -1; index--) {
            const tab = tabs[index];
            if (tab < pos) {
                return pos - tab;
            }
        }
        return 0;
    }

    public static async processTabKey(inserting: boolean): Promise<void> {
        const editor = window.activeTextEditor;
        if (editor) {
            const doc = editor.document;
            const sels = editor.selections;
            await TabUtils.executeTab(editor, doc, sels, inserting);
        }
    }
}



