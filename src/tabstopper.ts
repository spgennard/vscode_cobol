"use strict";

import { Position, Range, TextDocument, TextEditor, TextEditorEdit, Selection, window, commands } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { IAnchorTabInfo, ICOBOLSettings } from "./iconfiguration";
import { VSExternalFeatures } from "./vsexternalfeatures";

export class TabUtils {

    // TODO: Look at move this into a workspace locale area
    private static readonly handler = new TabUtils();
    private tabregMap = new Map<IAnchorTabInfo, RegExp>();

    private escapeString(str: string) : string {
        const chrs:string[] = [];
        for(const c of str) {
            switch(c) {
                case "$": chrs.push("\\$");
                break;

                default:
                    chrs.push(c);
            }
            //[ ] { } ( ) \ ^ $ . | ? * +
        }

        return chrs.join("");
    }

    private makeRegex(partialRegEx: string): RegExp | undefined {
        try {
            return new RegExp(".*"+this.escapeString(partialRegEx)+".*$", "i");
        }
        catch {
            return undefined;
        }
    }

    private getTabsForLine(langId:string, line: string, settings: ICOBOLSettings): IAnchorTabInfo {

        if (settings.enable_tabstops_anchors) {
            const lineU = line.toLowerCase();
            for (const [lineTab,testReg] of this.tabregMap) {
                if (testReg.test(lineU)) {
                    return lineTab;
                }
            }
        }
        return { anchor: "", tabstops: settings.get_tabstops(langId), out_of_range_tabstop_size: settings.get_out_of_range_tabstop_size(langId) };
    }

    public async executeTab(editor: TextEditor, doc: TextDocument, sel: readonly Selection[], inserting: boolean): Promise<void> {
        const settings = VSCOBOLConfiguration.get_resource_settings(editor.document,VSExternalFeatures);

        if (this.tabregMap.size === 0) {
            for (const lineTab of settings.anchor_tabstops) {
                const newRegex = this.makeRegex(lineTab.anchor);
                if (newRegex)
                    this.tabregMap.set(lineTab, newRegex);
            }
        }
        
        editor.edit(edit => {
            const langId = editor.document.languageId;
            for (let x = 0; x < sel.length; x++) {
                if (sel[x].isSingleLine) {
                    const position = sel[x].start;
                    const thisLine = editor.document.lineAt(position.line).text;
                    const tabs = this.getTabsForLine(langId, thisLine, settings);
                    if (inserting) {
                        this.singleSelectionTab(settings, edit, doc, position, tabs);
                    } else {
                        this.singleSelectionUnTab(true, settings, edit, doc, position, tabs);
                    }
                } else {
                    if (inserting) {
                        this.multipleSelectionTab(settings, edit, doc, sel[x]);
                    } else {
                        this.multipleSelectionUnTab(settings, edit, doc, sel[x]);
                    }
                }
            }

        });
    }

    private async singleSelectionTab(settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: IAnchorTabInfo): Promise<void> {
        const size = this.cobolTabSize(pos.character, tabs);
        edit.insert(pos, " ".repeat(size));
    }

    private async singleSelectionUnTab(singleLine: boolean, settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, pos: Position, tabs: IAnchorTabInfo): Promise<void> {
        let charpos = pos.character;
        if (charpos === 0) {
            const selline = d.lineAt(pos.line).text;
            if (selline !== null) {
                const match = selline.match(this.multipleSelectionUnTabPttrn);
                if (match !== null) {
                    charpos = match[0].length;
                }
            }
        }

        const size = this.cobolUnTabSize(pos.character, tabs);
        const range = new Range(pos.line, pos.character - size, pos.line, pos.character);
        const txt = d.getText(range);

        if (txt === " ".repeat(size)) {
            edit.delete(range);
        } else {
            // no text, so no movement required
            if (singleLine && txt.length > 0) {
                for (let x = 0; x < size; x++) {
                    await commands.executeCommand("cursorMove", { to: "left" });
                }
            }
        }
    }

    private multipleSelectionTab(settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, sel: Selection): void {
        const langId = d.languageId;
        for (let line = sel.start.line; line <= sel.end.line; line++) {
            const pos = new Position(line, sel.start.character);
            const thisLine = d.lineAt(line).text;
            const tabs = this.getTabsForLine(langId, thisLine, settings);
            this.singleSelectionTab(settings, edit, d, pos, tabs);
        }
    }

    private readonly multipleSelectionUnTabPttrn = /^\s*/;

    private multipleSelectionUnTab(settings: ICOBOLSettings, edit: TextEditorEdit, d: TextDocument, sel: Selection): void {
        const langId = d.languageId;
        for (let line = sel.start.line; line <= sel.end.line; line++) {
            const charpos = sel.start.character;
            const pos = new Position(line, charpos);
            const lineText = d.lineAt(line).text;
            const tabs = this.getTabsForLine(langId, lineText, settings);
            this.singleSelectionUnTab(false, settings, edit, d, pos, tabs);
        }
    }

    private cobolTabSize(pos: number, tabinfo: IAnchorTabInfo): number {
        const tabs = tabinfo.tabstops;
        let tab = 0;
        for (let index = 0; index < tabs.length; index++) {
            tab = tabs[index];

            if (tab > pos) {
                return tab - pos;
            }
        }

        // outside range?
        const out_of_range_tabstop_size = tabinfo.out_of_range_tabstop_size;
        return out_of_range_tabstop_size === 0 ? 0 : out_of_range_tabstop_size - ((pos - tabs[tabs.length - 1]) % out_of_range_tabstop_size);
    }


    public cobolUnTabSize(pos: number, tabinfo: IAnchorTabInfo) {
        const tabs = tabinfo.tabstops;
        const out_of_range_tabstop_size = tabinfo.out_of_range_tabstop_size;

        // outside range?
        if (pos > tabs[tabs.length - 1]) {
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
            await TabUtils.handler.executeTab(editor, doc, sels, inserting);
        }
    }

    public static clearTabstopCache():void {
        TabUtils.handler.tabregMap.clear();
    }
}



