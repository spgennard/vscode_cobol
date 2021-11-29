"use strict";

import { Range, Selection, TextEditorRevealType, window } from "vscode";

export class COBOLProgramCommands {
    public static move2pd(): void {
        const line = COBOLProgramCommands.findProcedureDivision();
        if (line > 0) {
            COBOLProgramCommands.goToLine(line);
        } else {
            window.setStatusBarMessage("ERROR: 'PROCEDURE DIVISION' not found.", 4000);
        }
    }

    public static move2dd(): void {
        let line = COBOLProgramCommands.findDataDivision();
        if (line > 0) {
            COBOLProgramCommands.goToLine(line);
            return;
        }

        line = COBOLProgramCommands.findWorkingStorageSection();
        if (line > 0) {
            COBOLProgramCommands.goToLine(line);
            return;
        }

        window.setStatusBarMessage("ERROR: 'DATA DIVISION' or 'WORKING-STORAGE SECTION' not found.", 4000);
    }

    public static move2ws(): void {
        const line = COBOLProgramCommands.findWorkingStorageSection();
        if (line > 0) {
            COBOLProgramCommands.goToLine(line);
            return;
        }

        window.setStatusBarMessage("ERROR: 'WORKING-STORAGE SECTION' not found.", 4000);
    }

    public static move2anyforward(): void {
        const line = COBOLProgramCommands.findAnyNext(1);
        if (line > 0) {
            COBOLProgramCommands.goToLine(line);
        }
    }

    public static move2anybackwards(): void {
        const line = COBOLProgramCommands.findAnyNext(-1);
        if (line > 0) {
            COBOLProgramCommands.goToLine(line);
        }
    }

    private static findMatch(mat: RegExp) {
        if (window.activeTextEditor) {
            const doc = window.activeTextEditor.document;
            for (let line = 0; line <= doc.lineCount; line++) {
                const range = new Range(line, 0, line, 132);
                const txt = doc.getText(range);
                if (txt.match(mat)) {
                    return line;
                }
            }
        }
        return 0;
    }

    private static findAnyMatch(mats: RegExp[], counter: number) {
        if (window.activeTextEditor) {
            const doc = window.activeTextEditor.document;
            const editor = window.activeTextEditor;
            const endValue = counter === 1 ? doc.lineCount : 0;
            let line = editor.selection.active.line + counter;
            for (; line !== endValue; line += counter) {
                const range = new Range(line, 0, line, 132);
                const txt = doc.getText(range);
                const matsLen = mats.length;
                for (let matpos = 0; matpos < matsLen; matpos++) {
                    const mat = mats[matpos];
                    if (txt.match(mat)) {
                        return line;
                    }
                }
            }
        }
        return 0;
    }

    private static findAnyNext(counter: number) {
        const mats = [
            /.*\s*division/i,
            /entry\s*"/i,
            /.*\s*section\./i,
            /eject/i,
            /program-id\./i,
            /class-id[.|\s]/i,
            /method-id[.|\s]/i];
        return COBOLProgramCommands.findAnyMatch(mats, counter);
    }

    private static findProcedureDivision() {
        return COBOLProgramCommands.findMatch(/procedure\s*division/i);
    }

    private static findDataDivision() {
        return COBOLProgramCommands.findMatch(/data\s*division/i);
    }

    private static findWorkingStorageSection() {
        return COBOLProgramCommands.findMatch(/working-storage\s*section/i);
    }


    private static goToLine(line: number) {
        if (window.activeTextEditor) {
            let reviewType = TextEditorRevealType.InCenter;
            if (line === window.activeTextEditor.selection.active.line) {
                reviewType = TextEditorRevealType.InCenterIfOutsideViewport;
            }
            const newSe = new Selection(line, 0, line, 0);
            window.activeTextEditor.selection = newSe;
            window.activeTextEditor.revealRange(newSe, reviewType);
        }
    }

}










