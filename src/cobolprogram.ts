'use strict';

import { Range, Selection, TextEditorRevealType, window } from 'vscode';

export function move2pd() {
    var line = findProcedureDivision();
    if (line > 0) {
        goToLine(line);
    } else {
        window.setStatusBarMessage('ERROR: \'PROCEDURE DIVISION\' not found.', 4000);
    }
}

export function move2dd() {
    let line = findDataDivision();
    if (line > 0) {
        goToLine(line);
        return;
    }

    line = findWorkingStorageSection();
    if (line > 0) {
        goToLine(line);
        return;
    }

    window.setStatusBarMessage('ERROR: \'DATA DIVISION\' or \'WORKING-STORAGE SECTION\' not found.', 4000);
}

export function move2ws() {
    const line = findWorkingStorageSection();
    if (line > 0) {
        goToLine(line);
        return;
    }

    window.setStatusBarMessage('ERROR: \'WORKING-STORAGE SECTION\' not found.', 4000);
}

export function move2anyforward() {
    const line = findAnyNext(1);
    if (line > 0) {
        goToLine(line);
    }
}

export function move2anybackwards() {
    const line = findAnyNext(-1);
    if (line > 0) {
        goToLine(line);
    }
}

function findMatch(mat: RegExp) {
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

function findAnyMatch(mats: RegExp[], counter: number) {
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

function findAnyNext(counter: number) {
    var mats = [
        /.*\s*division/i,
        /entry\s*"/i,
        /.*\s*section\./i,
        /eject/i,
        /program-id\./i,
        /class-id[\.|\s]/i,
        /method-id[\.|\s]/i];
    return findAnyMatch(mats, counter);
}

function findProcedureDivision() {
    return findMatch(/procedure\s*division/i);
}

function findDataDivision() {
    return findMatch(/data\s*division/i);
}

function findWorkingStorageSection() {
    return findMatch(/working-storage\s*section/i);
}

function goToLine(line: number) {
    if (window.activeTextEditor) {
        let reviewType = TextEditorRevealType.InCenter;
        if (line === window.activeTextEditor.selection.active.line)
        {
            reviewType = TextEditorRevealType.InCenterIfOutsideViewport;
        }
        let newSe = new Selection(line, 0, line, 0);
        window.activeTextEditor.selection = newSe;
        window.activeTextEditor.revealRange(newSe, reviewType);
    }
}
