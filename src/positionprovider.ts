'use strict';

import { TextEditorRevealType, window, Selection } from 'vscode';
import * as vscode from 'vscode';

interface DocPositions {
    name: string;
    positions: vscode.Position[];
}

const allDocuments: DocPositions[] = [];

export function setPositionDocument(document: vscode.TextDocument, position: vscode.Position) {

    const index = allDocuments.findIndex(doc => doc.name === document.fileName);
    let doc: DocPositions = { name: '', positions: [] };

    if (index >= 0) {
        doc = allDocuments[index];
        doc.positions.push(position);
        allDocuments.splice(index, 1, doc);
    } else {
        doc.name = document.fileName.toString();
        doc.positions.push(position);
        allDocuments.push(doc);
    }
}

export function backPositionDocument(document: vscode.TextDocument) {

    const index = allDocuments.findIndex(doc => doc.name === document.fileName);

    if (index < 0) {
        return;
    }

    let doc: DocPositions = { name: '', positions: [] };
    doc = allDocuments[index];

    if (doc.positions.length ===0 ){
        return;
    }

    const lastPosition = doc.positions.length - 1;
    const line = doc.positions[lastPosition].line;
    const col = doc.positions[lastPosition].character;

    doc.positions.pop();
    allDocuments.splice(index, 1, doc);

    goToLine(line, col);

}

function goToLine(line: number, column: number) {
    if (window.activeTextEditor) {
        let reviewType = TextEditorRevealType.InCenter;
        if (line === window.activeTextEditor.selection.active.line) {
            reviewType = TextEditorRevealType.InCenterIfOutsideViewport;
        }
        let newSe = new Selection(line, column, line, column);
        window.activeTextEditor.selection = newSe;
        window.activeTextEditor.revealRange(newSe, reviewType);
    }
}

export function clearPositionDocument(fileName: string) {

    const index = allDocuments.findIndex(doc => doc.name === fileName);

    if (index < 0) {
        return;
    }

    allDocuments.splice(index, 1);

}
