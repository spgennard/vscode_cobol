'use strict';

import { Range, TextEditor, TextDocument, TextEditorRevealType, Selection, window, workspace } from 'vscode';
import * as fs from 'fs';

const DEFAULT_COPYBOOK_EXTS = ["cpy"];
const DEFAULT_COPYBOOK_DIR = ["."];

var previousFile: string;
var previousLineNumber: number;

function getExtensions(): string[] {
    var editorConfig =  workspace.getConfiguration('coboleditor');
    editorConfig = editorConfig;
    var extensions = editorConfig.get<string[]>('copybookexts');
    if (!extensions || (extensions != null && extensions.length == 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    return extensions;
}

function getcopybookdirs(): string[] {
    var editorConfig =  workspace.getConfiguration('coboleditor');
    var dirs = editorConfig.get<string[]>('copybookdirs');
    if (!dirs || (dirs != null && dirs.length == 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }
    return dirs;
}


function extractText(str: string) {
    let getFirstMatchOrDefault =
        (s: string, pattern: RegExp) => {
            const match = s.match(pattern)
            return match ? match[1] : null;
        }

    if (/"/.test(str)) {
        return getFirstMatchOrDefault(str, /"(.*?)"/);
    }

    if (/'/.test(str)) {
        return getFirstMatchOrDefault(str, /'(.*?)'/);
    }

    const strl = str.toLowerCase();
    if (/copy/.test(strl)) {
        try {
            return getFirstMatchOrDefault(strl, /copy\s(.*)\./);
        } catch(e) {
            /* continue */
        }
        try {
            return getFirstMatchOrDefault(strl, /copy\s(.*)/);
        } catch(e) {
            /* continue */
        }
    }
    return str;
}

async function openFile(editor: TextEditor, filename: string, line = 0) {
    if (filename == "")
        return;

    var fileExtension = filename.split('.').pop();
    var fullPath = workspace.rootPath + '/' + filename ;

    var extsdir = getcopybookdirs();
    for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
        var extdir = extsdir[extsdirpos];

        const basefullPath = workspace.rootPath + "/" + extdir + '/' + filename ;

        //No extension?
        if (filename == fileExtension) {
            // search through the possible extensions
            const exts = getExtensions();
            for (let extpos = 0; extpos < exts.length; extpos++) {
                var ext = exts[extpos];
                var possibleFile = basefullPath + "." + ext;

                if (!fs.existsSync(possibleFile) === false) {
                    fullPath = possibleFile;
                    break;
                }
            }
        } else {
            if (!fs.existsSync(basefullPath) === false) {
                fullPath = basefullPath;
            }
        }
    }

    if (fs.existsSync(fullPath) === false) {
        window.showWarningMessage("Unable to locate : "+filename);
        return;
    }

    previousFile = editor.document.fileName;
    previousLineNumber = editor.selection.active.line;

    try {
        const doc = await workspace.openTextDocument(fullPath);
        await window.showTextDocument(doc)
        goToLine(line);
    } catch {
        window.showWarningMessage("Cannot open : " + fullPath);
    }
}

async function openSavedFile(editor: TextEditor, filename: string, line = 0)
{
    if (previousFile !== null) {
        try {
            const doc = await workspace.openTextDocument(previousFile);
            await window.showTextDocument(doc);
            goToLine(line);
        } catch {
            window.showWarningMessage("Cannot open previous file : " + previousFile);
        }
    }
}

export function openPreviousFile()
{
    var editor = window.activeTextEditor;
    if (editor) {
        openSavedFile(editor, previousFile, previousLineNumber);
    }
}

function goToLine(line: number) {
    const editor = window.activeTextEditor;
    if (editor) {
        let reviewType = TextEditorRevealType.InCenter;
        if (line === editor.selection.active.line) {
            reviewType = TextEditorRevealType.InCenterIfOutsideViewport;
        }
        const newSelection = new Selection(line, 0, line, 0);
        editor.selection = newSelection;
        editor.revealRange(newSelection, reviewType);
    }
}

function parseLine(doc: TextDocument, sel: Selection[]) {
    const range = new Range(sel[0].start.line, 0, sel[0].end.line, 999);
    const txt = doc.getText(range);
    const match = extractText(txt);
    return match != txt ? match : "";
}

export function openCopyBookFile()
{
    const editor = window.activeTextEditor;
    if (editor) {
        const doc = editor.document;
        const sel = editor.selections;
        if (sel.length > 1 || sel[0].start.line != sel[0].end.line)
            return;

        const filename = parseLine(doc, sel);
        if (filename) {
            openFile(editor, filename);
        }
    }
}
