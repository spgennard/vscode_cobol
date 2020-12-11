import * as vscode from 'vscode';
import { workspace } from 'vscode';
import COBOLSourceScanner, { splitArgument, camelize } from './cobolsourcescanner';
import { cobolKeywordDictionary, cobolRegistersDictionary, cobolStorageKeywordDictionary } from './keywords/cobolKeywords';
import { logMessage, isDirectory, logException, COBOLStatUtils } from './extension';
import { VSCodeSourceHandler } from './vscodesourcehandler';
import VSCOBOLSourceScanner from './vscobolscanner';
import { writeFileSync } from 'fs';
import path from 'path';
import { COBOLFileUtils } from './opencopybook';
import { VSCOBOLConfiguration } from './configuration';
import { getWorkspaceFolders } from './cobolfolders';
import { ICOBOLSettings } from './iconfiguration';

export enum FoldStyle {
    LowerCase = 1,
    UpperCase = 2,
    CamelCase = 3
}

export enum FoldAction {
    PerformTargets = 1,
    ConstantsOrVariables = 2,
    Keywords = 3
}

export class COBOLUtils {

    public static inCopybookdirs(config: ICOBOLSettings, copybookdir:string) : boolean {
        for(const ext of config.copybookdirs) {
            if (ext === copybookdir) {
                return true;
            }
        }

        return false;
    }

    public migrateCopybooksToWorkspace():void {
        const fileSearchDirectory = [];
        const settings = VSCOBOLConfiguration.get();
        const extsdir = settings.copybookdirs;

        let updateCopybookdirs = false;
        for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
            const ddir = extsdir[extsdirpos];

            if (COBOLFileUtils.isDirectPath(ddir)) {
                const ws = getWorkspaceFolders();
                if (workspace !== undefined && ws !== undefined) {
                    if (COBOLStatUtils.isPathInWorkspace(ddir) === false) {
                        if (COBOLFileUtils.isNetworkPath(ddir)) {
                            logMessage(" Adding " + ddir + " to workspace");
                            const uriToFolder = vscode.Uri.file(path.normalize(ddir));
                            vscode.workspace.updateWorkspaceFolders(ws.length, 0, { uri: uriToFolder });
                            updateCopybookdirs = true;
                        }
                    }
                }
            }
        }

        const ws = getWorkspaceFolders();
        if (ws !== undefined) {
            for (const folder of ws) {
                for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                    try {
                        const extdir = extsdir[extsdirpos];

                        let sdir: string;

                        if (COBOLFileUtils.isDirectPath(extdir) === false) {
                            sdir = path.join(folder.uri.fsPath, extdir);

                            if (isDirectory(sdir)) {
                                if (COBOLFileUtils.isNetworkPath(sdir)) {
                                    if (COBOLStatUtils.isPathInWorkspace(sdir) === false) {
                                        logMessage(" Adding " + sdir + " to workspace");
                                        const uriToFolder = vscode.Uri.file(path.normalize(sdir));
                                        vscode.workspace.updateWorkspaceFolders(ws.length, 0, { uri: uriToFolder });
                                        updateCopybookdirs = true;
                                    } else {
                                        fileSearchDirectory.push(sdir);
                                    }

                                    logMessage(" The directory " + sdir + " for performance should be part of the workspace");
                                } else {
                                    fileSearchDirectory.push(sdir);
                                }
                            }
                        }
                    }
                    catch (e) {
                        logException("dir", e);
                    }
                }
            }
        }

        // update copybookdirs with optiomized version
        if (updateCopybookdirs) {
            const editorConfig = workspace.getConfiguration('coboleditor');
            editorConfig.update('copybookdirs', fileSearchDirectory);
            logMessage("Copybook settings and workspace has been updated.");
        } else {
            logMessage("No copybook directories have been migrated");
        }

    }

    public extractSelectionToCopybook(activeTextEditor: vscode.TextEditor):void {
        const sel = activeTextEditor.selection;

        const ran = new vscode.Range(sel.start, sel.end);
        const text = activeTextEditor.document.getText(ran);
        const dir = path.dirname(activeTextEditor.document.fileName);

        vscode.window.showInputBox({
            prompt: 'Copybook name?',
            validateInput: (copybook_filename: string): string | undefined => {
                if (!copybook_filename || copybook_filename.indexOf(' ') !== -1 ||
                    copybook_filename.indexOf(".") !== -1 ||
                    COBOLStatUtils.isFile(path.join(dir, copybook_filename + ".cpy"))) {
                    return 'Invalid copybook';
                } else {
                    return undefined;
                }
            }
        }).then(copybook_filename => {
            // leave if we have no filename
            if (copybook_filename === undefined) {
                return;
            }
            const filename = path.join(dir, copybook_filename + ".cpy");
            writeFileSync(filename, text);

            activeTextEditor.edit(edit => {
                edit.replace(ran, "           copy \"" + copybook_filename + ".cpy\".");
            });
        });

    }

    public extractSelectionTo(activeTextEditor: vscode.TextEditor, para: boolean):void {
        const sel = activeTextEditor.selection;

        const ran = new vscode.Range(sel.start, sel.end);
        const text = activeTextEditor.document.getText(ran);


        // TODO - Should put something in place to search forward of current line
        //        to find the best position..

        vscode.window.showInputBox({
            prompt: para ? 'New paragrah name?' : 'New section name?',
            validateInput: (text: string): string | undefined => {
                if (!text || text.indexOf(' ') !== -1) {
                    return 'Invalid paragraph or section';
                } else {
                    return undefined;
                }
            }
        }).then(value => {
            // leave early
            if (value === undefined) {
                return;
            }

            activeTextEditor.edit(edit => {
                edit.replace(ran, "           perform "
                    + value + "\n\n       "
                    + value
                    + (para ? ".\n" : " section.\n")
                    + text
                    + "\n           .\n");
            });
        });

    }

    private pad(num: number, size: number): string {
        let s = num + "";
        while (s.length < size) {
            s = "0" + s;
        }
        return s;
    }

    public getMFUnitAnsiColorConfig(): boolean {
        const editorConfig = workspace.getConfiguration('coboleditor');
        let expEnabled = editorConfig.get<boolean>('mfunit.diagnostic.color');
        if (expEnabled === undefined || expEnabled === null) {
            expEnabled = false;
        }
        return expEnabled;
    }

    public resequenceColumnNumbers(activeEditor: vscode.TextEditor | undefined, startValue: number, increment: number): void {
        if (activeEditor === undefined) {
            return;
        }

        const edits = new vscode.WorkspaceEdit();
        const uri = activeEditor.document.uri;

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const lineAt = activeEditor.document.lineAt(l);
            const text = lineAt.text;

            if (text.length > 6) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l, 6);
                const range = new vscode.Range(startPos, endPos);
                const padString = this.pad(startValue + (l * increment), 6);
                edits.replace(uri, range, padString);
            }
        }

        vscode.workspace.applyEdit(edits);
    }

    public removeColumnNumbers(activeEditor: vscode.TextEditor): void {
        const edits = new vscode.WorkspaceEdit();
        const uri = activeEditor.document.uri;

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const lineAt = activeEditor.document.lineAt(l);
            const text = lineAt.text;

            if (text.length > 6) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l, 6);
                const range = new vscode.Range(startPos, endPos);
                edits.replace(uri, range, "      ");
            }
        }
        vscode.workspace.applyEdit(edits);
    }

    public RemoveIdentificationArea(activeEditor: vscode.TextEditor): void {
        const edits = new vscode.WorkspaceEdit();
        const uri = activeEditor.document.uri;

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const lineAt = activeEditor.document.lineAt(l);
            const text = lineAt.text;

            if (text.length > 73) {
                const startPos = new vscode.Position(l, 72);
                const endPos = new vscode.Position(l, text.length);
                const range = new vscode.Range(startPos, endPos);
                edits.delete(uri, range);
            }
        }
        vscode.workspace.applyEdit(edits);
    }

    public RemoveComments(activeEditor: vscode.TextEditor): void {
        const uri = activeEditor.document.uri;
        const edits = new vscode.WorkspaceEdit();
        const delimiters: string[] = [];
        const removeRanges: boolean[] = [];

        delimiters.push("\\*>");
        removeRanges.push(true);
        delimiters.push("^......\\*");
        removeRanges.push(false);

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const line = activeEditor.document.lineAt(l);
            let matched = false;
            for (let i = 0; i < delimiters.length; i++) {
                if (!matched) {
                    const expression = delimiters[i].replace(/\//ig, "\\/");
                    const removeRange = removeRanges[i];
                    const regEx = new RegExp(expression, "ig");
                    const match = regEx.exec(line.text);
                    if (match) {
                        if (removeRange) {
                            const startPos = new vscode.Position(l, match.index);
                            const endPos = new vscode.Position(l, line.text.length);
                            const range = new vscode.Range(startPos, endPos);
                            edits.delete(uri, range);
                            activeEditor.document.getText(range);
                        } else {
                            const startPos = new vscode.Position(l, match.index);
                            const endPos = new vscode.Position(l + 1, 0);
                            const range = new vscode.Range(startPos, endPos);
                            edits.delete(uri, range);
                        }

                        matched = true;
                    }
                }
            }
        }

        vscode.workspace.applyEdit(edits);
    }

    private isValidKeywordOrStorageKeyword(keyword: string): boolean {
        const keywordLower = keyword.toLowerCase();
        const isKeyword=cobolKeywordDictionary.has(keywordLower);
        if (isKeyword) {
            return true;
        }

        if(cobolStorageKeywordDictionary.has(keywordLower)) {
            return true;
        }

        return cobolRegistersDictionary.has(keywordLower);
    }

    public foldToken(activeEditor: vscode.TextEditor, action: FoldAction, foldstyle: FoldStyle):void {
        const uri = activeEditor.document.uri;

        const file = new VSCodeSourceHandler(activeEditor.document, false);
        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(activeEditor.document);
        if (current === undefined) {
            logMessage(`Unable to fold ${file.getFilename}, as it is has not been parsed`);
            return;
        }

        const edits = new vscode.WorkspaceEdit();
        // traverse all the lines
        for (let l = 0; l < file.getLineCount(); l++) {
            const lineAt = file.getLine(l);
            const text = lineAt;

            if (text === undefined) {
                break;      // eof
            }

            let newtext = text;
            const args: string[] = splitArgument(text, true);
            const textLower = text.toLowerCase();
            let lastPos = 0;
            for (let ic = 0; ic < args.length; ic++) {
                let arg = args[ic];
                if (arg.endsWith(".")) {
                    arg = arg.substr(0, arg.length - 1);
                }

                const argLower = arg.toLowerCase();
                const ipos = textLower.indexOf(argLower, lastPos);
                let actionIt = false;

                switch (action) {
                    case FoldAction.PerformTargets:
                        actionIt = current.sections.has(argLower);
                        if (actionIt === false) {
                            actionIt = current.paragraphs.has(argLower);
                        }
                        break;

                    case FoldAction.ConstantsOrVariables:
                        actionIt = current.constantsOrVariables.has(argLower);
                        break;

                    case FoldAction.Keywords:
                        actionIt = this.isValidKeywordOrStorageKeyword(argLower);
                        break;
                }

                if (actionIt) {
                    switch (foldstyle) {
                        case FoldStyle.LowerCase:
                            {
                                if (argLower !== arg) {
                                    const tmpline = newtext.substr(0, ipos) + argLower + newtext.substr(ipos + arg.length);
                                    newtext = tmpline;
                                    lastPos += arg.length;
                                }
                            }
                            break;
                        case FoldStyle.UpperCase:
                            {
                                const argUpper = arg.toUpperCase();
                                if (argUpper !== arg) {
                                    const tmpline = newtext.substr(0, ipos) + argUpper + newtext.substr(ipos + arg.length);
                                    newtext = tmpline;
                                    lastPos += arg.length;
                                }
                            }
                            break;
                        case FoldStyle.CamelCase:
                            {
                                const camelArg = camelize(arg);
                                if (camelArg !== arg) {
                                    const tmpline = newtext.substr(0, ipos) + camelArg + newtext.substr(ipos + arg.length);
                                    newtext = tmpline;
                                    lastPos += arg.length;
                                }
                            }
                            break;
                    }
                }
            }

            // one edit per line to avoid the odd overlapping error
            if (newtext !== text) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l, newtext.length);
                const range = new vscode.Range(startPos, endPos);
                edits.replace(uri, range, newtext);
            }
        }
        vscode.workspace.applyEdit(edits);
    }

}
