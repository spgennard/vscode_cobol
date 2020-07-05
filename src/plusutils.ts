import * as vscode from 'vscode';
import { workspace } from 'vscode';
import COBOLQuickParse, { splitArgument, SharedSourceReferences, COBOLToken, camelize } from './cobolquickparse';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';
import { logException, logMessage } from './extension';
import { VSCodeSourceHandler } from './VSCodeSourceHandler';
import { VSCOBOLConfiguration } from './configuration';
import { FileSourceHandler } from './FileSourceHandler';

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
    private pad(num: number, size: number): string {
        let s = num + "";
        while (s.length < size) {
            s = "0" + s;
        }
        return s;
    }

    public getMFUnitAnsiColorConfig(): boolean {
        var editorConfig = workspace.getConfiguration('coboleditor');
        var expEnabled = editorConfig.get<boolean>('mfunit.diagnostic.color');
        if (expEnabled === undefined || expEnabled === null) {
            expEnabled = false;
        }
        return expEnabled;
    }

    public resequenceColumnNumbers(activeEditor: vscode.TextEditor | undefined, startValue: number, increment: number): any {
        if (activeEditor === undefined) {
            return;
        }

        let edits = new vscode.WorkspaceEdit();
        let uri = activeEditor.document.uri;

        // traverse all the lines
        for (var l = 0; l < activeEditor.document.lineCount; l++) {
            let lineAt = activeEditor.document.lineAt(l);
            let text = lineAt.text;

            if (text.length > 6) {
                let startPos = new vscode.Position(l, 0);
                let endPos = new vscode.Position(l, 6);
                let range = new vscode.Range(startPos, endPos);
                let padString = this.pad(startValue + (l * increment), 6);
                edits.replace(uri, range, padString);
            }
        }

        vscode.workspace.applyEdit(edits);
    }

    public removeColumnNumbers(activeEditor: vscode.TextEditor): any {
        let edits = new vscode.WorkspaceEdit();
        let uri = activeEditor.document.uri;

        // traverse all the lines
        for (var l = 0; l < activeEditor.document.lineCount; l++) {
            let lineAt = activeEditor.document.lineAt(l);
            let text = lineAt.text;

            if (text.length > 6) {
                let startPos = new vscode.Position(l, 0);
                let endPos = new vscode.Position(l, 6);
                let range = new vscode.Range(startPos, endPos);
                edits.replace(uri, range, "      ");
            }
        }
        vscode.workspace.applyEdit(edits);
    }

    public RemoveIdentificationArea(activeEditor: vscode.TextEditor): any {
        let edits = new vscode.WorkspaceEdit();
        let uri = activeEditor.document.uri;

        // traverse all the lines
        for (var l = 0; l < activeEditor.document.lineCount; l++) {
            let lineAt = activeEditor.document.lineAt(l);
            let text = lineAt.text;

            if (text.length > 73) {
                let startPos = new vscode.Position(l, 72);
                let endPos = new vscode.Position(l, text.length);
                let range = new vscode.Range(startPos, endPos);
                edits.delete(uri, range);
            }
        }
        vscode.workspace.applyEdit(edits);
    }

    public RemoveComments(activeEditor: vscode.TextEditor): any {
        let uri = activeEditor.document.uri;
        let edits = new vscode.WorkspaceEdit();
        let delimiters: string[] = [];
        let removeRanges: boolean[] = [];

        delimiters.push("\\*>");
        removeRanges.push(true);
        delimiters.push("^......\\*");
        removeRanges.push(false);

        // traverse all the lines
        for (var l = 0; l < activeEditor.document.lineCount; l++) {
            let line = activeEditor.document.lineAt(l);
            let matched = false;
            for (var i = 0; i < delimiters.length; i++) {
                if (!matched) {
                    let expression = delimiters[i].replace(/\//ig, "\\/");
                    let removeRange = removeRanges[i];
                    let regEx = new RegExp(expression, "ig");
                    let match = regEx.exec(line.text);
                    if (match) {
                        if (removeRange) {
                            let startPos = new vscode.Position(l, match.index);
                            let endPos = new vscode.Position(l, line.text.length);
                            let range = new vscode.Range(startPos, endPos);
                            edits.delete(uri, range);
                            let n = activeEditor.document.getText(range);
                            console.log("Removing : " + n);
                        } else {
                            let startPos = new vscode.Position(l, match.index);
                            let endPos = new vscode.Position(l + 1, 0);
                            let range = new vscode.Range(startPos, endPos);
                            edits.delete(uri, range);
                        }

                        matched = true;
                    }
                }
            }
        }

        vscode.workspace.applyEdit(edits);
    }

    private isValidKeyword(keyword: string): boolean {
        return cobolKeywordDictionary.containsKey(keyword.toLowerCase());
    }

    public foldToken(activeEditor: vscode.TextEditor, action: FoldAction, foldstyle: FoldStyle) {
        let uri = activeEditor.document.uri;

        let file = new VSCodeSourceHandler(activeEditor.document, false);
        let sourceRefs: SharedSourceReferences = new SharedSourceReferences();
        let current: COBOLQuickParse = new COBOLQuickParse(file, activeEditor.document.fileName, VSCOBOLConfiguration.get(), "", sourceRefs);

        let edits = new vscode.WorkspaceEdit();
        // traverse all the lines
        for (var l = 0; l < activeEditor.document.lineCount; l++) {
            let lineAt = activeEditor.document.lineAt(l);
            let text = lineAt.text;
            let newtext = text;

            let args: string[] = splitArgument(text, true);
            let textLower = text.toLowerCase();
            let lastPos = 0;
            for (let ic = 0; ic < args.length; ic++) {
                let arg = args[ic];
                if (arg.endsWith(".")) {
                    arg = arg.substr(0, arg.length - 1);
                }

                let argLower = arg.toLowerCase();
                let ipos = textLower.indexOf(argLower, lastPos);
                let actionIt = false;

                switch (action) {
                    case FoldAction.PerformTargets:
                        current.sections.has(argLower);
                        if (actionIt === false) {
                            actionIt = current.paragraphs.has(argLower);
                        }
                        break;

                    case FoldAction.ConstantsOrVariables:
                        actionIt = current.constantsOrVariables.has(argLower);
                        break;

                    case FoldAction.Keywords:
                        actionIt = this.isValidKeyword(argLower);
                        break;
                }

                if (actionIt) {
                    switch (foldstyle) {
                        case FoldStyle.LowerCase:
                            {
                                if (argLower !== arg) {
                                    let tmpline = newtext.substr(0, ipos) + argLower + newtext.substr(ipos + arg.length);
                                    newtext = tmpline;
                                    lastPos += arg.length;
                                }
                            }
                            break;
                        case FoldStyle.UpperCase:
                            {
                                let argUpper = arg.toUpperCase();
                                if (argUpper !== arg) {
                                    let tmpline = newtext.substr(0, ipos) + argUpper + newtext.substr(ipos + arg.length);
                                    newtext = tmpline;
                                    lastPos += arg.length;
                                }
                            }
                            break;
                        case FoldStyle.CamelCase:
                            {
                                let camelArg = camelize(arg);
                                if (camelArg !== arg) {
                                    let tmpline = newtext.substr(0, ipos) + camelArg + newtext.substr(ipos + arg.length);
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
                let startPos = new vscode.Position(l, 0);
                let endPos = new vscode.Position(l, newtext.length);
                let range = new vscode.Range(startPos, endPos);
                edits.replace(uri, range, newtext);
            }
        }
        vscode.workspace.applyEdit(edits);
    }


}