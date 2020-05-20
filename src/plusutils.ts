import * as vscode from 'vscode';
import { workspace } from 'vscode';

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

    public resequenceColumnNumbers(activeEditor: vscode.TextEditor|undefined, startValue: number, increment: number): any {
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
}