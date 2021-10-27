import * as vscode from "vscode";

const problemMatches: string[][] = [
    [ "mfcobol-msbuild", "COBOLMSBuild"],
    [ "mfcobol-errformat2", "COBOLErrFormat2"],
    [ "mfcobol-errformat2-copybook", "COBOLErrFormat2Copybooks" ],
    [ "mfcobol-errformat3", "COBOLErrFormat3" ],
];


export class MigrationUtilsToMicroFocus {

    public static isMigrateProblemMatchersActive(doc: vscode.TextDocument):boolean {
        for (let l = 0; l < doc.lineCount; l++) {
            const lineAt = doc.lineAt(l);
            const text = lineAt.text;

            for(let c=0; c < text.length; c++) {
                for(let p=0; p<problemMatches.length; p++) {
                    const r1 = problemMatches[p][0];
                    const possPos = text.indexOf(r1, c);
                    if (possPos !== -1) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    public static migrateProblemMatchers(activeEditor: vscode.TextEditor):void {

        if (!activeEditor) {
            return;
        }

        const replaceRanges:vscode.Range[] = [];
        const replaceWith:string[] = [];
        const doc = activeEditor.document;
        for (let l = 0; l < doc.lineCount; l++) {
            const lineAt = doc.lineAt(l);
            const text = lineAt.text;

            for(let c=0; c < text.length; c++) {
                for(let p=0; p<problemMatches.length; p++) {
                    const r1 = problemMatches[p][0];
                    const possPos = text.indexOf(r1, c);
                    if (possPos !== -1) {
                        replaceRanges.push(
                            new vscode.Range(
                                new vscode.Position(l, possPos),
                                new vscode.Position(l, possPos+r1.length)
                            )
                        );
                        replaceWith.push(problemMatches[p][1]);
                        c = possPos+r1.length;    // skip forward
                    }
                }

            }
        }

        // nothing todo..
        if (replaceWith.length === 0) {
            vscode.commands.executeCommand("setContext", "coboleditor.enable_migrate2mf_tasks", false);
            return;
        }

        const uri = activeEditor.document.uri;
        const edits = new vscode.WorkspaceEdit();

        while(replaceWith.length !== 0) {
            const rw = replaceWith.pop();
            const rr = replaceRanges.pop();
            if (rw !== undefined && rr !== undefined) {
                edits.replace(uri, rr, rw);
            }
        }

        vscode.workspace.applyEdit(edits);

        vscode.commands.executeCommand("setContext", "coboleditor.enable_migrate2mf_tasks", false);
    }
}