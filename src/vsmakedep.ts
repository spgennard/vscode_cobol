import * as vscode from "vscode";
import { VSLogger } from "./vslogger";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import path from "path";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { MakeDep } from "./makedeps";

export class VSMakeDep extends MakeDep {
    public static async MakeDependency(config: ICOBOLSettings) {
        if (vscode.window.activeTextEditor) {
            const current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, config);
            if (current === undefined) {
                return;
            }

            const sn = current.sourceHandler.getShortWorkspaceFilename();
            const sn_with_no_ext = path.basename(sn, path.extname(sn));
            const copyBookNames = new Array<string>();
            for (const [key,] of current.copyBooksUsed) {
                copyBookNames.push(key);
            }

            const processUnUsedCopyBooks = new Array<string>();
            for (const [key,] of current.copyBooksUnresolved) {
                processUnUsedCopyBooks.push(key);
            }

            const sb = VSMakeDep.CreateDependencyFile(config, sn, copyBookNames, processUnUsedCopyBooks, vscode.window.activeTextEditor.document.fileName);
            if (sb.length === 0) {
                return;
            }
            
            let fpath = "";
            const ws = VSWorkspaceFolders.get(config);
            if (ws) {
                fpath = path.join(ws[0].uri.fsPath, config.makefile_dependency_prefix +sn_with_no_ext + ".d");
            } else {
                fpath = path.join(process.cwd(), config.makefile_dependency_prefix + sn_with_no_ext + ".d");
            }
            const furl = vscode.Uri.file(fpath).with({ scheme: "untitled" });
            await vscode.workspace.openTextDocument(furl).then(async document => {
                const editor = await vscode.window.showTextDocument(document);
                if (editor !== undefined) {
                    await vscode.languages.setTextDocumentLanguage(document, "makefile");
                    editor.edit(edit => {
                        edit.insert(new vscode.Position(0, 0), sb.join("\n"))
                    });
                }
            });
            VSLogger.logMessage(sb.join("\n"));
        }
    }
}
