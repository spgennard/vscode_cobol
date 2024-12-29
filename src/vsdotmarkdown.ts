import * as vscode from "vscode";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { SourceReference_Via_Length } from "./cobolsourcescanner";


export async function newFile_dot_callgraph(settings: ICOBOLSettings) {
    if (!vscode.window.activeTextEditor) {
        return;
    }

    const file = vscode.window.activeTextEditor.document.fileName;
    const current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, settings);
    if (current === undefined) {
        return;
    }
    let pos = file.lastIndexOf(".");
    const newFileName = file.substring(0, pos < 0 ? file.length : pos) + ".md";
    const doclang = "markdown";
    const ws = VSWorkspaceFolders.get(settings);
    if (ws) {
        const linesArray: string[] = [];
        // const sourceRefs: SharedSourceReferences = current.sourceReferences;
        const state = current.sourceReferences.state;
        linesArray.push("# " + current?.filename);
        linesArray.push("");
        linesArray.push("```mermaid");
        linesArray.push("graph TD;");
        
        for (const [section,] of current.sections) {
            const wordLower = section.toLowerCase();
            const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    const sr = targetRefs[trpos];
                    linesArray.push(`${section} --> ${sr.name};`);
                }
            }
        }
        for (const [paragraph,] of current.paragraphs) {
            const wordLower = paragraph.toLowerCase();
            const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    const sr = targetRefs[trpos];
                    linesArray.push(`${paragraph} --> ${sr.name};`);
                }
            }
        }
        linesArray.push("```")

        const furl = vscode.Uri.file(newFileName).with({ scheme: "untitled" });
        await vscode.workspace.openTextDocument(furl).then(async document => {
            const editor = await vscode.window.showTextDocument(document);
            if (editor !== undefined) {
                const linesAsOne = linesArray.join("\n"); linesArray
                await editor.insertSnippet(new vscode.SnippetString(linesAsOne), new vscode.Range(0, 0, 1 + linesArray.length, 0));
                await vscode.languages.setTextDocumentLanguage(document, doclang);
            }
        });
    }
}
