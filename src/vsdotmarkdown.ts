import * as vscode from "vscode";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { COBOLTokenStyle, SourceReference_Via_Length } from "./cobolsourcescanner";


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
    const newFileName = file.substring(0, pos < 0 ? file.length : pos) + "_callgraph.md";
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

        for (const [section,targetToken] of current.sections) {
            const wordLower = section.toLowerCase();
            const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);
            if (targetRefs !== undefined) {
                for (const sr of targetRefs) {
                    // skip definition
                    if (sr.line === targetToken.startLine && sr.column === targetToken.startColumn) {
                        continue;
                    }
                    if (sr.tokenStyle === COBOLTokenStyle.Section) {
                        linesArray.push(`${targetToken.tokenNameLower} --> ${sr.nameLower};`);
                    }
                }
            }
        }
        for (const [paragraph,targetToken] of current.paragraphs) {
            const wordLower = paragraph.toLowerCase();
            const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);
            if (targetRefs !== undefined) {
                for (const sr of targetRefs) {
                    // skip definition
                    if (sr.line === targetToken.startLine && sr.column === targetToken.startColumn) {
                        continue;
                    }

                    if (sr.tokenStyle === COBOLTokenStyle.Paragraph) {
                        linesArray.push(`${targetToken.tokenNameLower} --> ${sr.nameLower}`);
                    }
                }
            }
        }
        linesArray.push("```")

        const furl = vscode.Uri.file(newFileName).with({ scheme: "untitled" });
        await vscode.workspace.openTextDocument(furl).then(async document => {
            const editor = await vscode.window.showTextDocument(document);
            if (editor !== undefined) {
                const linesAsOne = linesArray.join("\n"); 
                await editor.insertSnippet(new vscode.SnippetString(linesAsOne), new vscode.Range(0, 0, 1 + linesArray.length, 0));
                await vscode.languages.setTextDocumentLanguage(document, doclang);
            }
        });
    }
}
