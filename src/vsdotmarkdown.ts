import * as vscode from "vscode";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { COBOLTokenStyle, SourceReference_Via_Length } from "./cobolsourcescanner";


export async function newFile_dot_callgraph(settings: ICOBOLSettings) {
    if (!vscode.window.activeTextEditor) {
        return;
    }

    const current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, settings);
    if (current === undefined) {
        return;
    }
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

        for (const [section, targetToken] of current.sections) {
            const wordLower = section.toLowerCase();
            const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);
            if (targetRefs !== undefined) {
                if (targetToken.isImplicitToken) {
                    linesArray.push(`${targetToken.tokenNameLower}[${targetToken.description}]`);
                }
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
        for (const [paragraph, targetToken] of current.paragraphs) {
            const wordLower = paragraph.toLowerCase();
            const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);
            if (targetRefs !== undefined) {
                if (targetToken.isImplicitToken) {
                    linesArray.push(`${targetToken.tokenNameLower}[${targetToken.description}]`);
                }

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

        vscode.workspace.openTextDocument({ language: "markdown" }).then(async document => {
            vscode.window.showTextDocument(document);
            const editor = await vscode.window.showTextDocument(document);
            if (editor !== undefined) {
                const linesAsOne = linesArray.join("\n");
                await editor.insertSnippet(new vscode.SnippetString(linesAsOne), new vscode.Range(0, 0, 1 + linesArray.length, 0));
                await vscode.languages.setTextDocumentLanguage(document, doclang);
            }
            // await document.save();
        });

    }
}
