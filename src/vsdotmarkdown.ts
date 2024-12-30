import * as vscode from "vscode";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { COBOLToken, COBOLTokenStyle, ParseState, SourceReference_Via_Length } from "./cobolsourcescanner";

function generate_partial_graph(linesArray: string[], state: ParseState, para_or_section: Map<string, COBOLToken>) {
    for (const [paragraph, targetToken] of para_or_section) {
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

                if (sr.tokenStyle === COBOLTokenStyle.Paragraph || sr.tokenStyle === COBOLTokenStyle.Section) {
                    if (sr.reason === 'perform') {
                        linesArray.push(`${targetToken.tokenNameLower} --> ${sr.nameLower}`);
                    } else {
                        linesArray.push(`${targetToken.tokenNameLower} -->|${sr.reason}|${sr.nameLower}`);
                    }
                }
            }
        }
    }
}

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
        const state = current.sourceReferences.state;
        if (current.ImplicitProgramId.length !== 0) {
            linesArray.push("# " + current.ImplicitProgramId);
        } else {
            if (current.ProgramId.length !== 0) {
                linesArray.push("# " + current.ProgramId);
            } else {
                linesArray.push(" # Unknown");
            }
        }
        linesArray.push("");
        linesArray.push("```mermaid");
        linesArray.push("graph TD;");

        generate_partial_graph(linesArray, state, current.sections);
        generate_partial_graph(linesArray, state, current.paragraphs);
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
