/* eslint-disable @typescript-eslint/no-explicit-any */
import * as vscode from "vscode";
import { COBOLSourceScanner, COBOLToken } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { ExtensionDefaults } from "./extensionDefaults";

export class VSPPCodeLens implements vscode.CodeLensProvider {
    private _onDidChangeCodeLenses: vscode.EventEmitter<void> = new vscode.EventEmitter<void>();
    public readonly onDidChangeCodeLenses: vscode.Event<void> = this._onDidChangeCodeLenses.event;

    private settings: ICOBOLSettings;

    constructor() {
        this.settings = VSCOBOLConfiguration.get();

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        vscode.workspace.onDidChangeConfiguration((_) => {
            this._onDidChangeCodeLenses.fire();
        });
    }

    private scanTargetUse(document: vscode.TextDocument, lens: vscode.CodeLens[], current: COBOLSourceScanner, target: string, targetToken: COBOLToken) {
        const refs = current.sourceReferences.targetReferences.get(target);
        if (refs !== undefined && refs.length >= 1) {
            const r = new vscode.Range(new vscode.Position(targetToken.startLine, targetToken.startColumn),
                new vscode.Position(targetToken.endLine, targetToken.endColumn));

            const cl = new vscode.CodeLens(r);
            cl.command = {
                title: `${target} referenced : ${refs.length}`,
                tooltip: `${target} referenced ${refs.length} `,
                command: "editor.action.findReferences",
                arguments: [
                    document.uri, new vscode.Position(targetToken.startLine, targetToken.startColumn)
                ]
            };

            lens.push(cl);
        }
    }

    public provideCodeLenses(document: vscode.TextDocument, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CodeLens[]> {
        const lens: vscode.CodeLens[] = [];

        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, this.settings);
        if (current === undefined) {
            return lens;
        }

        if (current.sourceReferences !== undefined && current.sourceReferences.constantsOrVariablesReferences !== undefined) {
            for (const [a, b] of current.constantsOrVariables) {
                const refs = current.sourceReferences.constantsOrVariablesReferences.get(a);
                if (refs !== undefined && b.length >= 1) {
                    for (const currentRef of b) {
                        const firstReference = currentRef.token;
                        const r = new vscode.Range(new vscode.Position(firstReference.startLine, firstReference.startColumn),
                            new vscode.Position(firstReference.endLine, firstReference.endColumn));

                        const cl = new vscode.CodeLens(r);
                        cl.command = {
                            title: `${a} referenced : ${b.length+refs.length}`,
                            tooltip: `${a} referenced ${b.length+refs.length} `,
                            command: "editor.action.findReferences",
                            arguments: [
                                document.uri, new vscode.Position(firstReference.startLine, firstReference.startColumn)
                            ]
                        };

                        lens.push(cl);
                    }
                }
            }
        }

        if (current.sourceReferences !== undefined && current.sourceReferences.sharedParagraphs !== undefined) {
            for (const [a, b] of current.sections) {
                this.scanTargetUse(document, lens, current, a, b);
            }

            for (const [a, b] of current.paragraphs) {
                this.scanTargetUse(document, lens, current, a, b);
            }
        }


        for (const [, cbInfo] of current.copyBooksUsed) {
            if (!cbInfo.scanComplete) {

                continue;
            }
            if (cbInfo.statementInformation !== undefined && cbInfo.statementInformation.copyReplaceMap.size !== 0) {
                const l = document.lineAt(cbInfo.statementInformation.startLineNumber);
                const r = new vscode.Range(new vscode.Position(cbInfo.statementInformation.startLineNumber, 0),
                    new vscode.Position(cbInfo.statementInformation.startLineNumber, l.text.length));
                const cl = new vscode.CodeLens(r);
                let src = "";
                let prevSrc = "";
                let prevMaxLines = 10;
                if (cbInfo.statementInformation.sourceHandler !== undefined) {
                    for (let c = 0; c < cbInfo.statementInformation.sourceHandler?.getLineCount(); c++) {
                        src += cbInfo.statementInformation.sourceHandler?.getUpdatedLine(c);
                        src += "\n";
                        if (prevMaxLines > 0) {
                            prevSrc += cbInfo.statementInformation.sourceHandler?.getUpdatedLine(c);
                            prevSrc += "\n";
                            --prevMaxLines;
                        }
                    }
                }

                if (prevMaxLines <= 0) {
                    prevSrc += "\n......";
                }

                if (src.length !== 0) {
                    const arg = `*> Caution: This is an approximation\n*> Original file: ${cbInfo.statementInformation.fileName}\n${src}`;

                    cl.command = {
                        title: "View copybook repacement",
                        tooltip: prevSrc,
                        command: "cobolplugin.ppcodelenaction",
                        arguments: [arg]
                    };
                    this.resolveCodeLens(cl, token);

                    lens.push(cl);
                }
            }
        }
        return lens;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public resolveCodeLens(codeLens: vscode.CodeLens, token: vscode.CancellationToken): vscode.CodeLens {
        return codeLens;
    }

    public static actionCodeLens(arg: string): void {
        vscode.workspace.openTextDocument({
            content: `${arg}`,
            language: "text"
        }).then((document: vscode.TextDocument) => {
            vscode.window.showTextDocument(document).then(editor => {
                if (arg.startsWith("*>")) {
                    vscode.languages.setTextDocumentLanguage(editor.document, ExtensionDefaults.defaultCOBOLLanguage);
                    return;
                }
            });
        });
    }
}
