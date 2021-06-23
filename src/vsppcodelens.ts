/* eslint-disable @typescript-eslint/no-explicit-any */
import * as vscode from 'vscode';
import COBOLSourceScanner from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './vsconfiguration';
import { ICOBOLSettings } from './iconfiguration';
import VSCOBOLSourceScanner from './vscobolscanner';

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

    private padInteger(num: number, sizeNumber: number): string {
        let numS = num.toString();
        while (numS.length < sizeNumber) {
            numS = "0" + numS;
        }

        return numS;
    }

    public provideCodeLenses(document: vscode.TextDocument, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CodeLens[]> {
        const lens: vscode.CodeLens[] = [];

        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, this.settings);
        if (current === undefined) {
            return lens;
        }

        for (const result of current.ppResults) {
            const r = new vscode.Range(new vscode.Position(result.atLine, 0), new vscode.Position(result.atLine, result.originalLine.length));
            const cl = new vscode.CodeLens(r);
            let tooltip = "", tooltip_lines = "";

            let maxLen = 2 + result.originalLine.length;
            if (3 + result.ppHandle.description.length > maxLen) {
                maxLen = 3 + result.ppHandle.description.length;
            }
            if (3 + result.ppHandle.bugReportEmail.length > maxLen) {
                maxLen = 3 + result.ppHandle.bugReportEmail.length;
            }
            if (3 + result.ppHandle.bugReportUrl.length > maxLen) {
                maxLen = 3 + result.ppHandle.bugReportUrl.length;
            }

            let c = 0;
            for (const line of result.replacedLines) {
                tooltip += `${line}\n`;
                tooltip_lines += `${this.padInteger(c, 5)} : "${line}"\n`;
                if (10 + line.length > maxLen) {
                    maxLen = line.length + 10;
                }
                c += 1;
            }

            let argCopybooks = "";
            for (const [copybook, file] of result.copybooks) {
                argCopybooks += `${copybook} = ${file}\n`;
                if (argCopybooks.length > maxLen) {
                    maxLen = argCopybooks.length;
                }
            }

            let arg = `Preprocessor ${result.ppHandle.id} changed ${result.replacedLines.length} lines\n`;
            arg += "Original Line:"
            arg += `"${result.originalLine}"\n`;
            arg += "-".repeat(maxLen) + "\n";
            arg += argCopybooks;
            arg += "-".repeat(maxLen) + "\n";
            arg += "LNum:\n";
            arg += `${tooltip_lines}`;
            arg += "-".repeat(maxLen) + "\n";
            arg += ` ${result.ppHandle.description}\n`;
            arg += ` ${result.ppHandle.bugReportEmail}\n`;
            arg += ` ${result.ppHandle.bugReportUrl}\n`;
            arg += "-".repeat(maxLen) + "\n";

            cl.command = {
                title: `Preprocessor ${result.ppHandle.id} changed ${result.replacedLines.length} lines`,
                tooltip: tooltip,
                command: "coboleditor.ppcodelenaction",
                arguments: [arg]
            };

            this.resolveCodeLens(cl, token);

            lens.push(cl);
        }


        for (const [, cbInfo] of current.copyBooksUsed) {
            if (cbInfo.parsed) {
                if (cbInfo.statementInformation.copyReplaceMap.size !== 0) {
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
                            title: `View copybook repacement`,
                            tooltip: prevSrc,
                            command: "coboleditor.ppcodelenaction",
                            arguments: [arg]
                        };
                        this.resolveCodeLens(cl, token);

                        lens.push(cl);
                    }
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
            language: 'text'
        }).then((document: vscode.TextDocument) => {
            vscode.window.showTextDocument(document).then(editor => {
                if (arg.startsWith("*>")) {
                    vscode.languages.setTextDocumentLanguage(editor.document, "COBOL");
                    return
                }
            })
        });
    }
}
