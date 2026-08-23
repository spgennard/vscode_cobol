/* eslint-disable @typescript-eslint/no-explicit-any */
import * as vscode from "vscode";
import { COBOLToken } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { ICOBOLSourceScanner } from "./icobolsourcescanner";
import { CopybookExpansionBuilder } from "./copybookexpansion";

export class VSPPCodeLens implements vscode.CodeLensProvider, vscode.Disposable {
    private _onDidChangeCodeLenses: vscode.EventEmitter<void> = new vscode.EventEmitter<void>();
    public readonly onDidChangeCodeLenses: vscode.Event<void> = this._onDidChangeCodeLenses.event;
    private readonly configurationListener: vscode.Disposable;

    constructor() {

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        this.configurationListener = vscode.workspace.onDidChangeConfiguration((_) => {
            this._onDidChangeCodeLenses.fire();
        });
    }

    public dispose(): void {
        this.configurationListener.dispose();
        this._onDidChangeCodeLenses.dispose();
    }

    private scanTargetUse(settings: ICOBOLSettings, document: vscode.TextDocument, lens: vscode.CodeLens[], current: ICOBOLSourceScanner, target: string, targetToken: COBOLToken) {
        // not interested
        if (targetToken.isFromScanCommentsForReferences || targetToken.ignoreInOutlineView) {
            return;
        }

        const refs = current.sourceReferences.targetReferences.get(target);
        if (refs !== undefined && settings.enable_codelens_section_paragraph_references) {
            const tupRefs = current.sourceReferences.getReferenceInformation4targetRefs(target, current.sourceFileId, targetToken.startLine, targetToken.startColumn);

            // no references found
            if (tupRefs[0] === 0 || tupRefs[1] === 0) {
                return;
            }

            const refCount = tupRefs[1];
            const refCountMsg = refCount === 1 ? `${refCount} reference` : `${refCount} references`;
            const r = new vscode.Range(new vscode.Position(targetToken.rangeStartLine, targetToken.rangeStartColumn),
                new vscode.Position(targetToken.rangeEndLine, targetToken.rangeEndColumn));

            const cl = new vscode.CodeLens(r);
            cl.command = {
                title: `${refCountMsg}`,
                command: "editor.action.findReferences",
                arguments: [
                    document.uri, new vscode.Position(targetToken.rangeStartLine, targetToken.rangeStartColumn)
                ]
            };

            lens.push(cl);
        }
    }

    public provideCodeLenses(document: vscode.TextDocument, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CodeLens[]> {
        const lens: vscode.CodeLens[] = [];

        const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
        if (!settings.enable_codelens_variable_references &&
            !settings.enable_codelens_section_paragraph_references &&
            !settings.enable_codelens_copy_replacing) {
            return lens;
        }
        
        const current: ICOBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (current === undefined) {
            return lens;
        }

        const sourceFileId = current.sourceFileId;

        if (settings.enable_codelens_variable_references &&
            current.sourceReferences !== undefined &&
            current.sourceReferences.constantsOrVariablesReferences !== undefined) {
            for (const [avar, vars] of current.constantsOrVariables) {
                for (const currentVar of vars) {
                    const currentToken = currentVar.token;
                    if (currentToken.isFromScanCommentsForReferences || currentToken.ignoreInOutlineView) {
                        continue;
                    }

                    const tupRefs = current.sourceReferences.getReferenceInformation4variables(avar, sourceFileId, currentToken.startLine, currentToken.startColumn);

                    // no references found
                    if (tupRefs[0] === 0 || tupRefs[1] === 0) {
                        continue;
                    }

                    const refCount = tupRefs[1];
                    let refCountMsg = refCount === 1 ? `${refCount} reference` : `${refCount} references`;
                    if (vars.length !== 1) {
                        //refCountMsg = `View references [${currentToken.tokenName}]`;
                        refCountMsg = `View references`;
                    }

                    const r = new vscode.Range(new vscode.Position(currentToken.rangeStartLine, currentToken.rangeStartColumn),
                        new vscode.Position(currentToken.rangeEndLine, currentToken.rangeEndColumn));

                    const cl = new vscode.CodeLens(r);
                    cl.command = {
                        title: `${refCountMsg}`,
                        command: "editor.action.findReferences",
                        arguments: [
                            document.uri, new vscode.Position(currentToken.startLine, currentToken.startColumn)
                        ]
                    };

                    lens.push(cl);
                }

            }
        }

        // codelens for sections & paragraphs enabled?
        if (settings.enable_codelens_section_paragraph_references) {
            if (current.sourceReferences !== undefined && current.sourceReferences.sharedParagraphs !== undefined) {
                for (const [a, b] of current.sections) {
                    this.scanTargetUse(settings, document, lens, current, a, b);
                }

                for (const [a, b] of current.paragraphs) {
                    this.scanTargetUse(settings, document, lens, current, a, b);
                }
            }
        }

        // codelens for simple copy replacing
        if (settings.enable_codelens_copy_replacing) {
            for (const [, cbInfos] of current.copyBooksUsed) {
                for (const cbInfo of cbInfos) {
                    if (!cbInfo.scanComplete) {
                        continue;
                    }
                    if (cbInfo.statementInformation !== undefined && cbInfo.statementInformation.copyReplaceMap.size !== 0) {
                        const occurrence = CopybookExpansionBuilder.occurrence(cbInfo);
                        if (occurrence === undefined || cbInfo.statementInformation.sourceHandler === undefined) {
                            continue;
                        }
                        if (occurrence.sourceUri !== document.uri.toString()) {
                            continue;
                        }
                        const l = document.lineAt(cbInfo.statementInformation.startLineNumber);
                        const r = new vscode.Range(new vscode.Position(cbInfo.statementInformation.startLineNumber, 0),
                            new vscode.Position(cbInfo.statementInformation.startLineNumber, l.text.length));
                        let prevSrc = "";
                        let prevMaxLines = 10;
                        for (let c = 0; c < cbInfo.statementInformation.sourceHandler.getLineCount(); c++) {
                            if (prevMaxLines > 0) {
                                prevSrc += cbInfo.statementInformation.sourceHandler.getUpdatedLine(c);
                                prevSrc += "\n";
                                --prevMaxLines;
                            }
                        }

                        if (prevMaxLines <= 0) {
                            prevSrc += "\n......";
                        }

                        lens.push(this.commandLens(r, "Open copybook", "cobolplugin.openCopybook", occurrence));
                        lens.push(this.commandLens(r, "View expanded copybook", "cobolplugin.openExpandedCopybook", occurrence, prevSrc));
                        lens.push(this.commandLens(r, "Compare original with expanded", "cobolplugin.compareExpandedCopybook", occurrence));
                    }
                }
            }
        }
        return lens;
    }

    private commandLens(
        range: vscode.Range,
        title: string,
        command: string,
        occurrence: ReturnType<typeof CopybookExpansionBuilder.occurrence>,
        tooltip?: string
    ): vscode.CodeLens {
        const lens = new vscode.CodeLens(range);
        lens.command = { title, command, arguments: [occurrence], tooltip };
        return lens;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public resolveCodeLens(codeLens: vscode.CodeLens, token: vscode.CancellationToken): vscode.CodeLens {
        return codeLens;
    }

}
