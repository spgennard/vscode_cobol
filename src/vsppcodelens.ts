/* eslint-disable @typescript-eslint/no-explicit-any */
import * as vscode from 'vscode';
import COBOLSourceScanner from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './configuration';
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

    public provideCodeLenses(document: vscode.TextDocument, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CodeLens[]> {
        const lens: vscode.CodeLens[] = [];

        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, this.settings);
        if (current === undefined) {
            return lens;
        }

        for(const result of current.ppResults) {
            const r = new vscode.Range(new vscode.Position(result.atLine, 0), new vscode.Position(result.atLine, result.originalLine.length));
            const cl = new vscode.CodeLens(r);
            let tooltip = "";
            for(const line of result.replacedLines) {
                tooltip += `${line}\n`;
            }
            cl.command = {
                title: `Preprocessor ${result.ppHandle.id} changed ${result.replacedLines.length} lines`,
                tooltip: tooltip,
                command: "codelens-sample.codelensAction",
                arguments: ["Argument 1", false]
            };

            this.resolveCodeLens(cl, token);
    
            lens.push(cl);
        }
 
        return lens;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public resolveCodeLens(codeLens: vscode.CodeLens, token: vscode.CancellationToken): vscode.CodeLens {
        // codeLens.command = 
        // codeLens.command = {
        //     title: codeLens.command?.title as string,
        //     tooltip: "Tooltip provided by sample extension",
        //     command: "codelens-sample.codelensAction",
        //     arguments: ["Argument 1", false]
        // };
        return codeLens;
    }

    public static actionCodeLens(args: string ):void {
        vscode.window.showInformationMessage(`CodeLens action clicked with args=${args}, size: ${args.length}`);
    }
}
