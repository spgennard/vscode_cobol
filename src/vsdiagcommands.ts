import * as vscode from "vscode";
import { VSLogger } from "./vslogger";

export class VSDiagCommands {

    private static dumpSymbol(depth: string, symbol: vscode.DocumentSymbol) {
        const symbolKind = vscode.SymbolKind[symbol.kind];

        VSLogger.logMessage(`${depth} Symbol : "${symbol.name}" of ${symbolKind} "${symbol.detail}" (${symbol.tags})`);
        for (const childSymbol of symbol.children) {
            VSDiagCommands.dumpSymbol(depth + " ", childSymbol);
        }
    }
    
    public static async DumpAllSymbols() {
        if (vscode.window.activeTextEditor) {
            const activeUrl = vscode.window.activeTextEditor.document.uri;
            const symbolsArray = await vscode.commands.executeCommand<[]>("vscode.executeDocumentSymbolProvider", activeUrl, "*");

            VSLogger.logMessage("Document Symbols found:")
            const symbols = symbolsArray as vscode.DocumentSymbol[];
            for (const symbol of symbols) {
                try {
                    VSDiagCommands.dumpSymbol("", symbol);
                } catch (e) {
                    VSLogger.logException(`Symbol: ${symbol.name}`, e as Error);
                }
            }
        }
    }
}