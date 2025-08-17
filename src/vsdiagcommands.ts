import * as vscode from "vscode";
import { VSLogger } from "./vslogger";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { ICOBOLSettings } from "./iconfiguration";
import path from "path";

export class VSDiagCommands {

    private static dumpSymbol(sb: Array<string>, depth: string, symbol: vscode.DocumentSymbol) {
        const symbolKind = vscode.SymbolKind[symbol.kind];
        const detail = symbol.detail !== undefined && symbol.detail.length !== 0 ? ` "{symbol.detail}` : "";
        const tag = symbol.tags !== undefined ? ` (Tag=${symbol.tags.join(',')})` : "";

        const rrinfo = symbol.range.isSingleLine ?
            `${symbol.range.start.line}:${symbol.range.start.character}-${symbol.range.end.character}` :
            `${symbol.range.start.line}:${symbol.range.start.character} -> ${symbol.range.end.line}:${symbol.range.end.character}`

        const srinfo = symbol.selectionRange.isSingleLine ?
            `${symbol.selectionRange.start.line}:${symbol.selectionRange.start.character}-${symbol.selectionRange.end.character}` :
            `${symbol.selectionRange.start.line}:${symbol.selectionRange.start.character} -> ${symbol.selectionRange.end.line}:${symbol.selectionRange.end.character}`

        const rinfo = symbol.range.isSingleLine === false ? (rrinfo.localeCompare(srinfo) === 0 ? rrinfo : `${rrinfo} / ${srinfo}`) : rrinfo;

        sb.push(`${depth}  ${symbolKind} : "${symbol.name}" ${detail}${tag} @ ${rinfo}`);
        const activeEditor = vscode.window.activeTextEditor;
        const text = activeEditor?.document.getText(symbol.range);
        sb.push(`\`\`\`${activeEditor?.document.languageId}\n${text}\n\`\`\`\n\n`)
        for (const childSymbol of symbol.children) {
            VSDiagCommands.dumpSymbol(sb, depth + "#", childSymbol);
        }
    }

    public static async DumpAllSymbols(config: ICOBOLSettings) {
        if (vscode.window.activeTextEditor) {
            const sb = new Array<string>();
            const activeUrl = vscode.window.activeTextEditor.document.uri;
            const symbolsArray = await vscode.commands.executeCommand<[]>("vscode.executeDocumentSymbolProvider", activeUrl, "*");

            sb.push("Document Symbols found:")
            const symbols = symbolsArray as vscode.DocumentSymbol[];
            for (const symbol of symbols) {
                try {
                    VSDiagCommands.dumpSymbol(sb, "#", symbol);
                } catch (e) {
                    VSLogger.logException(`Symbol: ${symbol.name}`, e as Error);
                }
            }
            let fpath = "";
            let data="dump";
            const ws = VSWorkspaceFolders.get(config);
            if (ws) {
                fpath = path.join(ws[0].uri.fsPath, data + ".md");
            } else {
                fpath = path.join(process.cwd(), data + ".md");
            }
            const furl = vscode.Uri.file(fpath).with({ scheme: "untitled" });
            await vscode.workspace.openTextDocument(furl).then(async document => {
                const editor = await vscode.window.showTextDocument(document);
                if (editor !== undefined) {
                    await vscode.languages.setTextDocumentLanguage(document, "markdown");
                    editor.edit(edit => {
                        edit.insert(new vscode.Position(0, 0), sb.join("\n"))

                    });
                }
            });
            VSLogger.logMessage(sb.join("\n"));
        }
    }
}