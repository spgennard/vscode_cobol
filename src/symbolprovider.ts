import * as vscode from 'vscode';

export default class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.DocumentSymbol[]> {
        let symbols: vscode.DocumentSymbol[] = [];
        let di: number;

        for (di = 0; di < document.lineCount; di++) {
            let line: string = document.lineAt(di).text;

            // is it a fixed comment?
            if (line.length > 7 && line[6] === '*') {
                continue;
            }

            var lineTokens = line.split(/[\s \,\.]/g);

            var tokens = [];

            // build up a list of token for this line
            for (var ti = 0; ti < lineTokens.length; ti++) {
                var token = lineTokens[ti];
                if (token.length === 0) {
                    continue;
                }
                if (token === "*>") {
                    break;
                }
                tokens.push(token);
            }

            var prev = "";

            for (var i = 0; i < tokens.length; i++) {
                var current = tokens[i].toLowerCase();

                // add program-id or class-id
                if ((prev === "program-id" ||
                    prev === 'class-id')
                    && current.length > 0) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + tokens[i],
                            "",
                            vscode.SymbolKind.Class,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }

                // add division
                if (prev.length !== 0 && current === "division") {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + tokens[i],
                            "",
                            vscode.SymbolKind.Module,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }

                // add sections
                if (prev.length !== 0 && current === "section") {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + tokens[i],
                            "",
                            vscode.SymbolKind.Method,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }

                // add entry-points or method-id
                if ((prev === "entry" ||
                    prev === 'method-id' ||
                    prev === 'function-id')
                    && current.length !== 0) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    let sym = prev === 'function-id' ? vscode.SymbolKind.Function : vscode.SymbolKind.Method;
                    current = current.replace(/\"/g,"");

                    if (current === "new") {
                        sym = vscode.SymbolKind.Constructor;
                    }
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + tokens[i],
                            "",
                            sym,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }

                prev = current;
            }
        }

        return symbols;
    }

}