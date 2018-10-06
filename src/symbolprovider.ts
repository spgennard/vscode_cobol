import * as vscode from 'vscode';

export default class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    private isValid(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }
        id = id.replace(/\"/g, "");

        let regex = /^[a-zA-Z][a-zA-Z0-9-]*/g;

        if (id.match(regex)) {
            return true;
        }

        return false;
    }

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
            let endsWithPeriod: boolean = false;


            // build up a list of token for this line
            for (var ti = 0; ti < lineTokens.length; ti++) {
                let token: string = lineTokens[ti];

                if (token.length === 0) {
                    continue;
                }
                if (token === "*>") {
                    break;
                }
                tokens.push(token);
            }

            var prev = "";

            var inQuote = false;

            for (var i = 0; i < tokens.length; i++) {
                var current = tokens[i].toLowerCase();

                // log quotes
                if (current.includes('"')) {
                    for (let ci = 0; ci < current.length; ci++) {
                        if (current[ci] === '"') {
                            inQuote = !inQuote;
                        }
                    }

                    continue;
                }
                endsWithPeriod = false;
                if (current.endsWith(".")) {
                    current = current.substring(0, current.length - 1);
                    endsWithPeriod = true;
                }

                // add program-id or class-id
                if ((prev === "program-id" ||
                    prev === 'class-id')
                    && this.isValid(current)) {
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

                // add enum-id 
                if (prev === "enum-id" && this.isValid(current)) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + tokens[i],
                            "",
                            vscode.SymbolKind.Enum,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }

                // add interface-id 
                if (prev === "interface-id" && this.isValid(current)) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + tokens[i],
                            "",
                            vscode.SymbolKind.Interface,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }
                
                // add valuetype-id 
                if (prev === "valuetype-id" && this.isValid(current)) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + tokens[i],
                            "",
                            vscode.SymbolKind.Struct,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }

                // add division
                if (this.isValid(prev) && current === "division" && !inQuote) {
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
                if (this.isValid(prev) && current === "section" && !inQuote) {
                    // avoid matching "exit section" or "declare"
                    if (prev === 'exit' || prev === 'declare') {
                        continue;
                    }
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
                    && this.isValid(current) && !inQuote) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    let sym = prev === 'function-id' ? vscode.SymbolKind.Function : vscode.SymbolKind.Method;
                    current = current.replace(/\"/g, "");

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

                if (endsWithPeriod) {
                    console.log(current);
                }
                prev = current;
            }
        }

        return symbols;
    }

}