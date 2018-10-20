import * as vscode from 'vscode';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';

export default class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    private isValid(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }
        id = id.replace(/\"/g, "");
        id = id.replace(/\'/g, "");

        let regex = /^[a-zA-Z][a-zA-Z0-9-_]*/g;

        if (id.match(regex)) {
            return true;
        }

        return false;
    }

    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.DocumentSymbol[]> {
        let symbols: vscode.DocumentSymbol[] = [];
        let di: number;
        let inProcedureDvision: boolean = false;
        let paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][ \\t]*$/g;
        let paraPrefixRegex2 = /^[ \\t]*$/g;
        let useDoggyParagraphcode: boolean = false;

        for (di = 0; di < document.lineCount; di++) {
            let line: string = document.lineAt(di).text;

            // is it a fixed comment?
            if (line.length > 7 && line[6] === '*') {
                continue;
            }

            var lineTokens = line.split(/[\s \,\t]/g);

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

            let prev = "";
            let inQuote = false;

            for (var i = 0; i < tokens.length; i++) {
                let current = tokens[i].toLowerCase();
                let currentUnchanged = tokens[i];

                // log quotes
                if (current.includes('"')) {
                    for (let ci = 0; ci < current.length; ci++) {
                        if (current[ci] === '"') {
                            inQuote = !inQuote;
                        }
                    }
                }
                endsWithPeriod = false;
                if (current.endsWith(".")) {
                    current = current.substring(0, current.length - 1);
                    currentUnchanged = currentUnchanged.substring(0, currentUnchanged.length - 1);
                    endsWithPeriod = true;
                }

                // add program-id or class-id
                if ((prev === "program-id" ||
                    prev === 'class-id')
                    && this.isValid(current)) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + currentUnchanged,
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
                            tokens[i - 1] + " " + currentUnchanged,
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
                            tokens[i - 1] + " " + currentUnchanged,
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
                            tokens[i - 1] + " " + currentUnchanged,
                            "",
                            vscode.SymbolKind.Struct,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }
                // add copybook
                if (prev === "copy" && this.isValid(current)) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    current = current.replace(/\"/g, "");
                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + currentUnchanged,
                            "",
                            vscode.SymbolKind.File,
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
                            tokens[i - 1] + " " + currentUnchanged,
                            "",
                            vscode.SymbolKind.Module,
                            range1,
                            range1
                        ));

                    if (prev === 'procedure') {
                        inProcedureDvision = true;
                    }
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
                            tokens[i - 1] + " " + currentUnchanged,
                            "",
                            vscode.SymbolKind.Module,
                            range1,
                            range1
                        ));


                    prev = current;
                    continue;
                }

                // add entry-points, function-id or program-id
                if ((prev === "program-id" ||
                    (prev === 'entry' && inProcedureDvision) ||
                    prev === 'function-id')
                    && this.isValid(current) && !inQuote) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    let sym = vscode.SymbolKind.Function;
                    current = current.replace(/\"/g, "");

                    let isOkay: boolean = false;
                    let prevLinePart = line.substr(0, line.indexOf(prev));

                    if (paraPrefixRegex2.test(prevLinePart)) {
                        isOkay = true;
                    }

                    if (isOkay) {
                        symbols.push(
                            new vscode.DocumentSymbol(
                                tokens[i - 1] + " " + currentUnchanged,
                                "",
                                sym,
                                range1,
                                range1
                            ));
                    }
                        prev = current;
                    continue;
                }

                // add method-id
                if (prev === 'method-id'
                    && this.isValid(current) && !inQuote) {
                    let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                    let sym = current === "new" ? vscode.SymbolKind.Constructor : vscode.SymbolKind.Method;
                    current = current.replace(/\"/g, "");

                    symbols.push(
                        new vscode.DocumentSymbol(
                            tokens[i - 1] + " " + currentUnchanged,
                            "",
                            sym,
                            range1,
                            range1
                        ));
                    prev = current;
                    continue;
                }

                if (useDoggyParagraphcode && 
                    this.isValid(current) && 
                    endsWithPeriod && inProcedureDvision && current.length !== 0) {
                    let isKeyword: boolean = cobolKeywordDictionary.containsKey(current);
                    let isPrevKeyword: boolean = cobolKeywordDictionary.containsKey(prev);
                    
                    if (!isKeyword && !isPrevKeyword) {

                        let isOkay: boolean = false;
                        let prevLinePart = line.substr(0, line.indexOf(current));

                        if (paraPrefixRegex1.test(prevLinePart) || paraPrefixRegex2.test(prevLinePart)) {
                            isOkay = true;
                        }

                        if (isOkay) {
                            let range1 = new vscode.Range(new vscode.Position(di, 0), new vscode.Position(di, line.length));
                            let sym = vscode.SymbolKind.Method;
                            symbols.push(
                                new vscode.DocumentSymbol(
                                    currentUnchanged,
                                    "",
                                    sym,
                                    range1,
                                    range1
                                ));
                            prev = current;
                        }
                        continue;
                    }
                }
                prev = current;
            }
        }

        return symbols;
    }

}