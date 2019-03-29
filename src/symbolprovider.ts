import * as vscode from 'vscode';
import QuickCOBOLParse from './cobolquickparse';
import ISourceHandler from './isourcehandler';

class VSCodeSourceHandler implements ISourceHandler {

    document: vscode.TextDocument;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;

    public constructor(document: vscode.TextDocument, dumpNumbersInAreaA: boolean) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.dumpAreaBOnwards = false;
    }

    getLineCount(): number {
        return this.document.lineCount;
    }

    getLine(lineNumber: number): string {
        let line = this.document.lineAt(lineNumber).text;

        // drop fixed format line
        if (line.length > 7 && line[6] === '*') {
            return "";
        }

        // todo - this is a bit messy and should be revised
        if (this.dumpNumbersInAreaA) {
            if (line.length > 7 && line[6] === ' ') {
                line = "       " + line.substr(6);
            }
            else {
                let paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ]/g;
                if (line.match(paraPrefixRegex1)) {
                    line = "       " + line.substr(6);
                }
            }
        }

        if (this.dumpAreaBOnwards && line.length >= 73) {
            line = line.substr(0, 72);
        }

        let inlineCommentStart = line.indexOf("*>");
        if (inlineCommentStart === -1) {
            return line;
        }

        return line.substr(0, inlineCommentStart);
    }

    setDumpAreaA(flag: boolean): void {
        this.dumpNumbersInAreaA = flag;
    }

    setDumpAreaBOnwards(flag: boolean): void {
        this.dumpAreaBOnwards = flag;
    }
}

export default class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {


    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        let symbols: vscode.SymbolInformation[] = [];

        let sf = new QuickCOBOLParse(new VSCodeSourceHandler(document, true));
        let ownerUri = this.getUri();

        for (var i = 0; i < sf.tokensInOrder.length; i++) {
            let token = sf.tokensInOrder[i];

            let srange = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                new vscode.Position(token.endLine, token.endColumn));

            let container = token.parentToken !== undefined ? token.parentToken.description : "";

            switch (token.tokenType) {
                case "Class-Id":
                case "Program-Id":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Class, srange, ownerUri, container));
                    break;
                case "Copybook":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, srange, ownerUri, container));
                    break;
                case "Division":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, srange, ownerUri, container));
                    break;
                case "Paragraph":
                case "Section":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, srange, ownerUri, container));
                    break;
                case "Entry":
                case "Function-Id":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Function, srange, ownerUri, container));
                    break;
                case "Enum-id":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Enum, srange, ownerUri, container));
                    break;
                case "Interface-Id":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Interface, srange, ownerUri, container));
                    break;
                case "Valuetype-Id":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, srange, ownerUri, container));
                    break;
                case "Variable":
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Field, srange, ownerUri, container));
                    break;

            }
        }
        return symbols;
    }

    private getUri(): vscode.Uri | undefined {
        const editor = vscode.window.activeTextEditor;
        if (editor) {
            const document = editor.document;
            return document.uri;
        }

        return undefined;
    }
}