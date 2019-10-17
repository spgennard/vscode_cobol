import * as vscode from 'vscode';
import QuickCOBOLParse, { COBOLTokenStyle, splitArgument } from './cobolquickparse';
import { VSCodeSourceHandler } from './VSCodeSourceHandler';
import { isOutlineEnabled, outlineFlag } from './extension';

export class JCLDocumentSymbolProvider implements vscode.DocumentSymbolProvider {
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        let symbols: vscode.SymbolInformation[] = [];

        if (isOutlineEnabled() === outlineFlag.Off) {
            return symbols;
        }

        let ownerUri = document.uri;

        let lastLine = document.lineCount;
        let lastLineColumn = document.lineAt(lastLine - 1).text.length;
        let container = "";

        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i);
            let textText = line.text;

            if (textText.startsWith("//*")) {
                continue;
            }

            if (textText.startsWith("//")) {
                let textLineClean = textText.substr(2);
                let lineTokens = [];
                let possibleTokens = splitArgument(textLineClean);
                for (let l = 0; l < possibleTokens.length; l++) {
                    if (possibleTokens[l] !== undefined) {
                        let possibleToken = possibleTokens[l].trim();
                        if (possibleToken.length > 0) {
                            lineTokens.push(possibleToken);
                        }
                    }
                }

                if (lineTokens.length > 0) {
                    if (lineTokens.length > 1) {

                        if (lineTokens[1].toLowerCase().indexOf("job") !== -1) {
                            let srange = new vscode.Range(new vscode.Position(i, 0),
                                new vscode.Position(lastLine, lastLineColumn));
                            let lrange = new vscode.Location(ownerUri, srange);

                            symbols.push(new vscode.SymbolInformation(lineTokens[0], vscode.SymbolKind.Field, container, lrange));
                            container = lineTokens[0];
                        }

                        if (lineTokens[1].toLowerCase().indexOf("exec") !== -1) {
                            let srange = new vscode.Range(new vscode.Position(i, 0),
                                new vscode.Position(i, lineTokens.length));
                            let lrange = new vscode.Location(ownerUri, srange);

                            symbols.push(new vscode.SymbolInformation(lineTokens[0], vscode.SymbolKind.Function, container, lrange));
                        }

                    }
                }
            }

        }

        return symbols;
    }
}

export class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        let symbols: vscode.SymbolInformation[] = [];
        let outlineLevel = isOutlineEnabled();
        if (outlineLevel === outlineFlag.Off) {
            return symbols;
        }

        let sf = QuickCOBOLParse.getCachedObject(document, document.fileName);

        if (sf === undefined) {
            return symbols;
        }

        let ownerUri = document.uri;

        let includePara: boolean = true;
        let includeVars: boolean = true;
        let includeSections: boolean = true;

        if (outlineLevel === outlineFlag.Partial) {
            includePara = false;
        }

        if (outlineLevel === outlineFlag.Skeleton) {
            includeVars = false;
            includeSections = false;
            includePara = false;
        }

        for (var i = 0; i < sf.tokensInOrder.length; i++) {
            let token = sf.tokensInOrder[i];

            let srange = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                new vscode.Position(token.endLine, token.endColumn));

            let lrange = new vscode.Location(ownerUri, srange);

            let container = token.parentToken !== undefined ? token.parentToken.description : "";

            switch (token.tokenType) {
                case COBOLTokenStyle.ClassId:
                case COBOLTokenStyle.ProgramId:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Class, container, lrange));
                    break;
                case COBOLTokenStyle.CopyBook:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                    break;
                case COBOLTokenStyle.Division:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                    break;
                case COBOLTokenStyle.Paragraph:
                    if (includePara === false) {
                        break;
                    }
                case COBOLTokenStyle.Section:
                    if (includeSections === false) {
                        break;
                    }
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                    break;
                case COBOLTokenStyle.EntryPoint:
                case COBOLTokenStyle.FunctionId:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Function, container, lrange));
                    break;
                case COBOLTokenStyle.EnumId:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Enum, container, lrange));
                    break;
                case COBOLTokenStyle.InterfaceId:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Interface, container, lrange));
                    break;
                case COBOLTokenStyle.ValueTypeId:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, container, lrange));
                    break;
                case COBOLTokenStyle.Variable:
                    if (includeVars === false) {
                        break;
                    }
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Field, container, lrange));
                    break;
                case COBOLTokenStyle.Constant:
                    if (includeVars === false) {
                        break;
                    }
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Constant, container, lrange));
                    break;
                case COBOLTokenStyle.MethodId:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                    break;
                case COBOLTokenStyle.Property:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Property, container, lrange));
                    break;
                case COBOLTokenStyle.Constructor:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Constructor, container, lrange));
                    break;
            }
        }
        return symbols;
    }
}