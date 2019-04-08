import * as vscode from 'vscode';
import QuickCOBOLParse, { COBOLTokenStyle } from './cobolquickparse';
import { VSCodeSourceHandler } from './VSCodeSourceHandler';

export default class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {


    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        let symbols: vscode.SymbolInformation[] = [];

        let sf = new QuickCOBOLParse(new VSCodeSourceHandler(document, true));
        let ownerUri = document.uri; //this.getUri();

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
                    symbols.push(new vscode.SymbolInformation(token.token, vscode.SymbolKind.File, container, lrange));
                    break;
                case COBOLTokenStyle.Division:
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                    break;
                case COBOLTokenStyle.Paragraph:
                case COBOLTokenStyle.Section:
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
                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Field, container, lrange));
                    break;

            }
        }
        return symbols;
    }
}