import * as vscode from 'vscode';
import { COBOLTokenStyle, splitArgument } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './configuration';
import { outlineFlag } from './iconfiguration';
import VSCOBOLSourceScanner from './vscobolscanner';

export class JCLDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        const symbols: vscode.SymbolInformation[] = [];
        const settings = VSCOBOLConfiguration.get();

        if (settings.outline === outlineFlag.Off) {
            return symbols;
        }

        const ownerUri = document.uri;

        const lastLine = document.lineCount;
        const lastLineColumn = document.lineAt(lastLine - 1).text.length;
        let container = "";

        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i);
            const textText = line.text;

            if (textText.startsWith("//*")) {
                continue;
            }

            if (textText.startsWith("//")) {
                const textLineClean = textText.substr(2);
                const lineTokens = [];
                const possibleTokens = splitArgument(textLineClean, false);
                for (let l = 0; l < possibleTokens.length; l++) {
                    if (possibleTokens[l] !== undefined) {
                        const possibleToken = possibleTokens[l].trim();
                        if (possibleToken.length > 0) {
                            lineTokens.push(possibleToken);
                        }
                    }
                }

                if (lineTokens.length > 0) {
                    if (lineTokens.length > 1) {

                        if (lineTokens[1].toLowerCase().indexOf("job") !== -1) {
                            const srange = new vscode.Range(new vscode.Position(i, 0),
                                new vscode.Position(lastLine, lastLineColumn));
                            const lrange = new vscode.Location(ownerUri, srange);

                            symbols.push(new vscode.SymbolInformation(lineTokens[0], vscode.SymbolKind.Field, container, lrange));
                            container = lineTokens[0];
                        }

                        if (lineTokens[1].toLowerCase().indexOf("exec") !== -1) {
                            const srange = new vscode.Range(new vscode.Position(i, 0),
                                new vscode.Position(i, lineTokens.length));
                            const lrange = new vscode.Location(ownerUri, srange);

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

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        const symbols: vscode.SymbolInformation[] = [];
        const settings = VSCOBOLConfiguration.get();
        const outlineLevel = settings.outline;

        if (outlineLevel === outlineFlag.Off) {
            return symbols;
        }

        const sf = VSCOBOLSourceScanner.getCachedObject(document, settings);

        if (sf === undefined) {
            return symbols;
        }

        const ownerUri = document.uri;

        let includePara = true;
        let includeVars = true;
        let includeSections = true;

        if (outlineLevel === outlineFlag.Partial) {
            includePara = false;
        }

        if (outlineLevel === outlineFlag.Skeleton) {
            includeVars = false;
            includeSections = false;
            includePara = false;
        }

        for (const token of sf.tokensInOrder) {
            try {
                if (token.ignoreInOutlineView === false) {
                    const srange = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                        new vscode.Position(token.endLine, token.endColumn));

                    const lrange = new vscode.Location(ownerUri, srange);

                    const container = token.parentToken !== undefined ? token.parentToken.description : "";
                    switch (token.tokenType) {
                        case COBOLTokenStyle.ClassId:
                        case COBOLTokenStyle.ProgramId:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Class, container, lrange));
                            break;
                        case COBOLTokenStyle.CopyBook:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            break;
                        case COBOLTokenStyle.CopyBookIn:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            break;
                        case COBOLTokenStyle.File:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            break;
                        case COBOLTokenStyle.Declaratives:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Division:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Paragraph:
                            if (includePara === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.DeclarativesSection:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
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

                            // drop fillers
                            if (token.tokenNameLower === "filler") {
                                continue;
                            }

                            if (token.extraInformation === 'fd' || token.extraInformation === 'sd'
                                || token.extraInformation === 'rd' || token.extraInformation === 'select') {
                                symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            }
                            else {
                                if (token.extraInformation.endsWith("-GROUP")) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, container, lrange));
                                } else if (token.extraInformation.endsWith("88")) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.EnumMember, container, lrange));
                                } else {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Field, container, lrange));
                                }
                            }
                            break;
                        case COBOLTokenStyle.ConditionName :
                            if (includeVars === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.TypeParameter, container, lrange));
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
            }
            catch (e) {
                console.log("Failed " + e + " on " + JSON.stringify(token));
            }
        }
        return symbols;
    }
}