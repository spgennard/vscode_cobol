import * as vscode from "vscode";
import { COBOLTokenStyle } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSLogger } from "./vslogger";
import { outlineFlag } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { SplitTokenizer } from "./splittoken";
import { VSExternalFeatures } from "./vsexternalfeatures";


export class MFDirectivesSymbolProvider implements vscode.DocumentSymbolProvider {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        const symbols: vscode.SymbolInformation[] = [];
        const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

        if (settings.outline === outlineFlag.Off) {
            return symbols;
        }

        const ownerUri = document.uri;

        const container = "";

        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i);
            const textText = line.text.trimEnd();

            if (textText.startsWith("#")) {
                continue;
            }

            if (textText.length > 1 && textText.trim() === "@root") {
                // 
                const srange = new vscode.Range(new vscode.Position(i, 1), new vscode.Position(i, textText.length));
                const lrange = new vscode.Location(ownerUri, srange);

                symbols.push(new vscode.SymbolInformation("@root", vscode.SymbolKind.Constant, container, lrange));

                await vscode.workspace.findFiles("**/*/directives.mf").then((uris: vscode.Uri[]) => {
                    uris.forEach((uri: vscode.Uri) => {

                        const item = uri.toString().substring(ownerUri.toString().length-"directives.mf".length);
                        const srange = new vscode.Range(new vscode.Position(1, 1), new vscode.Position(1, 1));
                        const lrange = new vscode.Location(vscode.Uri.parse(uri.toString()), srange);

                        const si = new vscode.SymbolInformation(item, vscode.SymbolKind.File, item, lrange);
                        symbols.push(si);

                    });
                });

                continue;
            }

            if (textText.length > 2 && textText.startsWith("[") && textText.endsWith("]")) {
                const item = textText.substring(1, textText.length - 1);
                let lastLine = i;
                let lastLineLength = textText.length;
                const startLine = i;
                let prevLineLength = lastLineLength;

                for (i = 1 + i; i < document.lineCount; i++) {
                    const line2 = document.lineAt(i);
                    const textText2 = line2.text.trimEnd();
                    if (textText2.trimStart().length === 0) {
                        lastLine = i;
                        lastLineLength = textText2.length;
                        break;
                    }
                    if (textText2.length > 2 && textText2.startsWith("[") && textText2.endsWith("]")) {
                        lastLine = i - 1;
                        lastLineLength = prevLineLength;
                        break;
                    }

                    prevLineLength = textText2.length;
                }

                const srange = new vscode.Range(new vscode.Position(startLine, 1),
                    new vscode.Position(lastLine, lastLineLength));
                const lrange = new vscode.Location(ownerUri, srange);

                symbols.push(new vscode.SymbolInformation(item, vscode.SymbolKind.Array, container, lrange));
            }
        }

        return symbols;
    }
}

export class JCLDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        const symbols: vscode.SymbolInformation[] = [];
        const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

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
                const textLineClean = textText.substring(2);
                const lineTokens = [];
                const possibleTokens: string[] = [];
                SplitTokenizer.splitArgument(textLineClean, possibleTokens);
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

export class CobolSymbolInformationProvider implements vscode.DocumentSymbolProvider {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.SymbolInformation[]> {
        const symbols: vscode.SymbolInformation[] = [];
        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

        const outlineLevel = config.outline;

        if (outlineLevel === outlineFlag.Off) {
            return symbols;
        }

        const sf = VSCOBOLSourceScanner.getCachedObject(document,config);

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
                    const srange = new vscode.Range(new vscode.Position(token.rangeStartLine, token.rangeStartColumn),
                        new vscode.Position(token.rangeEndLine, token.rangeEndColumn));

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
                        case COBOLTokenStyle.CopyBookInOrOf:
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
                        case COBOLTokenStyle.Section:
                            if (includeSections === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Method, container, lrange));
                            break;
                        case COBOLTokenStyle.Exec:
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
                        case COBOLTokenStyle.IgnoreLS:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Null, container, lrange));
                            break;
                        case COBOLTokenStyle.Variable:
                            if (includeVars === false) {
                                break;
                            }

                            // drop fillers
                            if (token.tokenNameLower === "filler") {
                                continue;
                            }

                            if (token.extraInformation1 === "fd" || token.extraInformation1 === "sd"
                                || token.extraInformation1 === "rd" || token.extraInformation1 === "select") {
                                symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            }
                            else {
                                if (token.extraInformation1.indexOf("-GROUP") !== -1) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, container, lrange));
                                } else if (token.extraInformation1.indexOf("88") !== -1) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.EnumMember, container, lrange));
                                } else if (token.extraInformation1.indexOf("-OCCURS") !== -1) {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Array, container, lrange));
                                } else {
                                    symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Field, container, lrange));
                                }
                            }
                            break;
                        case COBOLTokenStyle.ConditionName:
                            if (includeVars === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.TypeParameter, container, lrange));
                            break;
                        case COBOLTokenStyle.Union:
                            if (includeVars === false) {
                                break;
                            }
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.Struct, container, lrange));
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

                        case COBOLTokenStyle.Region:
                            symbols.push(new vscode.SymbolInformation(token.description, vscode.SymbolKind.File, container, lrange));
                            break;
                    }
                }
            }
            catch (e) {
                VSLogger.logException("Failed " + e + " on " + JSON.stringify(token), e as Error);
            }
        }
        return symbols;
    }
}