import * as vscode from 'vscode';
import { COBOLToken, COBOLTokenStyle, splitArgument } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './vsconfiguration';
import { VSLogger } from './extension';
import { outlineFlag } from './iconfiguration';
import VSCOBOLSourceScanner from './vscobolscanner';
import { VSPreProc } from './vspreproc';

export class JCLDocumentSymbolProvider2 implements vscode.DocumentSymbolProvider {

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
                const possibleTokens: string[] = [];
                splitArgument(textLineClean, false, possibleTokens);
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

class COBOLDocumentSymbols {
    protected symbols: vscode.DocumentSymbol[];

    constructor(symbols: vscode.DocumentSymbol[]) {
        this.symbols = symbols;
    }

    protected newDocumentSymbol(token: COBOLToken, symKind: vscode.SymbolKind, topLevel: boolean): vscode.DocumentSymbol {
        const srange = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
            new vscode.Position(token.endLine, token.endColumn));

        // const sym = new vscode.DocumentSymbol(token.tokenName, token.description, symKind, srange, srange);
        const sym = new vscode.DocumentSymbol(token.description, "", symKind, srange, srange);
        if (topLevel) {
            this.symbols.push(sym);
        }
        return sym;
    }
}

class ProgramSymbols extends COBOLDocumentSymbols {
    private symCount = 0;
    private divCount = 0;

    private currentDivisionSymbols: vscode.DocumentSymbol[] | undefined;
    private currentDivisionSymbol: vscode.DocumentSymbol | undefined;
    private currentSectionSymbols: vscode.DocumentSymbol[] | undefined;

    constructor(symbols: vscode.DocumentSymbol[], topLevelToken: COBOLToken) {
        super(symbols);
        const topLevelSymbol = this.addDivision(topLevelToken);
        this.currentDivisionSymbols = topLevelSymbol.children;
    }

    public addDivision(token: COBOLToken): vscode.DocumentSymbol {
        const currentDivisionSymbol = super.newDocumentSymbol(token, this.divCount === 0 ? vscode.SymbolKind.Class : vscode.SymbolKind.Method, this.divCount === 0);
        if (this.divCount !== 0) {
            this.currentDivisionSymbols?.push(currentDivisionSymbol);
        }
        this.divCount++;
        this.symCount++;

        this.currentSectionSymbols = undefined;

        this.currentDivisionSymbol = currentDivisionSymbol;
        return currentDivisionSymbol;
    }


    public addSection(token: COBOLToken) {
        this.divCount++;
        this.symCount++;

        const sym = super.newDocumentSymbol(token, vscode.SymbolKind.Method, false);

        this.currentDivisionSymbol?.children.push(sym);
        this.currentSectionSymbols = sym.children;
    }

    public addParagraph(token: COBOLToken) {
        this.symCount++;

        const sym = super.newDocumentSymbol(token, vscode.SymbolKind.Method, false);
        if (this.currentSectionSymbols !== undefined) {
            this.currentSectionSymbols.push(sym);
        } else {
            this.currentDivisionSymbols?.push(sym);
        }
    }

    private previousSectionSymbols: vscode.DocumentSymbol[] | undefined;
    private previousDivisionSymbol: vscode.DocumentSymbol | undefined;
    private previousDivisionSymbols: vscode.DocumentSymbol[] | undefined;

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public endDeclarativesSection(token: COBOLToken) {
        if (this.previousDivisionSymbol !== undefined) {
            this.currentSectionSymbols = this.previousSectionSymbols;
            this.currentDivisionSymbol = this.previousDivisionSymbol;
            this.currentDivisionSymbols = this.previousDivisionSymbols;
            this.previousDivisionSymbol = undefined;
        }
    }

    public addDeclarativesSection(token: COBOLToken) {
        this.previousSectionSymbols = this.currentSectionSymbols;
        this.previousDivisionSymbol = this.currentDivisionSymbol;
        this.previousDivisionSymbols = this.currentDivisionSymbols;

        const sym = super.newDocumentSymbol(token, vscode.SymbolKind.Method, false);

        this.currentDivisionSymbol?.children.push(sym);
        this.currentDivisionSymbol = sym;
    }

    public addProgramId(token: COBOLToken) {
        const sym = super.newDocumentSymbol(token, vscode.SymbolKind.Class, false);
        this.currentDivisionSymbols?.push(sym);
    }

    private addRawVariable(token: COBOLToken, symbol: vscode.DocumentSymbol):void {
        if (this.currentGroup !== undefined && token.extraInformation.substr(0,2) !== '01') {
            this.currentGroup?.push(symbol);
            return;
        }

        if (this.currentSectionSymbols !== undefined) {
            this.currentSectionSymbols.push(symbol);
        } else {
            this.currentDivisionSymbols?.push(symbol);
        }
    }

    private currentGroup: vscode.DocumentSymbol[] | undefined;
    // TODO: - need stack to handle nested groups, pop level when is <= current level

    public addVariable(token: COBOLToken) {
        //     // drop fillers
        if (token.tokenNameLower === "filler") {
            return;
        }

        if (token.extraInformation === 'fd' || token.extraInformation === 'sd'
            || token.extraInformation === 'rd' || token.extraInformation === 'select') {
            const sym = super.newDocumentSymbol(token, vscode.SymbolKind.File, false);
            this.addRawVariable(token,sym);
            return;
        }

        let addSymbol: vscode.DocumentSymbol;

        const firstTwo = token.extraInformation.substr(0,2);
        if (firstTwo === '01' || firstTwo === '77' || firstTwo === '88' || firstTwo === '66') {
            this.currentGroup = undefined;
        }

        if (token.extraInformation.endsWith("-GROUP")) {
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.Struct, false);
        } else if (token.extraInformation.endsWith("88")) {
            this.currentGroup = undefined;
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.EnumMember, false);
        } else if (token.extraInformation.endsWith("-OCCURS")) {
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.Array, false);
        } else {
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.Field, false);
        }

        this.addRawVariable(token, addSymbol);
        if (token.extraInformation.endsWith("-GROUP")) {
            this.currentGroup = addSymbol.children;
        }
    }


}

export class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

    private getDocumentSymbol(token: COBOLToken, symKind: vscode.SymbolKind): vscode.DocumentSymbol {
        const srange = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
            new vscode.Position(token.endLine, token.endColumn));

        return new vscode.DocumentSymbol(token.tokenName, token.description, symKind, srange, srange);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideDocumentSymbols(document: vscode.TextDocument, canceltoken: vscode.CancellationToken): Promise<vscode.DocumentSymbol[]> {
        const topLevelSymbols: vscode.DocumentSymbol[] = [];
        const settings = VSCOBOLConfiguration.get();
        const outlineLevel = settings.outline;

        if (outlineLevel === outlineFlag.Off) {
            return topLevelSymbols;
        }

        if (await VSPreProc.areAllPreProcessorsReady(settings) === false) {
            return topLevelSymbols;
        }

        const sf = VSCOBOLSourceScanner.getCachedObject(document, settings);

        if (sf === undefined) {
            return topLevelSymbols;
        }

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

        const symbols: vscode.DocumentSymbol[] = topLevelSymbols;
        // let currentLevel: vscode.DocumentSymbol[] = symbols;

        let currentProgram: ProgramSymbols | undefined = undefined;

        for (const token of sf.tokensInOrder) {
            try {
                if (token.ignoreInOutlineView === false) {
                    switch (token.tokenType) {
                        case COBOLTokenStyle.ClassId:
                        case COBOLTokenStyle.ProgramId:
                            if (currentProgram === undefined) {
                                currentProgram = new ProgramSymbols(symbols, token);
                            } else {
                                currentProgram.addProgramId(token);
                            }
                            break;
                        // case COBOLTokenStyle.EnumId: {
                        //     const idSym = this.getDocumentSymbol(token, vscode.SymbolKind.Enum);
                        //     symbols.push(idSym);
                        //     currentLevel = idSym.children;
                        // }
                        //     break;
                        // case COBOLTokenStyle.InterfaceId: {
                        //     const idSym = this.getDocumentSymbol(token, vscode.SymbolKind.Interface);
                        //     symbols.push(idSym);
                        //     currentLevel = idSym.children;
                        // }
                        //     break;
                        // case COBOLTokenStyle.ValueTypeId: {
                        //     const idSym = this.getDocumentSymbol(token, vscode.SymbolKind.Struct);
                        //     symbols.push(idSym);
                        //     currentLevel = idSym.children;
                        // }
                        //     break;

                        // case COBOLTokenStyle.CopyBook:
                        // case COBOLTokenStyle.CopyBookInOrOf:
                        // case COBOLTokenStyle.File: {
                        //     const idSym = this.getDocumentSymbol(token, vscode.SymbolKind.File);
                        //     currentLevel.push(idSym);
                        // }
                        //     break;
                        case COBOLTokenStyle.Declaratives:
                            if (currentProgram !== undefined) {
                                currentProgram.addDeclarativesSection(token);
                            }
                            break;
                        case COBOLTokenStyle.EndDeclaratives:
                            if (currentProgram !== undefined) {
                                currentProgram.endDeclarativesSection(token);
                            }
                            break;
                        case COBOLTokenStyle.Division:
                            if (currentProgram === undefined) {
                                currentProgram = new ProgramSymbols(symbols, token);
                            } else {
                                currentProgram.addDivision(token);
                            }

                            break;
                        case COBOLTokenStyle.Paragraph:
                            if (includePara && currentProgram !== undefined) {
                                currentProgram.addParagraph(token);
                            }
                            break;
                        case COBOLTokenStyle.Section:
                            if (includeSections && currentProgram !== undefined) {
                                currentProgram.addSection(token);
                            }
                            break;
                        // case COBOLTokenStyle.EntryPoint:
                        // case COBOLTokenStyle.FunctionId:
                        //     currentLevel.push(this.getDocumentSymbol(token, vscode.SymbolKind.Function));
                        //     break;
                        case COBOLTokenStyle.Variable:
                            if (includeVars && currentProgram !== undefined) {
                                currentProgram.addVariable(token);
                            }

                        // case COBOLTokenStyle.ConditionName:
                        //     if (includeVars === false) {
                        //         break;
                        //     }
                        //     currentLevel.push(this.getDocumentSymbol(token, vscode.SymbolKind.TypeParameter));
                        //     break
                        // case COBOLTokenStyle.Union:
                        //     if (includeVars === false) {
                        //         break;
                        //     }
                        //     currentLevel.push(this.getDocumentSymbol(token, vscode.SymbolKind.Struct));
                        //     break;
                        // case COBOLTokenStyle.Constant:
                        //     if (includeVars === false) {
                        //         break;
                        //     }
                        //     currentLevel.push(this.getDocumentSymbol(token, vscode.SymbolKind.Constant));
                        //     break;
                        // case COBOLTokenStyle.MethodId:
                        //     currentLevel.push(this.getDocumentSymbol(token, vscode.SymbolKind.Method));
                        //     break;
                        // case COBOLTokenStyle.Property:
                        //     currentLevel.push(this.getDocumentSymbol(token, vscode.SymbolKind.Property));
                        //     break;

                        // case COBOLTokenStyle.Constructor:
                        //     currentLevel.push(this.getDocumentSymbol(token, vscode.SymbolKind.Constructor));
                        //     break;
                    }
                }
            }
            catch (e) {
                VSLogger.logException("Failed " + e + " on " + JSON.stringify(token), e as Error);
            }
        }
        return topLevelSymbols;
    }
}