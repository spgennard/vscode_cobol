import * as vscode from "vscode";
import { COBOLToken, COBOLTokenStyle } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSLogger } from "./vslogger";
import { outlineFlag } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSPreProc } from "./vspreproc";

class SimpleStack<T> {
    _store: T[] = [];
    push(val: T) {
        this._store.push(val);
    }

    pop(): T | undefined {
        return this._store.pop();
    }

    peek(): T | undefined {
        return this._store[this.size() - 1];
    }

    size(): number {
        return this._store.length;
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

        this.currentSectionSymbols = undefined;

        this.currentDivisionSymbol = currentDivisionSymbol;
        return currentDivisionSymbol;
    }


    public addSection(token: COBOLToken) {
        this.divCount++;

        const sym = super.newDocumentSymbol(token, vscode.SymbolKind.Method, false);

        this.currentDivisionSymbol?.children.push(sym);
        this.currentSectionSymbols = sym.children;
    }

    public addParagraph(token: COBOLToken) {

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

    private addRawVariable(token: COBOLToken, symbol: vscode.DocumentSymbol): void {
        if (this.currentGroup !== undefined && token.extraInformation1.substr(0, 2) !== "01") {
            VSLogger.logMessage(` addRawVariable: ${this.currentGroup?.name} -> ${token.tokenName} -> ${token.extraInformation1}`);
            this.currentGroup?.children.push(symbol);
            return;
        }

        if (this.currentSectionSymbols !== undefined) {
            VSLogger.logMessage(` addRawVariable: section -> ${token.tokenName} -> ${token.extraInformation1}`);
            this.currentSectionSymbols.push(symbol);
        } else {
            VSLogger.logMessage(` addRawVariable: division -> ${token.tokenName} -> ${token.extraInformation1}`);
            this.currentDivisionSymbols?.push(symbol);

        }
    }

    private addGroup(firstTwo: number, token: COBOLToken, addSymbol: vscode.DocumentSymbol): void {
        if (this.currentGroupLevel === 0) {
            this.addRawVariable(token, addSymbol);
            this.currentGroup = addSymbol;
            this.currentGroupLevel = firstTwo;
            return;
        }

        if (firstTwo > this.currentGroupLevel) {
            VSLogger.logMessage(`Addgroup: push level, save: ${this.currentGroup?.name}@${this.currentGroupLevel}`);
            this.currentGroups.push(this.currentGroup);
            this.currentGroupsLevels.push(this.currentGroupLevel)
            this.currentGroup?.children.push(addSymbol);
            this.currentGroup = addSymbol;
            this.currentGroupLevel = firstTwo;
            VSLogger.logMessage(`Addgroup: push level, new: ${this.currentGroup?.name}@${this.currentGroupLevel}\n`);
            return;
        }

        if(firstTwo < this.currentGroupLevel) {
            VSLogger.logMessage(`Addgroup: pop level (top), ${this.currentGroup?.name}@${this.currentGroupLevel}`);
            const possibleCurrentGroup = this.currentGroups.pop();
            if (possibleCurrentGroup !== undefined) {
                this.currentGroup = possibleCurrentGroup;
            }
            const possibleGroupLevel = this.currentGroupsLevels.pop();
            if (possibleGroupLevel !== undefined) {
                this.currentGroupLevel = possibleGroupLevel;
            }
            this.currentGroup?.children.push(addSymbol);
            VSLogger.logMessage(`Addgroup: pop level (bot), ${this.currentGroup?.name}@${this.currentGroupLevel}\n`);
            return;
        }

        if (firstTwo === this.currentGroupLevel) {
            VSLogger.logMessage(`Addgroup: same level, ${this.currentGroup?.name}@${this.currentGroupLevel}`);
            const parent = this.currentGroups.peek();
            if (parent !== undefined) {
                VSLogger.logMessage(` Addgroup: parent name, ${parent.name}`);
                parent.children.push(addSymbol);
                VSLogger.logMessage("\n");
                // VSLogger.logMessage(`Addgroup: same level, ${this.currentGroup.name}@${this.currentGroupLevel}\n`);
            }
            return;
        }

        VSLogger.logMessage(`Addgroup: FAIL: ${firstTwo} -> ${token.tokenName} -> ${token.extraInformation1}`);
        // this.addRawVariable(token, addSymbol);
        // this.currentGroupLevel = firstTwo;
        // VSLogger.logMessage(`Addgroup: end: ${firstTwo} -> ${token.tokenName} -> ${token.extraInformation1}\n`);
    }
    private currentGroup: vscode.DocumentSymbol | undefined = undefined;

    // TODO: - need stack to handle nested groups, pop level when is <= current level

    private currentGroupLevel = 0;
    private currentGroups = new SimpleStack<vscode.DocumentSymbol | undefined>();
    private currentGroupsLevels = new SimpleStack<number>();

    public addVariable(token: COBOLToken) {
        //     // drop fillers
        if (token.tokenNameLower === "filler") {
            return;
        }

        if (token.extraInformation1 === "fd" || token.extraInformation1 === "sd"
            || token.extraInformation1 === "rd" || token.extraInformation1 === "select") {
            const sym = super.newDocumentSymbol(token, vscode.SymbolKind.File, false);
            this.addRawVariable(token, sym);
            return;
        }

        let addSymbol: vscode.DocumentSymbol;

        const firstTwo = Number(token.extraInformation1.substr(0, 2).replace(/^0+/, ""));
        if (firstTwo === 1 || firstTwo === 77 || firstTwo === 88 || firstTwo === 66) {
            this.currentGroup = undefined;
        }

        if (token.extraInformation1.endsWith("-GROUP")) {
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.Struct, false);
        } else if (token.extraInformation1.endsWith("88")) {
            this.currentGroup = undefined;
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.EnumMember, false);
        } else if (token.extraInformation1.endsWith("-OCCURS")) {
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.Array, false);
        } else {
            addSymbol = super.newDocumentSymbol(token, vscode.SymbolKind.Field, false);
        }

        VSLogger.logMessage(`${firstTwo} -> ${token.tokenName} -> ${token.extraInformation1}`);
        if (token.extraInformation1.indexOf("GROUP") !== -1) {
            this.addGroup(firstTwo, token, addSymbol);
        }
        //  else {
        //     this.addRawVariable(token, addSymbol);
        // }
    }
}

export class CobolDocumentSymbolProvider implements vscode.DocumentSymbolProvider {

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