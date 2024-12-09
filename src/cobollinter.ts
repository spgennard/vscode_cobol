
import * as vscode from "vscode";
import { COBOLSourceScanner, SharedSourceReferences } from "./cobolsourcescanner";
import { CodeActionProvider, CodeAction } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { CobolLinterProviderSymbols } from "./externalfeatures";
import { TextLanguage, VSExtensionUtils } from "./vsextutis";
import { VSCOBOLUtils } from "./vscobolutils";
import { VSCOBOLFileUtils } from "./vsfileutils";
import path from "path";
import { VSCOBOLConfiguration, VSCOBOLEditorConfiguration } from "./vsconfiguration";
import { VSExternalFeatures } from "./vsexternalfeatures";

export class CobolLinterActionFixer implements CodeActionProvider {

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    provideCodeActions(document: vscode.TextDocument, range: vscode.Range | vscode.Selection, context: vscode.CodeActionContext, token: vscode.CancellationToken): vscode.ProviderResult<(vscode.Command | vscode.CodeAction)[]> {
        const codeActions: CodeAction[] = [];
        for (const diagnostic of context.diagnostics) {
            if (diagnostic.code === undefined) {
                continue;
            }

            const codeMsg = diagnostic.code.toString();

            // is it ours?
            if (codeMsg.startsWith(CobolLinterProviderSymbols.NotReferencedMarker_internal) === true) {
                const startOfline = document.offsetAt(new vscode.Position(diagnostic.range.start.line, 0));
                const insertCode = diagnostic.code.toString().replace(CobolLinterProviderSymbols.NotReferencedMarker_internal, CobolLinterProviderSymbols.NotReferencedMarker_external);
                codeActions.push({
                    title: `Add COBOL lint ignore comment for '${diagnostic.message}'`,
                    diagnostics: [diagnostic],
                    command: {
                        title: "Add COBOL lint comment to ignore the warning",
                        command: "cobolplugin.insertIgnoreCommentLine",
                        arguments: [document.uri, startOfline, insertCode],
                    },
                    kind: vscode.CodeActionKind.QuickFix
                });
            }

            // is it ours?
            if (codeMsg.startsWith(CobolLinterProviderSymbols.CopyBookNotFound) === true) {
                const startOfline = document.offsetAt(new vscode.Position(diagnostic.range.start.line, 0));
                const insertCode = diagnostic.code.toString().replace(CobolLinterProviderSymbols.CopyBookNotFound, "").trim();
                codeActions.push({
                    title: `Find Copybook ${insertCode}`,
                    diagnostics: [diagnostic],
                    command: {
                        title: "Find a copybook within a workspace",
                        command: "cobolplugin.findCopyBookDirectory",
                        arguments: [document.uri, startOfline, insertCode],
                    },
                    kind: vscode.CodeActionKind.QuickFix
                });
            }

            // is it ours?
            if (codeMsg.startsWith(CobolLinterProviderSymbols.PortMessage) === true) {
                let portChangeLine = diagnostic.code.toString().replace(CobolLinterProviderSymbols.PortMessage, "").trim();
                if (portChangeLine.startsWith("$")) {
                    portChangeLine = "      " + portChangeLine;
                } else {
                    portChangeLine = "       " + portChangeLine;
                }

                const trimChangedLine = portChangeLine.trim();

                if (trimChangedLine.length !== 0) {
                    codeActions.push({
                        title: `Change line to ${trimChangedLine}`,
                        diagnostics: [diagnostic],
                        command: {
                            title: "Change line",
                            command: "cobolplugin.portCodeCommandLine",
                            arguments: [document.uri, diagnostic.range.start.line, portChangeLine],
                        },
                        kind: vscode.CodeActionKind.QuickFix
                    });
                }
            }
        }
        return codeActions;
    }

    public async insertIgnoreCommentLine(docUri: vscode.Uri, offset: number, code: string): Promise<void> {
        await vscode.window.showTextDocument(docUri);
        const w = vscode.window.activeTextEditor;

        if (w !== undefined && code !== undefined) {
            const pos = w.document.positionAt(offset);
            await w.edit(edit => {
                edit.insert(pos, "      *> cobol-lint " + code + "\n");
            });
        }
    }

    public async portCodeCommandLine(docUri: vscode.Uri, lineNumber: number, code: string): Promise<void> {
        await vscode.window.showTextDocument(docUri);
        const w = vscode.window.activeTextEditor;

        if (w !== undefined && code !== undefined) {

            await w.edit(edit => {
                const line = w.document.lineAt(lineNumber);
                edit.replace(line.range, code);
            });
        }
    }

    private knownExternalCopybooks = new Map<string, string>(
        [
            ["cblproto.cpy", "$COBCPY"],
            ["cbltypes.cpy", "$COBCPY"],
            ["windows.cpy", "$COBCPY"],
            ["mfunit.cpy", "$COBCPY"],
            ["mfunit_prototypes.cpy", "$COBCPY"]
        ],
    );

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async findCopyBookDirectory(settings: ICOBOLSettings, docUri: vscode.Uri, linenum: number, copybook: string) {
        let files: vscode.Uri[] = [];
        const fileSearchDirectory = settings.config_copybookdirs;
        let update = false;
        let update4found = false;

        if (process && process.env) {
            if (fileSearchDirectory.indexOf("$COBCPY") === -1) {
                const COBCPYenv = process.env["COBCPY"]
                if (COBCPYenv !== undefined && COBCPYenv.length > 0) {
                    fileSearchDirectory.push("$COBCPY")
                    update = true
                } else {
                    const knownDir = this.knownExternalCopybooks.get(copybook);
                    if (knownDir !== undefined) {
                        fileSearchDirectory.push(knownDir);
                        update = true
                    }
                }
            }
        }

        files = await vscode.workspace.findFiles(`**/${copybook}`);
        if (files.length === 0) {
            const globPattern = VSCOBOLUtils.getCopyBookGlobPatternForPartialName(settings, copybook);
            files = await vscode.workspace.findFiles(globPattern);
        }

        for (const uri of files) {
            const workspaceFile = VSCOBOLFileUtils.getShortWorkspaceFilename(uri.scheme, uri.fsPath, settings);
            if (workspaceFile) {
                let newDirname = path.dirname(workspaceFile);
                if (copybook.indexOf("/") !== -1) {
                    const upPathFound = copybook.split("/").length - 1;
                    for (let c = 0; c < upPathFound; c++) {
                        newDirname = path.dirname(newDirname);
                    }
                }
                if (fileSearchDirectory.indexOf(newDirname) === -1) {
                    fileSearchDirectory.push(newDirname);
                    update = update4found = true;
                }
            }
        }

        if (update) {
            const editorConfig = VSCOBOLEditorConfiguration.getEditorConfig();
            await editorConfig.update("copybookdirs", this.uniqByFilter(fileSearchDirectory));
        }

        if (update4found === false) {
            await vscode.window.showInformationMessage(`Unable to locate ${copybook} in workspace`);
        }

        if (update4found) {
            await vscode.commands.executeCommand("workbench.action.reloadWindow")
        }
    }

    private uniqByFilter<T>(array: T[]) {
        return array.filter((value, index) => array.indexOf(value) === index);
    }
}

export class CobolLinterProvider {
    private collection: vscode.DiagnosticCollection;
    private current: COBOLSourceScanner | undefined;
    private currentVersion?: number;
    private sourceRefs?: SharedSourceReferences;

    constructor(collection: vscode.DiagnosticCollection) {
        this.collection = collection;
    }

    public async updateLinter(document: vscode.TextDocument): Promise<void> {
        const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
        const linterSev: vscode.DiagnosticSeverity = settings.linter_mark_as_information ? vscode.DiagnosticSeverity.Information : vscode.DiagnosticSeverity.Hint;
    
        /* drop out early */
        if (settings.linter === false) {
            return;
        }

        /* drop out if not COBOL */
        if (VSExtensionUtils.isSupportedLanguage(document) !== TextLanguage.COBOL) {
            return;
        }

        /* has it changed? */
        if (this.setupCOBOLScannner(document, settings) === false) {
            return;
        }

        const diagRefs = new Map<string, vscode.Diagnostic[]>();
        this.collection.clear();

        if (this.current) {
            if (this.sourceRefs !== undefined && !this.current.sourceIsCopybook) {
                const qp: COBOLSourceScanner = this.current;


                // handle sections & paragraphs
                this.processScannedDocumentForUnusedSymbols(settings, qp, diagRefs, qp.configHandler.linter_unused_paragraphs, qp.configHandler.linter_unused_sections, linterSev);

                if (qp.configHandler.linter_house_standards_rules) {
                    this.processParsedDocumentForStandards(qp, diagRefs, linterSev);
                }
            }

            if (settings.linter_ignore_missing_copybook === false && this.current.diagMissingFileWarnings.size !== 0) {
                const qp: COBOLSourceScanner = this.current;
                for (const [msg, fileSymbol] of qp.diagMissingFileWarnings) {
                    if (fileSymbol.filename !== undefined && fileSymbol.linenum !== undefined) {
                        const r = new vscode.Range(new vscode.Position(fileSymbol.linenum, 0), new vscode.Position(fileSymbol.linenum, 0));
                        const diagMessage = new vscode.Diagnostic(r, msg, linterSev);
                        diagMessage.tags = [vscode.DiagnosticTag.Unnecessary];
                        diagMessage.code = CobolLinterProviderSymbols.CopyBookNotFound + fileSymbol.messageOrMissingFile;
                        if (diagRefs.has(fileSymbol.filename)) {
                            const diags = diagRefs.get(fileSymbol.filename);
                            if (diags !== undefined) {
                                diags.push(diagMessage);
                            }
                        } else {
                            const arr: vscode.Diagnostic[] = [];
                            arr.push(diagMessage);
                            diagRefs.set(fileSymbol.filename, arr);
                        }

                    }
                }
            }

            if (this.current.portWarnings.length !== 0) {
                const qp: COBOLSourceScanner = this.current;
                for (const fileSymbol of qp.portWarnings) {
                    if (fileSymbol.filename !== undefined && fileSymbol.linenum !== undefined) {
                        const r = new vscode.Range(new vscode.Position(fileSymbol.linenum, 0), new vscode.Position(fileSymbol.linenum, 0));
                        const diagMessage = new vscode.Diagnostic(r, fileSymbol.message, linterSev);
                        diagMessage.tags = [vscode.DiagnosticTag.Unnecessary];
                        diagMessage.code = CobolLinterProviderSymbols.PortMessage + fileSymbol.replaceLine;
                        if (diagRefs.has(fileSymbol.filename)) {
                            const diags = diagRefs.get(fileSymbol.filename);
                            if (diags !== undefined) {
                                diags.push(diagMessage);
                            }
                        } else {
                            const arr: vscode.Diagnostic[] = [];
                            arr.push(diagMessage);
                            diagRefs.set(fileSymbol.filename, arr);
                        }
                    }
                }
            }

            if (this.current.generalWarnings.length !== 0) {
                const qp: COBOLSourceScanner = this.current;
                for (const fileSymbol of qp.generalWarnings) {
                    if (fileSymbol.filename !== undefined && fileSymbol.linenum !== undefined) {
                        const r = new vscode.Range(new vscode.Position(fileSymbol.linenum, 0), new vscode.Position(fileSymbol.linenum, 0));
                        const diagMessage = new vscode.Diagnostic(r, fileSymbol.messageOrMissingFile, linterSev);
                        diagMessage.tags = [vscode.DiagnosticTag.Unnecessary];
                        diagMessage.code = CobolLinterProviderSymbols.GeneralMessage + fileSymbol.messageOrMissingFile;
                        if (diagRefs.has(fileSymbol.filename)) {
                            const diags = diagRefs.get(fileSymbol.filename);
                            if (diags !== undefined) {
                                diags.push(diagMessage);
                            }
                        } else {
                            const arr: vscode.Diagnostic[] = [];
                            arr.push(diagMessage);
                            diagRefs.set(fileSymbol.filename, arr);
                        }
                    }
                }
            }
        }

        for (const [f, value] of diagRefs) {
            const u = vscode.Uri.file(f);
            this.collection.set(u, value);
        }
    }

    private processParsedDocumentForStandards(qp: COBOLSourceScanner, diagRefs: Map<string, vscode.Diagnostic[]>, linterSev: vscode.DiagnosticSeverity) {

        if (this.sourceRefs === undefined) {
            return;
        }

        const standards: string[] = qp.configHandler.linter_house_standards_rules;
        const standardsMap = new Map<string, string>();
        const ruleRegexMap = new Map<string, RegExp>();

        for (const standard of standards) {
            const sectionStandard = standard.split("=", 2);
            standardsMap.set(sectionStandard[0].toLowerCase(), sectionStandard[1]);
        }

        for (const [key, tokens] of qp.constantsOrVariables) {
            for (const variable of tokens) {
                const token = variable.token;
                if (token === undefined || token.inSection === undefined) {
                    continue;
                }

                if (token.tokenNameLower === "filler") {
                    continue;
                }

                const rule = standardsMap.get(token.inSection.tokenNameLower);
                if (rule !== undefined) {
                    let regexForRule = ruleRegexMap.get(token.inSection.tokenNameLower);
                    if (regexForRule === undefined) {
                        regexForRule = this.makeRegex(rule);
                        if (regexForRule === undefined) {
                            continue;
                        }
                        ruleRegexMap.set(token.inSection.tokenNameLower, regexForRule);
                    }
                    
                    if (regexForRule.test(token.tokenName) === false) {
                        const r = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                            new vscode.Position(token.startLine, token.startColumn + token.tokenName.length));

                        const d = new vscode.Diagnostic(r, key + " breaks house standards rule for " + token.inSection.tokenNameLower + " section", linterSev);
                        d.tags = [vscode.DiagnosticTag.Unnecessary];

                        if (diagRefs.has(token.filename)) {
                            const arr = diagRefs.get(token.filename);
                            if (arr !== undefined) {
                                arr.push(d);
                            }
                        } else {
                            const arr: vscode.Diagnostic[] = [];
                            arr.push(d);
                            diagRefs.set(token.filename, arr);
                        }
                    }
                }
            }
        }
    }

    private processScannedDocumentForUnusedSymbols(settings: ICOBOLSettings, qp: COBOLSourceScanner, diagRefs: Map<string, vscode.Diagnostic[]>, processParas: boolean, processSections: boolean, linterSev: vscode.DiagnosticSeverity) {

        if (this.sourceRefs === undefined) {
            return;
        }

        const sharedSourceReferences: SharedSourceReferences = this.sourceRefs;

        if (processParas) {
            for (const [key, token] of qp.paragraphs) {
                const workLower = key.toLowerCase();
                if (sharedSourceReferences.ignoreUnusedSymbol.has(workLower)) {
                    continue;
                }

                const tupRefs = qp.sourceReferences.getReferenceInformation4targetRefs(workLower, qp.sourceFileId, token.startLine, token.startColumn);
                const refCount = tupRefs[1];
                if (refCount === 0) {
                    const r = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                        new vscode.Position(token.startLine, token.startColumn + token.tokenName.length));
                    const d = new vscode.Diagnostic(r, key + " paragraph is not referenced", linterSev);
                    d.tags = [vscode.DiagnosticTag.Unnecessary];
                    d.code = CobolLinterProviderSymbols.NotReferencedMarker_internal + " " + key;

                    if (diagRefs.has(token.filename)) {
                        const arr = diagRefs.get(token.filename);
                        if (arr !== undefined) {
                            arr.push(d);
                        }
                    } else {
                        const arr: vscode.Diagnostic[] = [];
                        arr.push(d);
                        diagRefs.set(token.filename, arr);
                    }
                }
            }
        }

        if (processSections) {
            for (const [key, token] of qp.sections) {
                const workLower = key.toLowerCase();

                if (sharedSourceReferences.ignoreUnusedSymbol.has(workLower)) {
                    continue;
                }

                if (token.inProcedureDivision) {
                    const tupRefs = qp.sourceReferences.getReferenceInformation4targetRefs(workLower, qp.sourceFileId, token.startLine, token.startColumn);
                    const refCount = tupRefs[1];
                    
                    if (refCount === 0) {
                        let ignore = false;

                        if (settings.linter_ignore_section_before_entry) {
                            const nextLine = qp.sourceHandler.getLine(1 + token.startLine, false);
                            if (nextLine?.toLowerCase().indexOf("entry") !== -1) {
                                ignore = true;
                            }
                        }

                        if (!ignore) {
                            const r = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                                new vscode.Position(token.startLine, token.startColumn + token.tokenName.length));
                            const d = new vscode.Diagnostic(r, key + " section is not referenced", linterSev);
                            d.code = CobolLinterProviderSymbols.NotReferencedMarker_internal + " " + key;
                            d.tags = [vscode.DiagnosticTag.Unnecessary];

                            if (diagRefs.has(token.filename)) {
                                const arr = diagRefs.get(token.filename);
                                if (arr !== undefined) {
                                    arr.push(d);
                                }
                            } else {
                                const arr: vscode.Diagnostic[] = [];
                                arr.push(d);
                                diagRefs.set(token.filename, arr);
                            }
                        }
                    }
                }
            }
        }
    }

    private setupCOBOLScannner(document: vscode.TextDocument, settings: ICOBOLSettings): boolean {
        if (this.current !== undefined && this.current.filename !== document.fileName) {
            this.current = undefined;
        }

        // cache current document, interactive search to be faster
        if (this.current === undefined || this.currentVersion !== document.version) {
            this.current = VSCOBOLSourceScanner.getCachedObject(document, settings);
            if (this.current !== undefined) {
                this.sourceRefs = this.current.sourceReferences;
            }
            this.currentVersion = document.version;
            return true;
        }

        return false;
    }

    private makeRegex(partialRegEx: string): RegExp | undefined {
        try {
            return new RegExp("^" + partialRegEx + "$", "i");
        }
        catch {
            return undefined;
        }
    }

}