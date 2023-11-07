
import * as vscode from "vscode";
import { COBOLSourceScanner, SharedSourceReferences } from "./cobolsourcescanner";
import { CodeActionProvider, CodeAction } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { CobolLinterProviderSymbols } from "./externalfeatures";
import { TextLanguage, VSExtensionUtils } from "./vsextutis";
import { COBOLUtils } from "./cobolutils";
import { VSLogger } from "./vslogger";
import { VSCOBOLFileUtils } from "./vsfileutils";
import path from "path";
import { ExtensionDefaults } from "./extensionDefaults";

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
                    title: `Find Copybook :${diagnostic.message}`,
                    diagnostics: [diagnostic],
                    command: {
                        title: "Find a copybook within a workspace",
                        command: "cobolplugin.findCopyBookDirectory",
                        arguments: [document.uri, startOfline,insertCode],
                    },
                    kind: vscode.CodeActionKind.QuickFix
                });
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

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async findCopyBookDirectory(settings: ICOBOLSettings, docUri: vscode.Uri,  linenum: number, copybook: string) {
        let files: vscode.Uri[] = [];
        files = await vscode.workspace.findFiles(`**/${copybook}`);
        if (files.length === 0) {
            const globPattern = COBOLUtils.getCopyBookGlobPatternForPartialName(settings, copybook);
            files = await vscode.workspace.findFiles(globPattern);
        }
        
        if (files.length === 0) {
            vscode.window.showInformationMessage(`Unable to locate ${copybook} in workspace`);
            return;
        }

        for (const uri of files) {
            const fullPath = uri.fsPath;
            const workspaceFile = VSCOBOLFileUtils.getShortWorkspaceFilename(uri.scheme, uri.fsPath);
            if (workspaceFile) {
                let newDirname = path.dirname(workspaceFile);
                if (copybook.indexOf("/") !== -1) {
                    const upPathFound = copybook.split("/").length-1;
                    for (let c = 0; c < upPathFound; c++) {
                        newDirname = path.dirname(newDirname);
                    }
                }
                const fileSearchDirectory = settings.copybookdirs;
                fileSearchDirectory.push(newDirname);
                const editorConfig = vscode.workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
                editorConfig.update("copybookdirs", fileSearchDirectory);
                await vscode.commands.executeCommand("workbench.action.reloadWindow")
                VSLogger.logMessage("Copybook dir changes:");
                VSLogger.logMessage(` Found file        : ${fullPath.toString()}`);
                VSLogger.logMessage(` Found short file  : ${workspaceFile}`);
                VSLogger.logMessage(` New copybook path : ${newDirname}`);
                return
            }
        }
    }
}

export class CobolLinterProvider {
    private settings: ICOBOLSettings;

    private collection: vscode.DiagnosticCollection;
    private linterSev: vscode.DiagnosticSeverity;

    private current: COBOLSourceScanner | undefined;
    private currentVersion?: number;
    private sourceRefs?: SharedSourceReferences;

    constructor(collection: vscode.DiagnosticCollection, settings: ICOBOLSettings) {
        this.collection = collection;
        this.settings = settings;
        this.linterSev = settings.linter_mark_as_information ? vscode.DiagnosticSeverity.Information : vscode.DiagnosticSeverity.Hint;
    }

    public async updateLinter(document: vscode.TextDocument): Promise<void> {
        /* drop out if not COBOL */
        if (VSExtensionUtils.isSupportedLanguage(document) !== TextLanguage.COBOL) {
            return;
        }

        /* has it changed? */
        if (this.setupCOBOLScannner(document) === false) {
            return;
        }

        const diagRefs = new Map<string, vscode.Diagnostic[]>();
        this.collection.clear();

        if (this.current) {
            if (this.sourceRefs !== undefined && this.settings.linter === false && !this.current.sourceIsCopybook) {
                const qp: COBOLSourceScanner = this.current;

                this.linterSev = this.settings.linter_mark_as_information ? vscode.DiagnosticSeverity.Information : vscode.DiagnosticSeverity.Hint;

                // handle sections & paragraphs
                this.processScannedDocumentForUnusedSymbols(qp, diagRefs, qp.configHandler.linter_unused_paragraphs, qp.configHandler.linter_unused_sections);

                if (qp.configHandler.linter_house_standards_rules) {
                    this.processParsedDocumentForStandards(qp, diagRefs);
                }
            }

            if (this.settings.linter_ignore_missing_copybook === false && this.current.diagMissingFileWarnings.size !== 0) {
                const qp: COBOLSourceScanner = this.current;
                for (const [msg, fileSymbol] of qp.diagMissingFileWarnings) {
                    if (fileSymbol.filename !== undefined && fileSymbol.lnum !== undefined) {
                        const r = new vscode.Range(new vscode.Position(fileSymbol.lnum, 0), new vscode.Position(fileSymbol.lnum, 0));
                        const diagMessage = new vscode.Diagnostic(r, msg, vscode.DiagnosticSeverity.Information);
                        diagMessage.tags = [vscode.DiagnosticTag.Unnecessary];
                        diagMessage.code = CobolLinterProviderSymbols.CopyBookNotFound+" "+fileSymbol.missingFile;
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

    private processParsedDocumentForStandards(qp: COBOLSourceScanner, diagRefs: Map<string, vscode.Diagnostic[]>) {

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

                        const d = new vscode.Diagnostic(r, key + " breaks house standards rule for " + token.inSection.tokenNameLower + " section", this.linterSev);
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

    private processScannedDocumentForUnusedSymbols(qp: COBOLSourceScanner, diagRefs: Map<string, vscode.Diagnostic[]>, processParas: boolean, processSections: boolean) {

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

                const refs = sharedSourceReferences.targetReferences.get(workLower);
                if (refs !== undefined && refs.length === 1) {
                    const r = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                        new vscode.Position(token.startLine, token.startColumn + token.tokenName.length));
                    const d = new vscode.Diagnostic(r, key + " paragraph is not referenced", this.linterSev);
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
                    const refs = sharedSourceReferences.targetReferences.get(workLower);
                    if (refs !== undefined && refs.length === 1) {
                        let ignore = false;

                        if (this.settings.linter_ignore_section_before_entry) {
                            const nextLine = qp.sourceHandler.getLine(1 + token.startLine, false);
                            if (nextLine?.toLowerCase().indexOf("entry") !== -1) {
                                ignore = true;
                            }
                        }

                        if (!ignore) {
                            const r = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                                new vscode.Position(token.startLine, token.startColumn + token.tokenName.length));
                            const d = new vscode.Diagnostic(r, key + " section is not referenced", this.linterSev);
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

    private setupCOBOLScannner(document: vscode.TextDocument): boolean {
        if (this.current !== undefined && this.current.filename !== document.fileName) {
            this.current = undefined;
        }

        // cache current document, interactive search to be faster
        if (this.current === undefined || this.currentVersion !== document.version) {
            this.current = VSCOBOLSourceScanner.getCachedObject(document, this.settings);
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