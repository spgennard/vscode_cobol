
import * as vscode from 'vscode';
import { VSCodeSourceHandler } from './VSCodeSourceHandler';
import COBOLQuickParse, { SharedSourceReferences } from './cobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import { CodeActionProvider, CodeAction } from 'vscode';
import { isSupportedLanguage, TextLanguage } from './margindecorations';
import { logMessage } from './extension';
import { ICommentCallback } from './isourcehandler';

export class CobolUsageActionFixer implements CodeActionProvider {
    provideCodeActions(document: vscode.TextDocument, range: vscode.Range | vscode.Selection, context: vscode.CodeActionContext, token: vscode.CancellationToken): vscode.ProviderResult<(vscode.Command | vscode.CodeAction)[]> {
        const codeActions: CodeAction[] = [];
        for (const diagnostic of context.diagnostics) {
            if (diagnostic.code !== undefined && diagnostic.code.toString().startsWith("ignore") === false) {
                continue;
            }
            var startOfline= document.offsetAt(new vscode.Position(diagnostic.range.start.line,0));
            codeActions.push({
                title: `Add COBOL lint ignore comment for '${diagnostic.message}'`,
                diagnostics: [diagnostic],
                command: {
                    title: 'Add COBOL lint comment to ignore the warning',
                    command: "cobolplugin.insertIgnoreCommentLine",
                    arguments: [document.uri, startOfline, diagnostic.code],
                },
                kind: vscode.CodeActionKind.QuickFix,
            });
        }
        return codeActions;
    }

    public async insertIgnoreCommentLine(docUri: vscode.Uri, offset: number, code: string) {
        await vscode.window.showTextDocument(docUri);
        let w = vscode.window.activeTextEditor;
        
        if (w !== undefined) {
            var pos = w.document.positionAt(offset);
            w.edit(edit => {
                edit.insert(pos,"       *> cobol-lint "+code+"\n");
            });
        }
    }
}

export class CobolUsageProvider implements ICommentCallback{
    private enabled: boolean = false;

    private collection: vscode.DiagnosticCollection;
    private diagCollect: vscode.DiagnosticSeverity;

    constructor(collection: vscode.DiagnosticCollection, enabled: boolean) {
        this.collection = collection;
        this.enabled = enabled;
        this.diagCollect = vscode.DiagnosticSeverity.Information;
    }



    public updateDiagnostics(document: vscode.TextDocument, ): void {
        if (this.enabled === false) {
            return;
        }

        /* drop out if not COBOL */
        if (isSupportedLanguage(document) !== TextLanguage.COBOL) {
            return;
        }

        /* has it changed? */
        if (this.setupCOBOLQuickParse(document) === false) {
            return;
        }

        if (this.sourceRefs === undefined || this.current === undefined) {
            return;
        }

        let qp: COBOLQuickParse = this.current;
        let sourceRefs: SharedSourceReferences = this.sourceRefs;

        let diagRefs = new Map<string, vscode.Diagnostic[]>();
        this.collection.clear();

        for (let [key, token] of qp.paragraphs) {
            let workLower = key.toLowerCase();
            if (this.ignoreUnusedSymbol.has(workLower)) {
                continue;
            }
            if (sourceRefs.targetReferences.has(workLower) === false) {
                let r = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                    new vscode.Position(token.startLine, token.startColumn + token.tokenName.length));
                let d = new vscode.Diagnostic(r, key + ' paragraph is not referenced', this.diagCollect);
                d.code ="ignore "+key;

                if (diagRefs.has(token.filename)) {
                    let arr = diagRefs.get(token.filename);
                    if (arr !== undefined) {
                        arr.push(d);
                    }
                } else {
                    let arr: vscode.Diagnostic[] = [];
                    arr.push(d);
                    diagRefs.set(token.filename, arr);
                }
            }
        }

        for (let [key, token] of qp.sections) {
            let workLower = key.toLowerCase();
            if (this.ignoreUnusedSymbol.has(workLower)) {
                continue;
            }
            if (token.inProcedureDivision) {
                if (sourceRefs.targetReferences.has(workLower) === false) {
                    let r = new vscode.Range(new vscode.Position(token.startLine, token.startColumn),
                        new vscode.Position(token.startLine, token.startColumn + token.tokenName.length));
                    let d = new vscode.Diagnostic(r, key + ' section is not referenced', this.diagCollect);
                    d.code ="noref("+key+")";

                    if (diagRefs.has(token.filename)) {
                        let arr = diagRefs.get(token.filename);
                        if (arr !== undefined) {
                            arr.push(d);
                        }
                    } else {
                        let arr: vscode.Diagnostic[] = [];
                        arr.push(d);
                        diagRefs.set(token.filename, arr);
                    }
                }
            }

        }


        for (let [f, value] of diagRefs) {
            let u = vscode.Uri.file(f);
            this.collection.set(u, value);
        }
    }

    private current?: COBOLQuickParse;
    private currentVersion?: number;
    private sourceRefs?: SharedSourceReferences;
    private ignoreUnusedSymbol: Map<string,string> = new Map<string,string>();


    private setupCOBOLQuickParse(document: vscode.TextDocument):boolean {
        if (this.current !== undefined && this.current.filename !== document.fileName) {
            this.current = undefined;
        }

        // cache current document, interatives search to be faster
        if (this.current === undefined || this.currentVersion !== document.version) {
            this.ignoreUnusedSymbol.clear();
            let file = new VSCodeSourceHandler(document, false, this);
            this.sourceRefs = new SharedSourceReferences();
            this.current = new COBOLQuickParse(file, document.fileName, VSCOBOLConfiguration.get(), "", this.sourceRefs);
            this.currentVersion = document.version;
            return true;
        }

        return false;
    }

    private cobolLintLiteral = "cobol-lint";
    
    processComment(commentLine: string): void {
        let startOfComment:number=commentLine.indexOf("*>");
        if (startOfComment !== undefined && startOfComment !== -1) {
            var comment = commentLine.substring(2+startOfComment).trim();
            if (comment.startsWith(this.cobolLintLiteral)) {
                let startOfCOBOLint:number=comment.indexOf(this.cobolLintLiteral);
                var commentCommandArgs= comment.substring(this.cobolLintLiteral.length+startOfCOBOLint).trim();
                var args = commentCommandArgs.split(" ");
                var command = args[0];
                args = args.slice(1);
                var commandTrimmed = command !== undefined ? command.trim() : undefined;
                if (commandTrimmed !== undefined && commandTrimmed.toLocaleLowerCase() === "ignore") {
                    args.forEach((symbol) => {
                        this.ignoreUnusedSymbol.set(symbol.toLocaleLowerCase(), symbol);
                    });
                }
                

            }
            
        }
    }

}