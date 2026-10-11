import * as vscode from "vscode";
import { CobmdFormat } from "./cobmdformat";

export class CobmdCodeLensProvider implements vscode.CodeLensProvider, vscode.Disposable {
    private readonly diagnostics = vscode.languages.createDiagnosticCollection("cobmd");
    private readonly disposables: vscode.Disposable[] = [];

    public static register(context: vscode.ExtensionContext, enableTaskExecution: boolean): CobmdCodeLensProvider {
        const provider = new CobmdCodeLensProvider(enableTaskExecution);
        context.subscriptions.push(
            provider,
            vscode.languages.registerCodeLensProvider({ language: "cobmd" }, provider)
        );
        return provider;
    }

    private constructor(private readonly enableTaskExecution: boolean) {
        this.disposables.push(
            vscode.workspace.onDidOpenTextDocument(document => this.updateDiagnostics(document)),
            vscode.workspace.onDidChangeTextDocument(event => this.updateDiagnostics(event.document)),
            vscode.workspace.onDidCloseTextDocument(document => this.diagnostics.delete(document.uri))
        );
        for (const document of vscode.workspace.textDocuments) {
            this.updateDiagnostics(document);
        }
    }

    public provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
        const parsed = CobmdFormat.parse(document.getText());
        const lenses: vscode.CodeLens[] = [];
        for (const fragment of parsed.fragments) {
            if (fragment.target === undefined) {
                continue;
            }
            const range = new vscode.Range(fragment.openingLine, 0, fragment.openingLine, 0);
            lenses.push(new vscode.CodeLens(range, {
                command: "cobolplugin.notebookBuildTarget",
                title: `Build ${fragment.target}`,
                arguments: [document.uri, fragment.target]
            }));
            if (this.enableTaskExecution) {
                lenses.push(new vscode.CodeLens(range, {
                    command: "cobolplugin.notebookRunTarget",
                    title: `Run ${fragment.target}`,
                    arguments: [document.uri, fragment.target]
                }));
            }
        }
        return lenses;
    }

    public dispose(): void {
        this.diagnostics.dispose();
        for (const disposable of this.disposables) {
            disposable.dispose();
        }
    }

    private updateDiagnostics(document: vscode.TextDocument): void {
        if (document.languageId !== "cobmd") {
            return;
        }
        const diagnostics = CobmdFormat.parse(document.getText()).diagnostics.map(problem => {
            const line = Math.min(problem.line, Math.max(0, document.lineCount - 1));
            const diagnostic = new vscode.Diagnostic(
                document.lineAt(line).range,
                problem.message,
                problem.severity === "error" ? vscode.DiagnosticSeverity.Error : vscode.DiagnosticSeverity.Warning
            );
            diagnostic.code = problem.code;
            diagnostic.source = "COBOL Markdown";
            return diagnostic;
        });
        this.diagnostics.set(document.uri, diagnostics);
    }
}