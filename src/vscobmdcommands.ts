import path from "path";
import * as vscode from "vscode";
import { CobmdDocument, CobmdFormat, CobmdTarget } from "./cobmdformat";
import { CobmdNotebookSerializer, cobmdNotebookType } from "./vscobmdnotebook";

const buildAllCommand = "cobolplugin.notebookBuildAll";
const runAllCommand = "cobolplugin.notebookRunAll";
const buildTargetCommand = "cobolplugin.notebookBuildTarget";
const runTargetCommand = "cobolplugin.notebookRunTarget";
const setTargetCommand = "cobolplugin.notebookSetTarget";
const openAsTextCommand = "cobolplugin.notebookOpenAsText";
const programFileCommand = "cobolplugin.notebookProgramFile";

interface CobmdSource {
    uri: vscode.Uri;
    document: CobmdDocument;
}

export class CobmdCommands implements vscode.Disposable {
    private readonly disposables: vscode.Disposable[] = [];
    private readonly serializer = new CobmdNotebookSerializer();
    private activeProgramFile: string | undefined;
    private running = false;
    private readonly enableTaskExecution: boolean;

    public static register(context: vscode.ExtensionContext, enableTaskExecution: boolean): CobmdCommands {
        const commands = new CobmdCommands(enableTaskExecution);
        context.subscriptions.push(commands);
        return commands;
    }

    private constructor(enableTaskExecution: boolean) {
        this.enableTaskExecution = enableTaskExecution;
        this.disposables.push(
            vscode.commands.registerCommand(buildAllCommand, () => this.buildAll()),
            vscode.commands.registerCommand(runAllCommand, () => this.runAll()),
            vscode.commands.registerCommand(buildTargetCommand, (uri: vscode.Uri, target: string) => this.buildTarget(uri, target)),
            vscode.commands.registerCommand(runTargetCommand, (uri: vscode.Uri, target: string) => this.runTarget(uri, target)),
            vscode.commands.registerCommand(setTargetCommand, (cell?: vscode.NotebookCell) => this.setTarget(cell)),
            vscode.commands.registerCommand(openAsTextCommand, () => this.openAsText()),
            vscode.commands.registerCommand(programFileCommand, () => this.activeProgramFile ?? "")
        );

        if (enableTaskExecution) {
            const controller = vscode.notebooks.createNotebookController(
                "cobol-markdown-task-runner",
                cobmdNotebookType,
                "COBOL Notebook Task Runner"
            );
            controller.supportedLanguages = ["COBOL", "BITLANG-COBOL", "ACUCOBOL", "RMCOBOL", "ILECOBOL", "COBOLIT"];
            controller.executeHandler = (cells, notebook) => this.executeCells(controller, cells, notebook);
            this.disposables.push(controller);
        }
    }

    public dispose(): void {
        for (const disposable of this.disposables) {
            disposable.dispose();
        }
    }

    private async buildAll(): Promise<void> {
        const source = await this.activeSource();
        if (source === undefined) {
            return;
        }
        const files = await this.materialize(source);
        if (files !== undefined) {
            await vscode.window.showInformationMessage(`Built ${files.size} COBOL notebook target(s).`);
        }
    }

    private async runAll(): Promise<void> {
        if (!this.enableTaskExecution) {
            await vscode.window.showErrorMessage("Running COBOL notebook tasks is unavailable in the web extension.");
            return;
        }
        const source = await this.activeSource();
        if (source !== undefined) {
            await this.runTargets(source, source.document.targets);
        }
    }

    private async buildTarget(uri: vscode.Uri, targetName: string): Promise<void> {
        const source = await this.sourceForUri(uri);
        const target = source?.document.targets.find(candidate => candidate.filename === targetName);
        if (source === undefined || target === undefined) {
            await vscode.window.showErrorMessage(`COBOL notebook target '${targetName}' was not found.`);
            return;
        }
        const files = await this.materialize(source, [target]);
        const generatedUri = files?.get(target.filename);
        if (generatedUri !== undefined) {
            const document = await vscode.workspace.openTextDocument(generatedUri);
            await vscode.languages.setTextDocumentLanguage(document, "COBOL");
            await vscode.window.showTextDocument(document, { preview: true, viewColumn: vscode.ViewColumn.Beside });
        }
    }

    private async runTarget(uri: vscode.Uri, targetName: string): Promise<void> {
        if (!this.enableTaskExecution) {
            return;
        }
        const source = await this.sourceForUri(uri);
        const target = source?.document.targets.find(candidate => candidate.filename === targetName);
        if (source === undefined || target === undefined) {
            await vscode.window.showErrorMessage(`COBOL notebook target '${targetName}' was not found.`);
            return;
        }
        await this.runTargets(source, [target]);
    }

    private async setTarget(requestedCell?: vscode.NotebookCell): Promise<void> {
        const editor = vscode.window.activeNotebookEditor;
        const cell = requestedCell ?? (editor === undefined ? undefined : editor.notebook.cellAt(editor.selection.start));
        if (editor === undefined || cell === undefined || cell.notebook !== editor.notebook) {
            await vscode.window.showErrorMessage("Select a COBOL notebook code cell first.");
            return;
        }

        const currentTarget = this.cellTarget(cell);
        const target = await vscode.window.showInputBox({
            title: "COBOL Notebook Target Filename",
            prompt: "Enter a relative COBOL source filename",
            value: currentTarget,
            placeHolder: "myprogram.cbl",
            validateInput: value => {
                const validation = CobmdFormat.parse(`\`\`\`cobol file=${JSON.stringify(value)}\n\`\`\``);
                return validation.diagnostics[0]?.message;
            }
        });
        if (target === undefined) {
            return;
        }

        const metadata = cell.metadata as { cobmd?: { language?: string; target?: string; originalValue?: string; originalSource?: string } };
        const edit = new vscode.WorkspaceEdit();
        edit.set(editor.notebook.uri, [vscode.NotebookEdit.updateCellMetadata(cell.index, {
            ...cell.metadata,
            cobmd: {
                ...metadata.cobmd,
                target
            }
        })]);
        await vscode.workspace.applyEdit(edit);
    }

    private async openAsText(): Promise<void> {
        const uri = vscode.window.activeNotebookEditor?.notebook.uri;
        if (uri !== undefined) {
            await vscode.commands.executeCommand("vscode.openWith", uri, "default");
        }
    }

    private async executeCells(
        controller: vscode.NotebookController,
        cells: vscode.NotebookCell[],
        notebook: vscode.NotebookDocument
    ): Promise<void> {
        const source = this.sourceFromNotebook(notebook);
        const targetNames = new Set(cells
            .map(cell => this.cellTarget(cell))
            .filter((target): target is string => target !== undefined));
        const targets = source.document.targets.filter(target => targetNames.has(target.filename));
        const executions = cells.map(cell => controller.createNotebookCellExecution(cell));
        executions.forEach(execution => execution.start(Date.now()));

        try {
            const success = targets.length !== 0 && await this.runTargets(source, targets);
            executions.forEach(execution => execution.end(success, Date.now()));
        } catch (error) {
            executions.forEach(execution => execution.end(false, Date.now()));
            throw error;
        }
    }

    private async runTargets(source: CobmdSource, targets: CobmdTarget[]): Promise<boolean> {
        if (!vscode.workspace.isTrusted) {
            await vscode.window.showWarningMessage("Running a COBOL notebook requires a trusted workspace.");
            return false;
        }
        if (this.running) {
            await vscode.window.showWarningMessage("A COBOL notebook task is already running.");
            return false;
        }
        if (targets.length === 0) {
            await vscode.window.showErrorMessage("The COBOL notebook has no runnable targets.");
            return false;
        }

        const files = await this.materialize(source, targets);
        if (files === undefined) {
            return false;
        }
        const task = await this.findTask();
        if (task === undefined) {
            return false;
        }

        this.running = true;
        try {
            for (const target of targets) {
                const generatedUri = files.get(target.filename);
                if (generatedUri === undefined) {
                    return false;
                }
                this.activeProgramFile = generatedUri.fsPath || generatedUri.toString(true);
                if (!await this.executeTask(task)) {
                    await vscode.window.showErrorMessage(`COBOL notebook task failed for ${target.filename}.`);
                    return false;
                }
            }
            return true;
        } finally {
            this.activeProgramFile = undefined;
            this.running = false;
        }
    }

    private async activeSource(): Promise<CobmdSource | undefined> {
        const notebook = vscode.window.activeNotebookEditor?.notebook;
        if (notebook?.notebookType === cobmdNotebookType) {
            return this.sourceFromNotebook(notebook);
        }

        const document = vscode.window.activeTextEditor?.document;
        if (document?.languageId !== "cobmd") {
            await vscode.window.showErrorMessage("Open a .cobmd document before running this command.");
            return undefined;
        }
        return {
            uri: document.uri,
            document: CobmdFormat.parse(document.getText())
        };
    }

    private async sourceForUri(uri: vscode.Uri): Promise<CobmdSource | undefined> {
        const openDocument = vscode.workspace.textDocuments.find(document => document.uri.toString() === uri.toString());
        const document = openDocument ?? await vscode.workspace.openTextDocument(uri);
        if (document.languageId !== "cobmd") {
            return undefined;
        }
        return { uri, document: CobmdFormat.parse(document.getText()) };
    }

    private sourceFromNotebook(notebook: vscode.NotebookDocument): CobmdSource {
        const cells = notebook.getCells().map(cell => {
            const data = new vscode.NotebookCellData(cell.kind, cell.document.getText(), cell.document.languageId);
            data.metadata = cell.metadata;
            return data;
        });
        const data = new vscode.NotebookData(cells);
        data.metadata = notebook.metadata;
        const source = Buffer.from(this.serializer.serializeNotebook(data)).toString("utf8");
        return { uri: notebook.uri, document: CobmdFormat.parse(source) };
    }

    private cellTarget(cell: vscode.NotebookCell): string | undefined {
        const metadata = cell.metadata as { cobmd?: { target?: string } };
        return metadata.cobmd?.target;
    }

    private async materialize(source: CobmdSource, requestedTargets = source.document.targets): Promise<Map<string, vscode.Uri> | undefined> {
        const errors = source.document.diagnostics.filter(diagnostic => diagnostic.severity === "error");
        if (errors.length !== 0) {
            await vscode.window.showErrorMessage(`Cannot build COBOL notebook: ${errors[0].message}`);
            return undefined;
        }
        if (requestedTargets.length === 0) {
            await vscode.window.showErrorMessage("The COBOL notebook has no targets to build.");
            return undefined;
        }

        const workspaceFolder = vscode.workspace.getWorkspaceFolder(source.uri);
        if (workspaceFolder === undefined) {
            await vscode.window.showErrorMessage("The COBOL notebook must be inside a workspace folder.");
            return undefined;
        }

        const configuration = vscode.workspace.getConfiguration("coboleditor.notebook", source.uri);
        const configuredRoot = configuration.get<string>("generatedRoot", ".cobol-notebook");
        const safeRoot = configuredRoot.split(/[\\/]+/).filter(segment => segment.length !== 0 && segment !== ".");
        if (path.isAbsolute(configuredRoot) || /^(?:[a-zA-Z]:[\\/]|[\\/])/.test(configuredRoot) ||
            safeRoot.length === 0 || safeRoot.some(segment => segment === "..")) {
            await vscode.window.showErrorMessage("COBOL notebook generatedRoot must be a relative path without parent segments.");
            return undefined;
        }
        const notebookName = path.posix.basename(source.uri.path, path.posix.extname(source.uri.path)).replace(/[^a-zA-Z0-9._-]/g, "_");
        const outputRoot = vscode.Uri.joinPath(workspaceFolder.uri, ...safeRoot, notebookName);
        const result = new Map<string, vscode.Uri>();

        for (const target of requestedTargets) {
            const segments = target.filename.split("/");
            const targetUri = vscode.Uri.joinPath(outputRoot, ...segments);
            if (segments.length > 1) {
                await vscode.workspace.fs.createDirectory(vscode.Uri.joinPath(outputRoot, ...segments.slice(0, -1)));
            } else {
                await vscode.workspace.fs.createDirectory(outputRoot);
            }
            await vscode.workspace.fs.writeFile(targetUri, Buffer.from(target.content, "utf8"));
            result.set(target.filename, targetUri);
        }
        return result;
    }

    private async findTask(): Promise<vscode.Task | undefined> {
        const taskLabel = vscode.workspace.getConfiguration("coboleditor.notebook").get<string>("taskLabel", "cobol notebook run");
        const task = (await vscode.tasks.fetchTasks()).find(candidate => candidate.name === taskLabel);
        if (task !== undefined) {
            return task;
        }

        const configure = "Configure Task";
        const choice = await vscode.window.showErrorMessage(
            `Task '${taskLabel}' was not found.`,
            configure
        );
        if (choice === configure) {
            await vscode.commands.executeCommand("workbench.action.tasks.configureTask");
        }
        return undefined;
    }

    private async executeTask(task: vscode.Task): Promise<boolean> {
        const execution = await vscode.tasks.executeTask(task);
        return new Promise<boolean>(resolve => {
            let exitCode: number | undefined;
            const processDisposable = vscode.tasks.onDidEndTaskProcess(event => {
                if (event.execution === execution) {
                    exitCode = event.exitCode;
                }
            });
            const taskDisposable = vscode.tasks.onDidEndTask(event => {
                if (event.execution === execution) {
                    processDisposable.dispose();
                    taskDisposable.dispose();
                    resolve(exitCode === undefined || exitCode === 0);
                }
            });
        });
    }
}