import path from 'path';
import fs from 'fs';
import * as vscode from 'vscode';
import { getWorkspaceFolders } from './cobolfolders';
import { VSCOBOLConfiguration } from './configuration';
import { COBOLFileUtils } from './opencopybook';


interface BldScriptDefinition extends vscode.TaskDefinition {
	arguments: string;
}

export class BldScriptTaskProvider implements vscode.TaskProvider {
	static BldScriptType = 'bldScript';

	private bldScriptPromise: Thenable<vscode.Task[]> | undefined = undefined;

	public static scriptName = COBOLFileUtils.isWin32 ? "bld.bat" : "./bld.sh";
	public static scriptPrefix =  COBOLFileUtils.isWin32 ? "cmd.exe /c " : "";


	public provideTasks(): Thenable<vscode.Task[]> | undefined {
		const settings = VSCOBOLConfiguration.get();

		if (!settings.experimental_features) {
			return this.bldScriptPromise;
		}

		if (!this.bldScriptPromise) {
			this.bldScriptPromise = getBldScriptTasks();
		}
		return this.bldScriptPromise;
	}

	public resolveTask(_task: vscode.Task): vscode.Task | undefined {
		const settings = VSCOBOLConfiguration.get();
		if (!settings.experimental_features) {
			return undefined;
		}

		// does this workspace have a bld.sh or bld.bat
		if (!this.isFilePresent()) {
			return undefined;
		}
		const task = _task.definition.task;

		if (task) {
			// resolveTask requires that the same definition object be used.
			// eslint-disable-next-line @typescript-eslint/no-explicit-any
			const definition: BldScriptDefinition = <any>_task.definition;

			const she = new vscode.ShellExecution(`${BldScriptTaskProvider.scriptPrefix}${BldScriptTaskProvider.scriptName} ${definition.arguments}`);

			return new vscode.Task(definition.task, vscode.TaskScope.Workspace, BldScriptTaskProvider.BldScriptType, BldScriptTaskProvider.BldScriptType, she);
		}
	}

	private isFilePresent(): boolean {
		const ws = getWorkspaceFolders();
		if (ws !== undefined) {
			for (const folder of ws) {
				if (folder.uri.scheme === 'file') {
					const fname = path.join(folder.uri.fsPath, BldScriptTaskProvider.scriptName);

					try {
						if (fs.existsSync(fname)) {
							return true;
						}
					}
					catch {
						// continue
					}
				}

			}
		}

		return false;
	}
}

async function getBldScriptTasks(): Promise<vscode.Task[]> {
	const result: vscode.Task[] = [];

	const she = new vscode.ShellExecution(`${BldScriptTaskProvider.scriptPrefix}${BldScriptTaskProvider.scriptName}`);

	const taskDef: BldScriptDefinition = {
		type: BldScriptTaskProvider.BldScriptType,
		arguments: ""
	};

	const task = new vscode.Task(taskDef, vscode.TaskScope.Workspace, BldScriptTaskProvider.BldScriptType, BldScriptTaskProvider.BldScriptType, she);

	result.push(task);
	return result;
}
