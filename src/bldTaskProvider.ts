import path, { dirname } from 'path';
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
			return undefined;
		}

		const scriptFilename = this.getFileFromWorkspace();
		if (scriptFilename === undefined) {
			return undefined;

		}
		if (!this.bldScriptPromise) {
			this.bldScriptPromise = getBldScriptTasks(scriptFilename);
		}
		return this.bldScriptPromise;
	}

	public static getSHEOptions(scriptName: string):vscode.ShellExecutionOptions {

		const sheOpts : vscode.ShellExecutionOptions = {
			cwd : dirname(scriptName)
		};

		return sheOpts;
	}

	public static getProblemMatchers(): string[] {
		const matchers = [];
		const envACUCOBOL = process.env["ACUCOBOL"];
		const envCOBDIR = process.env["COBDIR"];

		if (envACUCOBOL !== undefined) {
			matchers.push("$acucobol-warning-ccbl");
			matchers.push("$acucobol-ccbl");
		}

		if (envCOBDIR !== undefined) {
			matchers.push("$mfcobol-errformat3");
		}

		return matchers;
	}

	public resolveTask(_task: vscode.Task): vscode.Task | undefined {
		const settings = VSCOBOLConfiguration.get();
		if (!settings.experimental_features) {
			return undefined;
		}

		const scriptName = this.getFileFromWorkspace();

		// does this workspace have a bld.sh or bld.bat
		if (scriptName === undefined) {
			return undefined;
		}

		const task = _task.definition.task;

		if (task) {
			// resolveTask requires that the same definition object be used.
			// eslint-disable-next-line @typescript-eslint/no-explicit-any
			const definition: BldScriptDefinition = <any>_task.definition;

			const she = new vscode.ShellExecution(`${BldScriptTaskProvider.scriptPrefix}${BldScriptTaskProvider.scriptName} ${definition.arguments}`, BldScriptTaskProvider.getSHEOptions(scriptName));
			const rtask = new vscode.Task(definition.task, vscode.TaskScope.Workspace, BldScriptTaskProvider.BldScriptType, BldScriptTaskProvider.BldScriptType, she, BldScriptTaskProvider.getProblemMatchers());
			const dname = path.dirname(scriptName);
			const fname = `${scriptName.substr(1 + dname.length)} (in ${dname})`;
			task.detail = `Execute ${fname}`;

			return rtask;
		}
	}

	private getFileFromWorkspace(): string|undefined {
		const ws = getWorkspaceFolders();
		if (ws !== undefined) {
			for (const folder of ws) {
				if (folder.uri.scheme === 'file') {
					const fname = path.join(folder.uri.fsPath, BldScriptTaskProvider.scriptName);

					try {
						if (fs.existsSync(fname)) {
							return fname;
						}
					}
					catch {
						// continue
					}
				}

			}
		}

		return undefined;
	}
}

async function getBldScriptTasks(scriptName: string): Promise<vscode.Task[]> {
	const result: vscode.Task[] = [];

	const she = new vscode.ShellExecution(`${BldScriptTaskProvider.scriptPrefix}${BldScriptTaskProvider.scriptName}`, BldScriptTaskProvider.getSHEOptions(scriptName));

	const taskDef: BldScriptDefinition = {
		type: BldScriptTaskProvider.BldScriptType,
		arguments: ""
	};

	const task = new vscode.Task(taskDef, vscode.TaskScope.Workspace, BldScriptTaskProvider.BldScriptType, "Build script", she, BldScriptTaskProvider.getProblemMatchers() );
	const dname = path.dirname(scriptName);
	const fname = scriptName.substr(1 + dname.length);
	task.detail = `Execute ${fname}`;
	task.group = vscode.TaskGroup.Build;
	result.push(task);
	return result;
}
