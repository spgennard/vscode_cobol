import * as path from 'path';
import * as fs from 'fs';
import * as cp from 'child_process';
import * as vscode from 'vscode';


export interface COBOLTaskDefinition extends vscode.TaskDefinition {
	/**
	 * The task name
	 */
	task: string;

	/**
	 * The rake file containing the task
	 */
	file?: string;
}

export async function getCOBOLTasks(): Promise<vscode.Task[]> {
	let workspaceRoot = vscode.workspace.rootPath;
	let emptyTasks: vscode.Task[] = [];
	if (!workspaceRoot) {
		return emptyTasks;
	}

	let taskName = 'compile-one';
	let kind: COBOLTaskDefinition = {
		type: 'cobol',
		task: taskName
	};

	let options: vscode.ShellExecutionOptions = { cwd: workspaceRoot };
	let result: vscode.Task[] = [];
	let shellCmd: vscode.ShellExecution = new vscode.ShellExecution("cobc -fsyntax-only -std=mf -I${workspaceFolder}\\CopyBooks -I${workspaceFolder}\\CopyBooks\\Public \\${file}", options);
			
	let task = new vscode.Task(kind, taskName, 'shell', shellCmd);
	task.group = vscode.TaskGroup.Build;
	task.problemMatchers.push("$gnucobol3-cobc");
	task.execution = shellCmd;
	result.push(task);
	return result;
}