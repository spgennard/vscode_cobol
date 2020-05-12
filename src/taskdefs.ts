import * as path from 'path';
import * as fs from 'fs';
import * as cp from 'child_process';
import * as vscode from 'vscode';
import { stringify } from 'querystring';
import { COBOLSettings } from './iconfiguration';
import { VSCOBOLConfiguration } from './configuration';
import { getLogicalCopybookdirs } from './extension';


export interface GnuCOBCTaskDefinition extends vscode.TaskDefinition {
	extraArguments?: string;
	label?: string;
	syntaxCheck?: boolean;
}

export interface MFCOBOLTaskDefinition extends vscode.TaskDefinition {
	extraArguments?: string;
	label?: string;
	syntaxCheck?: boolean;
}

export function getTaskForCOBOL(definition: MFCOBOLTaskDefinition, label?: string, syntaxCheck?: boolean): vscode.Task {
	let workspaceFolder = vscode.workspace.rootPath;

	if (label !== undefined) {
		definition.label = label;
	}

	if (syntaxCheck !== undefined) {
		definition.syntaxCheck = syntaxCheck;
	}

	let options: vscode.ShellExecutionOptions = { cwd: workspaceFolder };
	let shellCmd: vscode.ShellExecution = new vscode.ShellExecution(
		`cobol.exe \\$\{file\} NOINT NOGNT COPYPATH(${workspaceFolder}/CopyBooks;${workspaceFolder}\\CopyBooks\\Public) ${definition.extraArguments} `, options);

	let task = new vscode.Task(definition, vscode.TaskScope.Workspace, "Syntax check using cobc", 'COBOL', shellCmd);
	task.group = {
		"kind": "build",
		"isDefault": true
	};
	task.problemMatchers.push("$gnucobol3-cobc");
	task.execution = shellCmd;
	task.presentationOptions = {
		"echo": false,
		"focus": false,
		"showReuseMessage": false,
		"clear": true
	};
	return task;
}

export function getTaskForCOBC(definition: GnuCOBCTaskDefinition, label?: string, syntaxCheck?: boolean): vscode.Task {
	let workspaceFolder = vscode.workspace.rootPath;

	if (label !== undefined) {
		definition.label = label;
	}

	if (syntaxCheck !== undefined) {
		definition.syntaxCheck = syntaxCheck;
	}

	let options: vscode.ShellExecutionOptions = { cwd: workspaceFolder };
	let shellCmd: vscode.ShellExecution = new vscode.ShellExecution(
		`cobc -fsyntax-only -std=mf -I${workspaceFolder}\\CopyBooks -I${workspaceFolder}\\CopyBooks\\Public ${definition.extraArguments} \\$\{file\}`, options);

	let task = new vscode.Task(definition, vscode.TaskScope.Workspace, "Syntax check using cobc", 'COBOL', shellCmd);
	task.group = {
		"kind": "build",
		"isDefault": true
	};
	task.problemMatchers.push("$gnucobol3-cobc");
	task.execution = shellCmd;
	task.presentationOptions = {
		"echo": false,
		"focus": false,
		"showReuseMessage": false,
		"clear": true
	};
	return task;
}


export async function getCOBOLTasks_for_mfcobol(taskName: string, syntaxCheck: boolean): Promise<vscode.Task[]> {
	let workspaceFolder = vscode.workspace.rootPath;
	let emptyTasks: vscode.Task[] = [];
	if (!workspaceFolder) {
		return emptyTasks;
	}

	let copyBookArgs = "COPYPATH("+getLogicalCopybookdirs("", ";")+")";

	let kind: MFCOBOLTaskDefinition = {
		type: 'mfcobol',
		label: '',
		extraArguments: copyBookArgs
	};

	let result: vscode.Task[] = [];

	let task = getTaskForCOBOL(kind, taskName, syntaxCheck);
	result.push(task);
	return result;
}

export async function getCOBOLTasks_for_cobc(taskName: string, syntaxCheck: boolean): Promise<vscode.Task[]> {
	let workspaceFolder = vscode.workspace.rootPath;
	let emptyTasks: vscode.Task[] = [];
	if (!workspaceFolder) {
		return emptyTasks;
	}

	let copyBookArgs = getLogicalCopybookdirs("-I", " ");

	let kind: GnuCOBCTaskDefinition = {
		type: 'cobc',
		label: '',
		extraArguments: copyBookArgs
	};

	let result: vscode.Task[] = [];

	let task = getTaskForCOBC(kind, taskName, syntaxCheck);
	result.push(task);
	return result;

	/*
	 "label": cobc-syntax-check",
            "type": "shell",
            "presentation": {
                "echo": false,
                "reveal": "never",
                "focus": false,
                "panel": "shared",
                "showReuseMessage": false,
                "clear": true
            },
            "command": "cobc",
            "args": [
                "-fsyntax-only",
                "-std=mf",
                "-I${workspaceFolder}\\CopyBooks",
                "-I${workspaceFolder}\\CopyBooks\\Public",
                "${file}"
            ],
            "problemMatcher": "$gnucobol3-cobc",
            "group": {
                "kind": "build",
                "isDefault": true
            }
		},
		*/
}
