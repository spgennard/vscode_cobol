import * as path from 'path';
import * as fs from 'fs';
import * as cp from 'child_process';
import * as vscode from 'vscode';
import { stringify } from 'querystring';
import { COBOLSettings } from './iconfiguration';
import { VSCOBOLConfiguration } from './configuration';


export interface COBCTaskDefinition extends vscode.TaskDefinition {
	extraArguments?: string;
	label?: string;
}

export function getTaskForCOBC(definition: COBCTaskDefinition, label?: string) : vscode.Task {
	let workspaceFolder = vscode.workspace.rootPath;

	if (label !== undefined) {
		definition.label = label;
	}
	let options: vscode.ShellExecutionOptions = { cwd: workspaceFolder };
	let shellCmd: vscode.ShellExecution = new vscode.ShellExecution(
		`cobc -fsyntax-only -std=mf -I${workspaceFolder}\\CopyBooks -I${workspaceFolder}\\CopyBooks\\Public ${definition.extraArguments} \\$\{file\}`, options);
	
	let task = new vscode.Task(definition, vscode.TaskScope.Workspace, "Compile using cobc", 'COBOL Utilities', shellCmd);
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

export async function getCOBOLTasks(): Promise<vscode.Task[]> {
	let workspaceFolder = vscode.workspace.rootPath;
	let emptyTasks: vscode.Task[] = [];
	if (!workspaceFolder) {
		return emptyTasks;
	}

	let x = VSCOBOLConfiguration.getCopybookdirs_defaults;
	
	let kind: COBCTaskDefinition = {
		type: 'cobc',
		label: '',
		extraArguments: ''
	};

	let result: vscode.Task[] = [];
	let taskName = 'compile-one-cobc';

	let task = getTaskForCOBC(kind,taskName);
	result.push(task);
	return result;

	/*
	 "label": "cobol-syntax-check",
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