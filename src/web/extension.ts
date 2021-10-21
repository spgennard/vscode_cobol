// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as cobolProgram from '../cobolprogram';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
	const commands = vscode.commands;

    context.subscriptions.push(commands.registerCommand('cobolplugin.move2pd', function () {
        cobolProgram.move2pd();
    }));

    context.subscriptions.push(commands.registerCommand('cobolplugin.move2dd', function () {
        cobolProgram.move2dd();
    }));

    context.subscriptions.push(commands.registerCommand('cobolplugin.move2ws', function () {
        cobolProgram.move2ws();
    }));

    context.subscriptions.push(commands.registerCommand('cobolplugin.move2anyforward', function () {
        cobolProgram.move2anyforward();
    }));

    context.subscriptions.push(commands.registerCommand('cobolplugin.move2anybackwards', function () {
        cobolProgram.move2anybackwards();
    }));
}

// this method is called when your extension is deactivated
export function deactivate() {
	//
}
