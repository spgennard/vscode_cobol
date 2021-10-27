// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import * as cobolProgram from "../cobolprogram";
import * as tabstopper from "../tabstopper";
import * as commenter from "../commenter";
import { VSCOBOLConfiguration } from "../vsconfiguration";
import { ICOBOLSettings } from "../iconfiguration";
import { VSExtensionUtils } from "../vsextutis";
import { CobolSymbolInformationProvider } from "../symbolprovider";

export function activate(context: vscode.ExtensionContext) {
    const commands = vscode.commands;
    const settings: ICOBOLSettings = VSCOBOLConfiguration.reinit(undefined);

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2pd", function () {
        cobolProgram.move2pd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2dd", function () {
        cobolProgram.move2dd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2ws", function () {
        cobolProgram.move2ws();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anyforward", function () {
        cobolProgram.move2anyforward();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anybackwards", function () {
        cobolProgram.move2anybackwards();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.tab", function () {
        tabstopper.processTabKey(true);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.revtab", function () {
        tabstopper.processTabKey(false);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.commentline", function () {
        if (vscode.window.activeTextEditor !== undefined) {
            commenter.processCommentLine();
        }
    }));

    const cobolSelectors = VSExtensionUtils.getAllCobolSelectors(settings);

    context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(cobolSelectors, new CobolSymbolInformationProvider()));

}

// this method is called when your extension is deactivated
export function deactivate() {
    //
}
