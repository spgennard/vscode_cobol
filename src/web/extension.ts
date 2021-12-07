// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import { VSCOBOLConfiguration } from "../vsconfiguration";
import { ICOBOLSettings } from "../iconfiguration";
import { VSExtensionUtils } from "../vsextutis";
import { CobolSymbolInformationProvider } from "../symbolprovider";
import { VSExternalFeatures } from "../vsexternalfeatures";
import { COBOLProgramCommands } from "../cobolprogram";
import { TabUtils } from "../tabstopper";
import { VSCOBOLSourceScanner } from "../vscobolscanner";
import { VSmargindecorations } from "../margindecorations";
import { commentUtils } from "../commenter";
import { VSLogger } from "../vslogger";

function showExtensionInformation():void {
    const thisExtension = vscode.extensions.getExtension("bitlang.cobol");

    if (thisExtension !== undefined) {
        if (vscode.env.uriScheme !== "vscode") {
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Warning: you are using a untested environment : ${vscode.env.uriScheme}`);
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Version                                     : ${vscode.version}`);
        } else {
            VSLogger.logMessage(`VSCode version                              : ${vscode.version}`);
        }
        VSLogger.logMessage("Extension Information:");
        VSLogger.logMessage(` Extension path                             : ${thisExtension.extensionPath}`);
        VSLogger.logMessage(` Version                                    : ${thisExtension.packageJSON.version}`);
    }
}
export function activate(context: vscode.ExtensionContext) {
    VSCOBOLConfiguration.externalFeatures = VSExternalFeatures;

    const commands = vscode.commands;
    const settings: ICOBOLSettings = VSCOBOLConfiguration.reinit();
    
    context.subscriptions.push(commands.registerCommand("cobolplugin.move2pd", function () {
        COBOLProgramCommands.move2pd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2dd", function () {
        COBOLProgramCommands.move2dd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2ws", function () {
        COBOLProgramCommands.move2ws();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anyforward", function () {
        COBOLProgramCommands.move2anyforward();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anybackwards", function () {
        COBOLProgramCommands.move2anybackwards();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.tab", function () {
        TabUtils.processTabKey(true);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.revtab", function () {
        TabUtils.processTabKey(false);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.commentline", function () {
        if (vscode.window.activeTextEditor !== undefined) {
            commentUtils.processCommentLine();
        }
    }));

    const cobolSelectors = VSExtensionUtils.getAllCobolSelectors(settings);

    try {
        context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(cobolSelectors, new CobolSymbolInformationProvider()));
    } catch(e) {
        VSExternalFeatures.logException("during registerDocumentSymbolProvider",e as Error);
    }

    const onDidCloseTextDocumentHandler = vscode.workspace.onDidCloseTextDocument(async (doc: vscode.TextDocument) => {
        if (VSExtensionUtils.isSupportedLanguage(doc)) {
            VSCOBOLSourceScanner.removeCachedObject(doc, settings);
        }
    });
    context.subscriptions.push(onDidCloseTextDocumentHandler);

    vscode.window.onDidChangeActiveTextEditor(editor => {
        if (!editor) {
            return;
        }
        VSmargindecorations.updateDecorations(editor);
        // linter.updateLinter(editor.document);

    }, null, context.subscriptions);

    vscode.window.onDidChangeTextEditorSelection(event => {
        if (!event.textEditor) {
            return;
        }
        VSmargindecorations.updateDecorations(event.textEditor);
        //cobolusage.updateDiagnostics(event.textEditor.document);
    }, null, context.subscriptions);

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    vscode.workspace.onDidChangeTextDocument(event => {
        if (!vscode.window.activeTextEditor) {
            return;
        }
        VSmargindecorations.updateDecorations(vscode.window.activeTextEditor);
        // linter.updateLinter(window.activeTextEditor.document);
    }, null, context.subscriptions);

    if (vscode.window.activeTextEditor !== undefined) {
        VSmargindecorations.updateDecorations(vscode.window.activeTextEditor);
        // linter.updateLinter(window.activeTextEditor.document);
    }
    // const onDidOpenTextDocumentHandler = vscode.workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {
    //     VSLogger.logMessage(`Open document ${doc.fileName} / ${doc.uri.scheme}`);
    // });
    // context.subscriptions.push(onDidOpenTextDocumentHandler);

    showExtensionInformation();


}

// this method is called when your extension is deactivated
export function deactivate() {
    //
}
