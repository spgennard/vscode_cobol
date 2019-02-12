'use strict';

import { commands, workspace, StatusBarItem, StatusBarAlignment, DecorationOptions, Range, ExtensionContext, languages, TextDocument, TextEditor, Position, CancellationToken, ProviderResult, Definition, window } from 'vscode';
import * as cobolProgram from './cobolprogram';
import * as tabstopper from './tabstopper';
import * as opencopybook from './opencopybook';
import { DocComment } from './formatting/DocComment';
import { TextAutocompleteCompletionItemProvider } from './textprovider';
import { ESourceFormat } from './margindecorations';

import { jclStatements } from "./keywords/jclstatements";
import CobolDocumentSymbolProvider from './symbolprovider';
import * as sourcedefinitionprovider from './sourcedefinitionprovider';
import updateDecorations from './margindecorations';

let formatStatusBarItem: StatusBarItem;

export function activate(context: ExtensionContext) {
    var move2pdCommand = commands.registerCommand('cobolplugin.move2pd', function () {
        cobolProgram.move2pd();
    });

    var move2ddCommand = commands.registerCommand('cobolplugin.move2dd', function () {
        cobolProgram.move2dd();
    });

    var move2wsCommand = commands.registerCommand('cobolplugin.move2ws', function () {
        cobolProgram.move2ws();
    });

    var move2anyforwardCommand = commands.registerCommand('cobolplugin.move2anyforward', function () {
        cobolProgram.move2anyforward();
    });

    var move2anybackwardsCommand = commands.registerCommand('cobolplugin.move2anybackwards', function () {
        cobolProgram.move2anybackwards();
    });

    var tabCommand = commands.registerCommand('cobolplugin.tab', function () {
        tabstopper.processTabKey(true);
    });
    var unTabCommand = commands.registerCommand('cobolplugin.revtab', function () {
        tabstopper.processTabKey(false);
    });

    var changeSourceFormat = commands.registerCommand('cobolplugin.change_source_format', function () {
        // margindecorations.changeSourceFormat();
        const action = 'Open Settings';
        window.showWarningMessage("Change coboleditor setting?", action).then((selection) => {
            if (action === selection) {
                let ws = workspace.getWorkspaceFolder;
                if (ws === undefined || ws === null) {
                    commands.executeCommand("workbench.action.openWorkspaceSettings", "coboleditor");
                }
                else {
                    commands.executeCommand("workbench.action.openGlobalSettings", "coboleditor");
                }
            }
        });
    });

    context.subscriptions.push(move2pdCommand);
    context.subscriptions.push(move2ddCommand);
    context.subscriptions.push(move2wsCommand);
    context.subscriptions.push(move2anyforwardCommand);
    context.subscriptions.push(move2anybackwardsCommand);
    context.subscriptions.push(tabCommand);
    context.subscriptions.push(unTabCommand);
    context.subscriptions.push(changeSourceFormat);

    const allCobolSelectors = [
        { scheme: 'file', language: 'COBOL' },
        { scheme: 'file', language: 'ACUCOBOL' },
        { scheme: 'file', language: 'OpenCOBOL' }
    ];
    languages.registerDefinitionProvider(allCobolSelectors, {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            return opencopybook.provideDefinition(doc, pos, ct);
        }
    });
    languages.registerDefinitionProvider(allCobolSelectors, {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            return sourcedefinitionprovider.provideDefinition(doc, pos, ct);
        }
    });

    context.subscriptions.push(DocComment.register());


    // const completionItemProvider = new TextAutocompleteCompletionItemProvider(cobolKeywords);
    // const completionItemProviderDisposable = languages.registerCompletionItemProvider(allCobolSelectors, completionItemProvider);
    // context.subscriptions.push(completionItemProviderDisposable);


    const jclSelectors = [
        { scheme: 'file', language: 'JCL' }
    ];
    const completionJCLItemProvider = new TextAutocompleteCompletionItemProvider(jclStatements);
    const completionJCLItemProviderDisposable = languages.registerCompletionItemProvider(jclSelectors, completionJCLItemProvider);
    context.subscriptions.push(completionJCLItemProviderDisposable);

    /* TODO: add .DIR keywords too */
    const documentSymbolProvider = new CobolDocumentSymbolProvider();
    context.subscriptions.push(languages.registerDocumentSymbolProvider(allCobolSelectors, documentSymbolProvider));
    context.subscriptions.push(DocComment.register());

    window.onDidChangeActiveTextEditor(editor => {
        if (!editor) {
            return;
        }
        updateDecorations(editor);
    }, null, context.subscriptions);

    window.onDidChangeTextEditorSelection(event => {
        if (!event.textEditor) {
            return;
        }
        updateDecorations(event.textEditor);
    }, null, context.subscriptions);

    workspace.onDidChangeTextDocument(event => {
        if (!window.activeTextEditor) {
            return;
        }
        updateDecorations(window.activeTextEditor);
    }, null, context.subscriptions);

    formatStatusBarItem = window.createStatusBarItem(StatusBarAlignment.Right);
    formatStatusBarItem.command = "cobolplugin.change_source_format";
    formatStatusBarItem.show();

    updateDecorations(window.activeTextEditor);
}

export function enableMarginStatusBar(formatStyle: ESourceFormat) {
    formatStatusBarItem.text = "Source:"+formatStyle;
    formatStyle.toString();
    formatStatusBarItem.show();
}

export function hideMarginStatusBar() {
    formatStatusBarItem.hide();
}

export function deactivate() {
    formatStatusBarItem.dispose();
}
