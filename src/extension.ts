'use strict';

import { commands, ExtensionContext, languages, TextDocument, Position, CancellationToken, ProviderResult, Definition } from 'vscode';
import * as cobolProgram from './cobolprogram';
import * as tabstopper from './tabstopper';
import * as opencopybook from './opencopybook';
import { DocComment } from './formatting/DocComment';
import { TextAutocompleteCompletionItemProvider } from './textprovider';
import { cobolKeywords } from './keywords/cobolKeywords';
import { jclStatements } from "./keywords/jclstatements";
import CobolDocumentSymbolProvider from './symbolprovider';
import * as sourcedefinitionprovider from './sourcedefinitionprovider';

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

    context.subscriptions.push(move2pdCommand);
    context.subscriptions.push(move2ddCommand);
    context.subscriptions.push(move2wsCommand);
    context.subscriptions.push(move2anyforwardCommand);
    context.subscriptions.push(move2anybackwardsCommand);
    context.subscriptions.push(tabCommand);
    context.subscriptions.push(unTabCommand);

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

	const completionItemProvider = new TextAutocompleteCompletionItemProvider(cobolKeywords);
    /* TODO: find out the ACU & OpenCOBOL/GNU keyword list */
    const completionItemProviderDisposable = languages.registerCompletionItemProvider(allCobolSelectors, completionItemProvider);
    context.subscriptions.push(completionItemProviderDisposable);


    const jclSelectors = [
        { scheme: 'file', language: 'JCL' }
    ];
    const completionJCLItemProvider = new TextAutocompleteCompletionItemProvider(jclStatements);
    const completionJCLItemProviderDisposable = languages.registerCompletionItemProvider(jclSelectors, completionJCLItemProvider);
    context.subscriptions.push(completionJCLItemProviderDisposable);

    /* TODO: add .DIR keywords too */ 

    const symbol = new CobolDocumentSymbolProvider();
    context.subscriptions.push(languages.registerDocumentSymbolProvider(allCobolSelectors,symbol));
    context.subscriptions.push(DocComment.register());
}

export function deactivate() {
}
