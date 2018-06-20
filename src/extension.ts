'use strict';

import { commands, ExtensionContext, languages, TextDocument, Position, CancellationToken, ProviderResult, Definition } from 'vscode';
import * as cobolProgram from './cobolprogram';
import * as tabstopper from './tabstopper';
import * as opencopybook from './opencopybook';

export function activate(context: ExtensionContext) {
    var move2pdCommand = commands.registerCommand('cobolplugin.move2pd', function () {
        cobolProgram.move2pd();
    });

    var move2ddCommand = commands.registerCommand('cobolplugin.move2dd', function () {
        cobolProgram.move2dd();
    })

    var move2wsCommand = commands.registerCommand('cobolplugin.move2ws', function () {
        cobolProgram.move2ws();
    })

    var move2anyforwardCommand = commands.registerCommand('cobolplugin.move2anyforward', function () {
        cobolProgram.move2anyforward();
    })

    var move2anybackwardsCommand = commands.registerCommand('cobolplugin.move2anybackwards', function () {
        cobolProgram.move2anybackwards();
    })

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

    const allCobolSelectors = ["COBOL", "ACUCOBOL", "OpenCOBOL"];    
    languages.registerDefinitionProvider(allCobolSelectors, {
        provideDefinition(document: TextDocument, position: Position, token: CancellationToken): ProviderResult<Definition> {
            return opencopybook.provideDefinition(document, position, token);
        }
    });
}

export function deactivate() {
}
