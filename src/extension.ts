'use strict';

import { commands, workspace, StatusBarItem, StatusBarAlignment, DecorationOptions, Range, ExtensionContext, languages, TextDocument, TextEditor, Position, CancellationToken, ProviderResult, Definition, window, Hover, OutputChannel, extensions } from 'vscode';
import * as cobolProgram from './cobolprogram';
import * as tabstopper from './tabstopper';
import * as opencopybook from './opencopybook';
import { DocComment } from './formatting/DocComment';
import { TextAutocompleteCompletionItemProvider } from './textprovider';
import { ESourceFormat, enableMarginCobolMargin, isEnabledViaWorkspace4cobol } from './margindecorations';

import { jclStatements } from "./keywords/jclstatements";
import { CobolDocumentSymbolProvider, JCLDocumentSymbolProvider } from './symbolprovider';
import * as sourcedefinitionprovider from './sourcedefinitionprovider';

import updateDecorations from './margindecorations';
import { getCallTarget } from './keywords/cobolCallTargets';
import QuickCOBOLParse, { InMemoryGlobalCachesHelper, COBOLSymbolTableHelper } from './cobolquickparse';

const util = require('util');

let formatStatusBarItem: StatusBarItem;

var currentContext: ExtensionContext;
var COBOLOutputChannel: OutputChannel;

let logChannelDisabled:boolean = isLogChannelDisabled();

export function activateLogChannel(show: boolean) {
    logChannelDisabled = false;
    let thisExtension = extensions.getExtension("bitlang.cobol");
    logCOBOLChannelLine("");
    COBOLOutputChannel.clear();
    if (thisExtension !== undefined) {
        logCOBOLChannelLine("Extension Information:");
        logCOBOLChannelLine(" Extension path    : " + thisExtension.extensionPath);
        logCOBOLChannelLine(" Version           : " + thisExtension.packageJSON.version);
        logCOBOLChannelLine(" Caching           : "+getCachingSetting());
        if (isCachingEnabled()) {
            logCOBOLChannelLine("  Cache directory  : "+COBOLSymbolTableHelper.getCacheDirectory());
        }
        logCOBOLChannelLine("");
    }

    if (show) {
        COBOLOutputChannel.show();
    }

}

export function getCurrentContext(): ExtensionContext {
    return currentContext;
}

export function activate(context: ExtensionContext) {
    currentContext = context;

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
        if (isTabstopEnabled()) {
            tabstopper.processTabKey(true);
        } else {
            commands.executeCommand("tab");
        }

    });

    var unTabCommand = commands.registerCommand('cobolplugin.revtab', function () {
        if (isTabstopEnabled()) {
            tabstopper.processTabKey(false);
        } else {
            commands.executeCommand("outdent");
        }
    });

    var changeSourceFormat = commands.registerCommand('cobolplugin.change_source_format', function () {
        // margindecorations.changeSourceFormat();
        const action = 'Open Settings';
        window.showWarningMessage("Change coboleditor setting?", action).then((selection) => {
            if (action === selection) {
                let ws = workspace.getWorkspaceFolder;
                if (ws === undefined || ws === null) {
                    commands.executeCommand("workbench.action.openGlobalSettings");
                }
                else {
                    commands.executeCommand("workbench.action.openWorkspaceSettings");
                }
            }
        });
    });

    var changeLanguageToAcu = commands.registerCommand('cobolplugin.change_lang_to_acu', function () {
        let act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "ACUCOBOL");
    });

    var changeLanguageToCOBOL = commands.registerCommand('cobolplugin.change_lang_to_cobol', function () {
        let act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "COBOL");
    });

    var changeLanguageToOpenCOBOL = commands.registerCommand('cobolplugin.change_lang_to_opencobol', function () {
        let act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "OpenCOBOL");
    });

    var changeLanguageToGnuCOBOL = commands.registerCommand('cobolplugin.change_lang_to_gnucobol', function () {
        let act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "GnuCOBOL");
    });

    var toggleCOBOLMargin = commands.registerCommand('cobolplugin.toggle_margin', function () {
        let act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        enableMarginCobolMargin(!isEnabledViaWorkspace4cobol());
        updateDecorations(act);
    });


    var processAllFilesInWorkspace = commands.registerCommand('cobolplugin.processAllFilesInWorkspace', function () {
        QuickCOBOLParse.processAllFilesInWorkspaces(false);
    });

    var processAllFilesInWorkspaceAndSubDirs = commands.registerCommand('cobolplugin.processAllFilesInWorkspaceAndSubdirs', function () {
        QuickCOBOLParse.processAllFilesInWorkspaces(true);
    });

    var dumpMetadata = commands.registerCommand('cobolplugin.dumpMetaData', function () {
        QuickCOBOLParse.dumpMetaData();
    });

    context.subscriptions.push(move2pdCommand);
    context.subscriptions.push(move2ddCommand);
    context.subscriptions.push(move2wsCommand);
    context.subscriptions.push(move2anyforwardCommand);
    context.subscriptions.push(move2anybackwardsCommand);
    context.subscriptions.push(tabCommand);
    context.subscriptions.push(unTabCommand);
    context.subscriptions.push(changeSourceFormat);

    context.subscriptions.push(changeLanguageToAcu);
    context.subscriptions.push(changeLanguageToCOBOL);
    context.subscriptions.push(changeLanguageToOpenCOBOL);
    context.subscriptions.push(changeLanguageToGnuCOBOL);

    context.subscriptions.push(toggleCOBOLMargin);

    context.subscriptions.push(processAllFilesInWorkspace);
    context.subscriptions.push(processAllFilesInWorkspaceAndSubDirs);
    context.subscriptions.push(dumpMetadata);

    context.subscriptions.push(DocComment.register());

    const allCobolSelectors = [
        { scheme: 'file', language: 'COBOL' },
        { scheme: 'file', language: 'ACUCOBOL' },
        { scheme: 'file', language: 'OpenCOBOL' },
        { scheme: 'file', language: 'GnuCOBOL' }
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


    // const completionItemProvider = new TextAutocompleteCompletionItemProvider(cobolKeywords);
    // const completionItemProviderDisposable = languages.registerCompletionItemProvider(allCobolSelectors, completionItemProvider);
    // context.subscriptions.push(completionItemProviderDisposable);


    const jclSelectors = [
        { scheme: 'file', language: 'JCL' }
    ];
    const completionJCLItemProvider = new TextAutocompleteCompletionItemProvider(jclStatements);
    const completionJCLItemProviderDisposable = languages.registerCompletionItemProvider(jclSelectors, completionJCLItemProvider);
    context.subscriptions.push(completionJCLItemProviderDisposable);

    if (isOutlineEnabled()) {
        const jclDocumentSymbolProvider = new JCLDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(jclSelectors, jclDocumentSymbolProvider));

        /* TODO: add .DIR keywords too */
        const documentSymbolProvider = new CobolDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(allCobolSelectors, documentSymbolProvider));
    }

    /* hover provider */
    if (getExperimentialFeatures()) {
        let disposable = languages.registerHoverProvider(allCobolSelectors, {
            provideHover(document, position, token) {

                // window.showInformationMessage(position.toString());
                let txt = document.getText(document.getWordRangeAtPosition(position));
                let txtTarger = getCallTarget(txt);
                if (txtTarger !== null) {
                    return new Hover("### " + txtTarger.api + "\n" + txtTarger.description + "\n\n#### [More information?](" + txtTarger.url + ")");
                }
            }
        });
        context.subscriptions.push(disposable);
    }

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

    if (logChannelDisabled === false) {
        activateLogChannel(false);
    }

    if (isCachingSetToON()) {
        InMemoryGlobalCachesHelper.loadInMemoryGlobalSymbolCaches();
        commands.executeCommand("cobolplugin.processAllFilesInWorkspace");
    }
}

export function isLogChannelDisabled(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var diagnosticOn = editorConfig.get<boolean>('diagnostic');
    if (diagnosticOn === undefined || diagnosticOn === null) {
        return false;
    }
    return !diagnosticOn;
}


export function getExperimentialFeatures(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var expEnabled = editorConfig.get<boolean>('experimential.features');
    if (expEnabled === undefined || expEnabled === null) {
        expEnabled = false;
    }
    return expEnabled;
}

export function isTabstopEnabled(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var expEnabled = editorConfig.get<boolean>('enable_tabstop');
    if (expEnabled === undefined || expEnabled === null) {
        expEnabled = false;
    }
    return expEnabled;
}

export enum outlineFlag {
    On = "on",
    Off = "off",
    Partial = "partial",
    Skeleton = "skeleton"
}

export function isOutlineEnabled(): outlineFlag {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var outlineEnabled = editorConfig.get('outline');
    if (outlineEnabled === undefined || outlineEnabled === null) {
        return outlineFlag.On;
    }

    switch (outlineEnabled) {
        case "on": return outlineFlag.On;
        case "off": return outlineFlag.Off;
        case "partial": return outlineFlag.Partial;
        case "skeleton": return outlineFlag.Skeleton;
    }
    return outlineFlag.On;
}

export function enableMarginStatusBar(formatStyle: ESourceFormat) {
    formatStatusBarItem.text = "Source:" + formatStyle;
    formatStyle.toString();
    formatStatusBarItem.show();
}

export function hideMarginStatusBar() {
    formatStatusBarItem.hide();
}

export function deactivate() {
    if (isCachingEnabled()) {
        InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches();
    }
    formatStatusBarItem.dispose();
}

export function logCOBOLChannelLineException(message: string, ex: Error) {
    logCOBOLChannelLine(ex.name+ ":" + message);
    if (ex !== undefined && ex.stack !== undefined) {
        logCOBOLChannelLine(ex.stack);
    }
}

export function logCOBOLChannelLine(message: string, ...parameters: any[]) {
    if (logChannelDisabled) {
        return;
    }

    if (COBOLOutputChannel === null || COBOLOutputChannel === undefined) {
        COBOLOutputChannel = window.createOutputChannel("COBOL");
    }

    if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
        COBOLOutputChannel.appendLine(util.format(message, parameters));
        console.log(util.format(message, parameters) + "\n");
        return;
    }

    COBOLOutputChannel.appendLine(message);
    console.log(message + "\n");
}

export function getFuzzyVariableSearch(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var fuzzyVarOn = editorConfig.get<boolean>('fuzzy_variable_search');
    if (fuzzyVarOn === undefined || fuzzyVarOn === null) {
        fuzzyVarOn = false;
    }
    return fuzzyVarOn;
}

export function getCachingSetting() : string {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var cacheEnum = editorConfig.get<string>('cache_metadata');

    if (cacheEnum === undefined || cacheEnum === null) {
        return "";
    }

    return cacheEnum;
}

export function isCachingEnabled(): boolean {
    var cacheEnum = getCachingSetting();

    switch(cacheEnum) {
        case "on" : return true;
        case "partial": return true;
        case "off" : return false;
    }
    return false;
}

export function isCachingSetToON(): boolean {
    var cacheEnum = getCachingSetting();
    switch(cacheEnum) {
        case "on" : return true;
        case "partial": return false;
        case "off" : return false;
    }
    return false;
}
