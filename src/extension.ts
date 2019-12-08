'use strict';

import { commands, workspace, StatusBarItem, StatusBarAlignment, DecorationOptions, Range, ExtensionContext, languages, TextDocument, TextEditor, Position, CancellationToken, ProviderResult, Definition, window, Hover, OutputChannel, extensions, Disposable } from 'vscode';
import * as cobolProgram from './cobolprogram';
import * as tabstopper from './tabstopper';
import * as opencopybook from './opencopybook';
import { DocComment } from './formatting/DocComment';
import { TextAutocompleteCompletionItemProvider } from './textprovider';
import { ESourceFormat, enableMarginCobolMargin, isEnabledViaWorkspace4cobol } from './margindecorations';

import { jclStatements } from "./keywords/jclstatements";
import { CobolDocumentSymbolProvider, JCLDocumentSymbolProvider } from './symbolprovider';
import * as sourcedefinitionprovider from './sourcedefinitionprovider';
import * as path from 'path';
import * as fs from 'fs';

import updateDecorations from './margindecorations';
import { getCallTarget } from './keywords/cobolCallTargets';
import COBOLQuickParse, { InMemoryGlobalCachesHelper, COBOLSymbolTableHelper } from './cobolquickparse';
import { isDirectPath } from './opencopybook';

import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient';
import VSQuickCOBOLParse from './vscobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import { CobolReferenceProvider } from './cobolreferenceprovider';
import { CobolUsageProvider } from './cobolusageprovider';

const util = require('util');

let formatStatusBarItem: StatusBarItem;

var currentContext: ExtensionContext;
const COBOLOutputChannel: OutputChannel = window.createOutputChannel("COBOL");


export function getcopybookdirs(): string[] {
    return fileSearchDirectory;
}


let fileSearchDirectory: string[] = [];
let invalidSearchDirectory: string[] = [];

function initExtensions() {
    fileSearchDirectory = [];
    var extsdir = VSCOBOLConfiguration.getCopybookdirs_defaults();

    if (workspace.workspaceFolders) {
        for (var folder of workspace.workspaceFolders) {
            for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                try {
                    var extdir = extsdir[extsdirpos];

                    let sdir: string;
                    if (isDirectPath(extdir)) {
                        sdir = extdir;
                    } else {
                        sdir = path.join(folder.uri.fsPath, extdir);
                    }
                    if (fs.existsSync(sdir)) {
                        let f = fs.statSync(sdir);
                        if (f.isDirectory()) {
                            fileSearchDirectory.push(sdir);
                        } else {
                            invalidSearchDirectory.push(sdir);
                        }
                    } else {
                        invalidSearchDirectory.push(sdir);
                    }
                }
                catch (e) {
                    logException("dir", e);
                }
            }
        }
    }

    fileSearchDirectory = fileSearchDirectory.filter((elem, pos) => fileSearchDirectory.indexOf(elem) === pos);
    invalidSearchDirectory = invalidSearchDirectory.filter((elem, pos) => invalidSearchDirectory.indexOf(elem) === pos);
}

export function showLogChannel() {
    COBOLOutputChannel.show();
}

function activateLogChannel() {
    initExtensions();
    let thisExtension = extensions.getExtension("bitlang.cobol");
    logMessage("");
    COBOLOutputChannel.clear();
    if (thisExtension !== undefined) {
        logMessage("Extension Information:");
        logMessage(" Extension path    : " + thisExtension.extensionPath);
        logMessage(" Version           : " + thisExtension.packageJSON.version);
        logMessage(" Caching           : " + VSCOBOLConfiguration.getCachingSetting());
        if (VSCOBOLConfiguration.isCachingEnabled()) {
            logMessage("  Cache directory  : " + VSQuickCOBOLParse.getCacheDirectory());
        }

        var extsdir = fileSearchDirectory;
        for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
            let sdir = extsdir[extsdirpos];
            logMessage("  Search directory : " + sdir);
        }

        extsdir = invalidSearchDirectory;
        for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
            let sdir = extsdir[extsdirpos];
            logMessage("  Invalid Search directory : " + sdir);
        }


        logMessage("");
    }
}

export function getCurrentContext(): ExtensionContext {
    return currentContext;
}

function activateLanguageServer(context: ExtensionContext) {
    const serverModule = context.asAbsolutePath(path.join('out', 'server', 'src', 'server.js'));
    const serverOptions: ServerOptions = {
        run: {
            module: serverModule,
            transport: TransportKind.ipc,
        },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: {
                execArgv: ['--nolazy', '--inspect=6009'],
            },
        },
    };

    const clientOptions: LanguageClientOptions = {

        documentSelector: [{
            scheme: 'file',
            language: 'COBOL',
        }],
        synchronize: {
            // Synchronize the setting section 'coboleditor' to the server
            configurationSection: 'coboleditor',

            // Notify the server about file changes to '.clientrc files contain in the workspace
            fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        }
    };

    const client = new LanguageClient('COBOL', 'Language Server for COBOL', serverOptions, clientOptions);
    const disposable = client.start();

    context.subscriptions.push(disposable);
}

export function activate(context: ExtensionContext) {
    currentContext = context;
    VSCOBOLConfiguration.init();

    const collection = languages.createDiagnosticCollection('cobolDiag');
    const cobolusage = new CobolUsageProvider(collection);
    initExtensions();

    // if (VSCOBOLConfiguration.get().experimential_features) {
    //     activateLanguageServer(context);
    // }

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
        if (VSCOBOLConfiguration.isTabstopEnabled()) {
            tabstopper.processTabKey(true);
        } else {
            commands.executeCommand("tab");
        }

    });

    var unTabCommand = commands.registerCommand('cobolplugin.revtab', function () {
        if (VSCOBOLConfiguration.isTabstopEnabled()) {
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
        VSQuickCOBOLParse.processAllFilesInWorkspaces();
    });

    var dumpMetadata = commands.registerCommand('cobolplugin.dumpMetaData', function () {
        COBOLQuickParse.dumpMetaData(VSCOBOLConfiguration.get(), VSQuickCOBOLParse.getCacheDirectory());
    });


    const onDidChangeConfiguration = workspace.onDidChangeConfiguration(() => {
        COBOLOutputChannel.appendLine('Configuration changed... Refreshing...');
        VSCOBOLConfiguration.init();
        initExtensions();
        COBOLOutputChannel.appendLine('Refresh done!');
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

    context.subscriptions.push(languages.registerReferenceProvider(allCobolSelectors, new CobolReferenceProvider()));

    const jclSelectors = [
        { scheme: 'file', language: 'JCL' }
    ];
    const completionJCLItemProvider = new TextAutocompleteCompletionItemProvider(jclStatements);
    const completionJCLItemProviderDisposable = languages.registerCompletionItemProvider(jclSelectors, completionJCLItemProvider);
    context.subscriptions.push(completionJCLItemProviderDisposable);

    if (VSCOBOLConfiguration.isOutlineEnabled()) {
        const jclDocumentSymbolProvider = new JCLDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(jclSelectors, jclDocumentSymbolProvider));

        /* TODO: add .DIR keywords too */
        const documentSymbolProvider = new CobolDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(allCobolSelectors, documentSymbolProvider));
    }

    /* hover provider */
    if (VSCOBOLConfiguration.getExperimentialFeatures()) {
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
        cobolusage.updateDiagnostics(editor.document);

    }, null, context.subscriptions);

    window.onDidChangeTextEditorSelection(event => {
        if (!event.textEditor) {
            return;
        }
        updateDecorations(event.textEditor);
        cobolusage.updateDiagnostics(event.textEditor.document);
    }, null, context.subscriptions);

    workspace.onDidChangeTextDocument(event => {
        if (!window.activeTextEditor) {
            return;
        }
        updateDecorations(window.activeTextEditor);
        cobolusage.updateDiagnostics(window.activeTextEditor.document);
    }, null, context.subscriptions);

    formatStatusBarItem = window.createStatusBarItem(StatusBarAlignment.Right);
    formatStatusBarItem.command = "cobolplugin.change_source_format";
    formatStatusBarItem.show();

    if (window.activeTextEditor !== undefined) {
        updateDecorations(window.activeTextEditor);
        cobolusage.updateDiagnostics(window.activeTextEditor.document);
    }

    activateLogChannel();

    if (VSCOBOLConfiguration.isCachingSetToON()) {
        let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
        InMemoryGlobalCachesHelper.loadInMemoryGlobalSymbolCaches(cacheDirectory);
        InMemoryGlobalCachesHelper.loadInMemoryGlobalFileCache(cacheDirectory);
        commands.executeCommand("cobolplugin.processAllFilesInWorkspace");
    }
}

export function enableMarginStatusBar(formatStyle: ESourceFormat) {
    formatStatusBarItem.text = "Source:" + formatStyle;
    formatStyle.toString();
    formatStatusBarItem.show();
}


export function hideMarginStatusBar() {
    formatStatusBarItem.hide();
}

export async function deactivateAsync() {
    if (VSCOBOLConfiguration.isCachingEnabled()) {
        InMemoryGlobalCachesHelper.saveInMemoryGlobalCaches(VSQuickCOBOLParse.getCacheDirectory());
    }
    formatStatusBarItem.dispose();
}

export async function deactivate(): Promise<void> {
    await deactivateAsync();
}

export function logException(message: string, ex: Error) {
    logMessage(ex.name + ":" + message);
    if (ex !== undefined && ex.stack !== undefined) {
        logMessage(ex.stack);
    }
}

let thresholdTime: number = 30;

export function logTimedMessage(timeTaken: number, message: string, ...parameters: any[]) {
    let fixedTimeTaken = " (" + timeTaken.toFixed(2) + "ms)";

    if (timeTaken < thresholdTime) {
        return;
    }

    if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
        let m: string = util.format(message, parameters);
        COBOLOutputChannel.appendLine(m.padEnd(60) + fixedTimeTaken);
        //console.log(util.format(message, parameters) + "\n");
        return;
    }

    COBOLOutputChannel.appendLine(message.padEnd(60) + fixedTimeTaken);
    //console.log(message + "\n");
}


export function logMessage(message: string, ...parameters: any[]) {
    if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
        COBOLOutputChannel.appendLine(util.format(message, parameters));
        //console.log(util.format(message, parameters) + "\n");
        return;
    }

    COBOLOutputChannel.appendLine(message);
    //console.log(message + "\n");
}



