'use strict';

import { commands, workspace, StatusBarItem, StatusBarAlignment, DecorationOptions, Range, ExtensionContext, languages, TextDocument, TextEditor, Position, CancellationToken, ProviderResult, Definition, window, Hover, OutputChannel, extensions, Disposable, Uri, tasks, Task, ShellExecution, TextEditorSelectionChangeKind } from 'vscode';
import * as cobolProgram from './cobolprogram';
import * as tabstopper from './tabstopper';
import * as opencopybook from './opencopybook';
import * as commenter from './commenter';
import { DocComment } from './formatting/DocComment';
import { KeywordAutocompleteCompletionItemProvider } from './keywordprovider';
import { ESourceFormat, enableMarginCobolMargin, isEnabledViaWorkspace4cobol } from './margindecorations';

import { jclStatements } from "./keywords/jclstatements";
import { cobolKeywords } from "./keywords/cobolKeywords";

import { CobolDocumentSymbolProvider, JCLDocumentSymbolProvider } from './symbolprovider';
import * as sourcedefinitionprovider from './sourcedefinitionprovider';
import * as path from 'path';
import * as fs from 'fs';

import updateDecorations from './margindecorations';
import { getCallTarget, CallTarget } from './keywords/cobolCallTargets';
import COBOLQuickParse, { InMemoryGlobalCachesHelper, COBOLSymbolTableHelper } from './cobolquickparse';
import { isDirectPath } from './opencopybook';

import VSQuickCOBOLParse from './vscobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import { CobolReferenceProvider } from './cobolreferenceprovider';
import { CobolLinterProvider, CobolLinterActionFixer } from './cobollinter';
import { SourceViewTree } from './sourceViewTree';
import { GnuCOBCTaskDefinition, getTaskForCOBC, getCOBOLTasks_for_cobc, MFCOBOLTaskDefinition, getCOBOLTasks_for_cobol, getTaskForCOBOL } from './taskdefs';
import { CobolSourceCompletionItemProvider } from './cobolprovider';

const util = require('util');
var which = require('which');

let formatStatusBarItem: StatusBarItem;

var currentContext: ExtensionContext;
const COBOLOutputChannel: OutputChannel = window.createOutputChannel("COBOL");


export function getcopybookdirs(): string[] {
    return fileSearchDirectory;
}

export function getLogicalCopybookdirs(prefix: string, suffix:string): string {
    let copyBookLine: string = "";
    var extsdir = VSCOBOLConfiguration.getCopybookdirs_defaults();

    if (workspace.workspaceFolders) {
        for (var folder of workspace.workspaceFolders) {
            for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                try {
                    var extdir = extsdir[extsdirpos];

                    let sdir: string;
                    if (isDirectPath(extdir)) {
                        sdir = extdir;
                        if (fs.existsSync(sdir)) {
                            let f = fs.statSync(sdir);
                            if (f.isDirectory()) {
                                copyBookLine += prefix+sdir+suffix;
                            }
                        }
                    } else {
                        sdir = path.join(folder.uri.fsPath, extdir);
                        if (fs.existsSync(sdir)) {
                            let f = fs.statSync(sdir);
                            if (f.isDirectory()) {
                                copyBookLine += prefix+"${workspaceFolder}/"+extdir+suffix;
                            }
                        }
                    }
                }
                catch (e) {
                    logException("dir", e);
                }
            }
        }
    }

    return copyBookLine;
}

let fileSearchDirectory: string[] = [];
let invalidSearchDirectory: string[] = VSCOBOLConfiguration.getInvalid_copybookdirs();

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
    if (thisExtension !== undefined) {
        logMessage("Extension Information:");
        logMessage(" Extension path    : " + thisExtension.extensionPath);
        logMessage(" Version           : " + thisExtension.packageJSON.version);
        if (VSCOBOLConfiguration.isCachingEnabled()) {
            logMessage(" Caching           : " + VSCOBOLConfiguration.getCachingSetting());
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

export function activate(context: ExtensionContext) {
    currentContext = context;
    VSCOBOLConfiguration.init();

    const collection = languages.createDiagnosticCollection('cobolDiag');
    const linter = new CobolLinterProvider(collection, VSCOBOLConfiguration.get());
    const cobolfixer = new CobolLinterActionFixer();
    initExtensions();
    activateLogChannel();


    // if (VSCOBOLConfiguration.get().experimential_features) {
    //     activateLanguageServer(context);
    // }

    var insertIgnoreCommentLineCommand = commands.registerCommand("cobolplugin.insertIgnoreCommentLine", function (docUri: Uri, offset: number, code: string) {
        cobolfixer.insertIgnoreCommentLine(docUri, offset, code);
    });
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

    var commentLine = commands.registerCommand('cobolplugin.commentline', function () {
        if (VSCOBOLConfiguration.isLineCommentEnabled()) {
            commenter.processCommentLine();
        } else {
            commands.executeCommand("editor.action.commentLine");
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


    var syntaxCheck = commands.registerCommand('cobolplugin.syntaxCheck', function () {
        tasks.fetchTasks().then((fetchedTasks) => {
            for (const task of fetchedTasks) {
                if (task.name.startsWith("cobol-syntax-check")) {
                    tasks.executeTask(task);
                }
            }
        }, (reason) => {
            console.log('fetch task was rejected', reason);
        });
    });


    const onDidChangeConfiguration = workspace.onDidChangeConfiguration(() => {
        VSCOBOLConfiguration.init();
        initExtensions();
    });

    var treeView = new SourceViewTree(VSCOBOLConfiguration.get());
    const watcher = workspace.createFileSystemWatcher('**/*');

    watcher.onDidCreate((uri) => {
        treeView.checkFile(uri);
    });

    watcher.onDidDelete((uri) => {
        treeView.clearFile(uri);
    });



    let cobcLocation = which.sync('cobc', { nothrow: true });

    if (cobcLocation !== null) {
        let cobolTaskPromise4cobc: Thenable<Task[]> | undefined = undefined;

        const taskProvider4cobc = tasks.registerTaskProvider('cobc', {
            provideTasks: () => {
                if (!cobolTaskPromise4cobc) {
                    cobolTaskPromise4cobc = getCOBOLTasks_for_cobc("cobol_syntax_check_with_cobc", true);
                }
                return cobolTaskPromise4cobc;
            },
            resolveTask(task: Task): Task | undefined {
                if (task) {
                    const definition: GnuCOBCTaskDefinition = <any>task.definition;
                    if (definition.extraArguments || definition.syntaxCheck) {
                        return getTaskForCOBC(definition, definition.label, definition.syntaxCheck);
                    }
                    return task;
                }
                return undefined;
            }

        });

        context.subscriptions.push(taskProvider4cobc);
    }


    let cobolLocation = which.sync('cobol.exe', { nothrow: true });
    if (cobolLocation !== null) {
        let cobolTaskPromise4cobol: Thenable<Task[]> | undefined = undefined;

        const taskProvider4cobc = tasks.registerTaskProvider('cobc', {
            provideTasks: () => {
                if (!cobolTaskPromise4cobol) {
                    cobolTaskPromise4cobol = getCOBOLTasks_for_cobol("cobol_syntax_check_with_mfcobol", true);
                }
                return cobolTaskPromise4cobol;
            },
            resolveTask(task: Task): Task | undefined {
                if (task) {
                    const definition: MFCOBOLTaskDefinition = <any>task.definition;
                    if (definition.extraArguments || definition.syntaxCheck) {
                        return getTaskForCOBOL(definition, definition.label, definition.syntaxCheck);
                    }
                    return task;
                }
                return undefined;
            }

        });

        context.subscriptions.push(taskProvider4cobc);
    }

    window.registerTreeDataProvider('flat-source-view', treeView);

    context.subscriptions.push(move2pdCommand);
    context.subscriptions.push(move2ddCommand);
    context.subscriptions.push(move2wsCommand);
    context.subscriptions.push(move2anyforwardCommand);
    context.subscriptions.push(move2anybackwardsCommand);
    context.subscriptions.push(tabCommand);
    context.subscriptions.push(unTabCommand);
    context.subscriptions.push(commentLine);

    context.subscriptions.push(changeSourceFormat);

    context.subscriptions.push(changeLanguageToAcu);
    context.subscriptions.push(changeLanguageToCOBOL);
    context.subscriptions.push(changeLanguageToOpenCOBOL);
    context.subscriptions.push(changeLanguageToGnuCOBOL);

    context.subscriptions.push(toggleCOBOLMargin);

    context.subscriptions.push(processAllFilesInWorkspace);
    context.subscriptions.push(dumpMetadata);

    context.subscriptions.push(DocComment.register());

    context.subscriptions.push(insertIgnoreCommentLineCommand);

    context.subscriptions.push(syntaxCheck);


    const allCobolSelectors = [
        { scheme: 'file', language: 'COBOL' },
        { scheme: 'file', language: 'ACUCOBOL' },
        { scheme: 'file', language: 'OpenCOBOL' },
        { scheme: 'file', language: 'GnuCOBOL' },
        { scheme: 'file', language: 'entcobol' }
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
    context.subscriptions.push(languages.registerCodeActionsProvider(allCobolSelectors, cobolfixer));

    const jclSelectors = [
        { scheme: 'file', language: 'JCL' }
    ];
    const completionJCLItemProvider = new KeywordAutocompleteCompletionItemProvider(jclStatements);
    const completionJCLItemProviderDisposable = languages.registerCompletionItemProvider(jclSelectors, completionJCLItemProvider);
    context.subscriptions.push(completionJCLItemProviderDisposable);

    const keywordProvider = new KeywordAutocompleteCompletionItemProvider(cobolKeywords);
    const keywordProviderDisposible = languages.registerCompletionItemProvider(allCobolSelectors, keywordProvider);
    context.subscriptions.push(keywordProviderDisposible);

    if (VSCOBOLConfiguration.isOutlineEnabled()) {
        const jclDocumentSymbolProvider = new JCLDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(jclSelectors, jclDocumentSymbolProvider));

        /* TODO: add .DIR keywords too */
        const documentSymbolProvider = new CobolDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(allCobolSelectors, documentSymbolProvider));
    }

    /* hover provider */
    const cobolProvider = new CobolSourceCompletionItemProvider(VSCOBOLConfiguration.get());
    const cobolProviderDisposible = languages.registerCompletionItemProvider(allCobolSelectors, cobolProvider);
    context.subscriptions.push(cobolProviderDisposible);

    let disposable = languages.registerHoverProvider(allCobolSelectors, {
        provideHover(document, position, token) {
            let txt = document.getText(document.getWordRangeAtPosition(position));
            let txtTarger:CallTarget|undefined = getCallTarget(txt);
            if (txtTarger !== undefined) {
                return new Hover("### " + txtTarger.api + "\n" + txtTarger.description + "\n\n#### [More information?](" + txtTarger.url + ")");
            }
        }
    });
    context.subscriptions.push(disposable);


    window.onDidChangeActiveTextEditor(editor => {
        if (!editor) {
            return;
        }
        updateDecorations(editor);
        linter.updateDiagnostics(editor.document);

    }, null, context.subscriptions);

    window.onDidChangeTextEditorSelection(event => {
        if (!event.textEditor) {
            return;
        }
        updateDecorations(event.textEditor);
        //cobolusage.updateDiagnostics(event.textEditor.document);
    }, null, context.subscriptions);

    workspace.onDidChangeTextDocument(event => {
        if (!window.activeTextEditor) {
            return;
        }
        updateDecorations(window.activeTextEditor);
        linter.updateDiagnostics(window.activeTextEditor.document);
    }, null, context.subscriptions);

    formatStatusBarItem = window.createStatusBarItem(StatusBarAlignment.Right);
    formatStatusBarItem.command = "cobolplugin.change_source_format";
    formatStatusBarItem.show();

    if (window.activeTextEditor !== undefined) {
        updateDecorations(window.activeTextEditor);
        linter.updateDiagnostics(window.activeTextEditor.document);
    }

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
