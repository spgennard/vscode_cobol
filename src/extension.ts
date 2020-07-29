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
import * as vscode from "vscode";

import updateDecorations from './margindecorations';
import { getCallTarget, CallTarget } from './keywords/cobolCallTargets';
import COBOLQuickParse from './cobolquickparse';
import { InMemoryGlobalCachesHelper } from "./imemorycache";
import { isDirectPath, isNetworkPath } from './opencopybook';

import VSQuickCOBOLParse from './vscobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import { CobolReferenceProvider } from './cobolreferenceprovider';
import { CobolLinterProvider, CobolLinterActionFixer } from './cobollinter';
import { SourceViewTree } from './sourceviewtree';
import { GnuCOBCTaskDefinition, getTaskForCOBC, getCOBOLTasks_for_cobc, MFCOBOLTaskDefinition, getCOBOLTasks_for_mfcobol, getTaskForCOBOL } from './taskdefs';
import { CobolSourceCompletionItemProvider } from './cobolprovider';
import { COBOLUtils, FoldStyle, FoldAction } from './cobolutils';
import { ICOBOLSettings } from './iconfiguration';

const propertiesReader = require('properties-reader');

import util from 'util';
import { getWorkspaceFolders } from './cobolfolders';
var which = require('which');

let formatStatusBarItem: StatusBarItem;

var currentContext: ExtensionContext;
const COBOLOutputChannel: OutputChannel = window.createOutputChannel("COBOL");


export function getcopybookdirs(): string[] {
    return fileSearchDirectory;
}

export function isDirectory(sdir: string): boolean {
    try {
        let f = fs.statSync(sdir);
        if (f.isDirectory()) {
            return true;
        }
    }
    catch {

    }
    return false;
}

export function isFile(sdir: string): boolean {
    try {
        let f = fs.statSync(sdir);
        if (f.isFile()) {
            return true;
        }
    }
    catch {

    }
    return false;
}

export function getLogicalCopybookdirs(prefix: string, suffix: string): string {
    let copyBookLine: string = "";
    var extsdir = VSCOBOLConfiguration.getCopybookdirs_defaults();

    for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
        var extdir_direct = extsdir[extsdirpos];
        if (isDirectPath(extdir_direct)) {
            let sdir: string;
            sdir = extdir_direct;
            if (isDirectory(sdir)) {
                copyBookLine += prefix + sdir + suffix;
            }
        }
    }

    let ws = getWorkspaceFolders();
    if (ws !== undefined) {
        for (var folder of ws) {
            for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                try {
                    var extdir = extsdir[extsdirpos];

                    let sdir: string;
                    if (isDirectPath(extdir) === false) {
                        sdir = path.join(folder.uri.fsPath, extdir);
                        if (isDirectory(sdir)) {
                            copyBookLine += prefix + "${workspaceFolder}/" + extdir + suffix;
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
let invalidSearchDirectory: string[] = [];
let unitTestTerminal: vscode.Terminal | undefined = undefined;
let terminalName = "UnitTest";


export function isPathInWorkspace(ddir: string): boolean {
    let ws = getWorkspaceFolders();
    if (workspace === undefined || ws === undefined) {
        return false;
    }

    let fullPath = path.normalize(ddir);
    for (var folder of ws) {
        if (folder.uri.fsPath === fullPath) {
            return true;
        }
    }

    return false;
}

function initExtensions(config: ICOBOLSettings) {
    fileSearchDirectory = [];

    var extsdir = VSCOBOLConfiguration.getCopybookdirs_defaults();
    invalidSearchDirectory = VSCOBOLConfiguration.getInvalid_copybookdirs();
    invalidSearchDirectory.length = 0;

    for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
        var ddir = extsdir[extsdirpos];
        if (config.disable_unc_copybooks_directories && isNetworkPath(ddir)) {
            logMessage(" Copybook directory " + ddir + " has been marked as invalid, as it is a unc filename");
            invalidSearchDirectory.push(ddir);
        }
        else if (isDirectPath(ddir)) {
            if (workspace !== undefined && getWorkspaceFolders() !== undefined) {
                if (isPathInWorkspace(ddir) === false) {
                    if (isNetworkPath(ddir)) {
                        logMessage(" The directory " + ddir + " for performance should be part of the workspace");
                    }
                }
            }

            let startTime = performance_now();
            if (isDirectory(ddir)) {
                let totalTimeInMS = performance_now() - startTime;
                let timeTaken = totalTimeInMS.toFixed(2);
                if (totalTimeInMS <= 2000) {
                    fileSearchDirectory.push(ddir);
                } else {
                    logMessage(" Slow copybook directory dropped " + ddir + " as it took " + timeTaken + "ms");
                }
            } else {
                invalidSearchDirectory.push(ddir);
            }
        }
    }

    let ws = getWorkspaceFolders();
    if (ws !== undefined) {
        for (var folder of ws) {
            for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                try {
                    var extdir = extsdir[extsdirpos];

                    let sdir: string;

                    if (isDirectPath(extdir) === false) {
                        sdir = path.join(folder.uri.fsPath, extdir);

                        if (isDirectory(sdir)) {
                            if (isNetworkPath(sdir) && isPathInWorkspace(sdir) === false) {
                                logMessage(" The directory " + sdir + " for performance should be part of the workspace");
                            }

                            fileSearchDirectory.push(sdir);
                        } else {
                            invalidSearchDirectory.push(sdir);
                        }
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
    COBOLOutputChannel.show(true);
}

function activateLogChannel() {

    let thisExtension = extensions.getExtension("bitlang.cobol");
    logMessage("");
    if (thisExtension !== undefined) {
        logMessage("Extension Information:");
        logMessage(" Extension path    : " + thisExtension.extensionPath);
        logMessage(" Version           : " + thisExtension.packageJSON.version);
        if (VSCOBOLConfiguration.isCachingEnabled()) {
            logMessage(" Caching                       : " + VSCOBOLConfiguration.getCachingSetting());
            logMessage("  Cache Strategy               : " + VSCOBOLConfiguration.getCache_directory_strategy());
            logMessage("  Cache directory              : " + VSQuickCOBOLParse.getCacheDirectory());
            logMessage("  UNC paths disabled           : " + VSCOBOLConfiguration.getDisable_unc_copybooks());
        }
        logMessage(" Parse copybook for references : " + VSCOBOLConfiguration.getParse_copybooks_for_references());
    }

    initExtensions(VSCOBOLConfiguration.get());

    if (thisExtension !== undefined) {
        let ws = getWorkspaceFolders();
        if (ws !== undefined) {
            logMessage("  Workspace Folders");
            for (var folder of ws) {
                logMessage("   => " + folder.name+" @ "+folder.uri.fsPath);
            }
        }

        var extsdir = fileSearchDirectory;
        if (extsdir.length !== 0) {
            logMessage("  CopyBook Search directories (" + extsdir.length + ")");
            for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                let sdir = extsdir[extsdirpos];
                logMessage("   => " + sdir);
            }

            let logicalDirs = getLogicalCopybookdirs("", " ");
            logMessage("  CopyBook Search directories (logical)");
            logMessage("   => " + logicalDirs);
        }

        extsdir = invalidSearchDirectory;
        if (extsdir.length !== 0) {
            logMessage("  Invalid CopyBook directories (" + extsdir.length + ")");
            for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                let sdir = extsdir[extsdirpos];
                logMessage("   => " + sdir);
            }
        }

        logMessage("");
    }
}

export function getCurrentContext(): ExtensionContext {
    return currentContext;
}

function flip_plaintext(doc: TextDocument) {
    if (doc.languageId === 'plaintext') {
        let lcount = doc.lineCount;
        if (lcount >= 3) {
            let firstLine = doc.lineAt((0)).text;
            let secondLine = doc.lineAt(1).text;

            if ((firstLine.length >= 1 && firstLine.charCodeAt(0) === 12) && secondLine.startsWith("* Micro Focus COBOL ")) {
                vscode.languages.setTextDocumentLanguage(doc, "COBOL_LISTFILE");
                return;
            }
        }
    }
}

export function activate(context: ExtensionContext) {
    currentContext = context;
    VSCOBOLConfiguration.init();

    const collection = languages.createDiagnosticCollection('cobolDiag');
    const linter = new CobolLinterProvider(collection, VSCOBOLConfiguration.get());
    const cobolfixer = new CobolLinterActionFixer();
    initExtensions(VSCOBOLConfiguration.get());
    activateLogChannel();

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
        if (window.activeTextEditor !== undefined) {
            let langid = window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                if (VSCOBOLConfiguration.isLineCommentEnabled()) {
                    commenter.processCommentLine();
                } else {
                    commands.executeCommand("editor.action.commentLine");
                }
            }
            return;
        }

        commands.executeCommand("editor.action.commentLine");
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

    var processAllFilesInWorkspace = commands.registerCommand('cobolplugin.processAllFilesInWorkspace', (editor) => {
        return new Promise(resolve => {
            VSQuickCOBOLParse.processAllFilesInWorkspaces();
            resolve("Complete");
        });
    });

    var dumpMetadata = commands.registerCommand('cobolplugin.dumpMetaData', function () {
        COBOLQuickParse.dumpMetaData(VSCOBOLConfiguration.get(), VSQuickCOBOLParse.getCacheDirectory());
    });

    var clearMetaData = commands.registerCommand('cobolplugin.clearMetaData', function () {
        COBOLQuickParse.clearMetaData(VSCOBOLConfiguration.get(), VSQuickCOBOLParse.getCacheDirectory());
    });


    var syntaxCheck = commands.registerCommand('cobolplugin.syntaxCheck', function () {
        tasks.fetchTasks().then((fetchedTasks) => {
            for (const task of fetchedTasks) {
                if (task.name.startsWith("cobol-syntax-check")) {
                    tasks.executeTask(task);
                }
            }
        }, (reason) => {
            // console.log('fetch task was rejected', reason);
        });
    });


    const onDidChangeConfiguration = workspace.onDidChangeConfiguration(() => {
        VSCOBOLConfiguration.init();
        COBOLOutputChannel.clear();
        activateLogChannel();
    });
    context.subscriptions.push(onDidChangeConfiguration);

    // handle Micro Focus .lst files!
    const onDidOpenTextDocumentHandler = workspace.onDidOpenTextDocument((doc) => {
        flip_plaintext(doc);
    });
    context.subscriptions.push(onDidOpenTextDocumentHandler);

    /* flip any already opened docs */
    for (let docid = 0; docid < workspace.textDocuments.length; docid++) {
        flip_plaintext(workspace.textDocuments[docid]);
    }

    var treeView = new SourceViewTree(VSCOBOLConfiguration.get());
    const watcher = workspace.createFileSystemWatcher('**/*');

    watcher.onDidCreate((uri) => {
        treeView.checkFile(uri);
    });

    watcher.onDidDelete((uri) => {
        treeView.clearFile(uri);
    });


    if (VSCOBOLConfiguration.getEnable_auto_tasks()) {
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

            const taskProvider4cobc = tasks.registerTaskProvider('mfcobol', {
                provideTasks: () => {
                    if (!cobolTaskPromise4cobol) {
                        cobolTaskPromise4cobol = getCOBOLTasks_for_mfcobol("cobol_syntax_check_with_mfcobol", true);
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
    context.subscriptions.push(clearMetaData);

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

    let disposable4hover_more_info = languages.registerHoverProvider(allCobolSelectors, {
        provideHover(document, position, token) {
            let txt = document.getText(document.getWordRangeAtPosition(position));
            let txtTarger: CallTarget | undefined = getCallTarget(txt);
            if (txtTarger !== undefined) {
                return new Hover("### " + txtTarger.api + "\n" + txtTarger.description + "\n\n#### [More information?](" + txtTarger.url + ")");
            }
        }
    });
    context.subscriptions.push(disposable4hover_more_info);


    window.onDidChangeActiveTextEditor(editor => {
        if (!editor) {
            return;
        }
        updateDecorations(editor);
        linter.updateLinter(editor.document);

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
        linter.updateLinter(window.activeTextEditor.document);
    }, null, context.subscriptions);

    formatStatusBarItem = window.createStatusBarItem(StatusBarAlignment.Right);
    formatStatusBarItem.command = "cobolplugin.change_source_format";
    formatStatusBarItem.show();

    if (window.activeTextEditor !== undefined) {
        updateDecorations(window.activeTextEditor);
        linter.updateLinter(window.activeTextEditor.document);
    }

    // Open context menu on current file
    let disposable = vscode.commands.registerCommand('cobolplugin.mfurunMenu', function (fileUri) {

        if (unitTestTerminal === undefined) {
            unitTestTerminal = vscode.window.createTerminal(terminalName);
        }

        unitTestTerminal.show(true);
        let utils: COBOLUtils = new COBOLUtils();
        let enableAnsiColor = utils.getMFUnitAnsiColorConfig();

        let properties = propertiesReader(fileUri.fsPath);
        let prefRunner = properties.get('global.preferred-runner');
        unitTestTerminal.sendText(prefRunner + " -show-progress " +
            (enableAnsiColor ? " -dc:ansi " : " ") +
            fileUri.fsPath);
    });
    context.subscriptions.push(disposable);

    let removeAllCommentsCommand = vscode.commands.registerCommand('cobolplugin.removeAllComments', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.RemoveComments(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeAllCommentsCommand);

    let removeIdentificationAreaCommand = vscode.commands.registerCommand('cobolplugin.removeIdentificationArea', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.RemoveIdentificationArea(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeIdentificationAreaCommand);

    let removeColumnNumbersCommand = vscode.commands.registerCommand('cobolplugin.removeColumnNumbers', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.removeColumnNumbers(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeColumnNumbersCommand);


    let makeKeywordsLowercaseCommands = vscode.commands.registerCommand('cobolplugin.makeKeywordsLowercase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.LowerCase);
            }
        }
    });
    context.subscriptions.push(makeKeywordsLowercaseCommands);

    let makeKeywordsUppercaseCommands = vscode.commands.registerCommand('cobolplugin.makeKeywordsUppercase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makeKeywordsUppercaseCommands);

    let makeKeywordsCamelCaseCommands = vscode.commands.registerCommand('cobolplugin.makeKeywordsCamelCase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.CamelCase);
            }
        }
    });
    context.subscriptions.push(makeKeywordsCamelCaseCommands);

    let makeFieldsLowercaseCommand = vscode.commands.registerCommand('cobolplugin.makeFieldsLowercase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makeFieldsLowercaseCommand);

    let makeFieldsUppercaseCommand = vscode.commands.registerCommand('cobolplugin.makeFieldsUppercase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makeFieldsUppercaseCommand);

    let makeFieldsCamelCaseCommand = vscode.commands.registerCommand('cobolplugin.makeFieldsCamelCase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.CamelCase);
            }
        }
    });
    context.subscriptions.push(makeFieldsCamelCaseCommand);

    let makePerformTargetsLowerCaseCommand = vscode.commands.registerCommand('cobolplugin.makePerformTargetsLowerCase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.LowerCase);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsLowerCaseCommand);

    let makePerformTargetsUpperCaseCommand = vscode.commands.registerCommand('cobolplugin.makePerformTargetsUpperCase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsUpperCaseCommand);

    let makePerformTargetsCamelCaseCommand = vscode.commands.registerCommand('cobolplugin.makePerformTargetsCamelCase', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.CamelCase);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsCamelCaseCommand);

    let extractSelectionToParagraphCommand = vscode.commands.registerCommand('cobolplugin.extractSelectionToParagraph', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.extractSelectionTo(vscode.window.activeTextEditor, true);
            }
        }
    });
    context.subscriptions.push(extractSelectionToParagraphCommand);

    let extractSelectionToSectionCommand = vscode.commands.registerCommand('cobolplugin.extractSelectionToSection', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.extractSelectionTo(vscode.window.activeTextEditor, false);
            }
        }
    });
    context.subscriptions.push(extractSelectionToSectionCommand);


    let extractSelectionToCopybookCommand = vscode.commands.registerCommand('cobolplugin.extractSelectionToCopybook', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {
                let utils: COBOLUtils = new COBOLUtils();
                utils.extractSelectionToCopybook(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(extractSelectionToCopybookCommand);


    let migrateCopybooksToWorkspaceCommand = vscode.commands.registerCommand('cobolplugin.migrateCopybooksToWorkspace', () => {
        let utils: COBOLUtils = new COBOLUtils();
        utils.migrateCopybooksToWorkspace();
    });
    context.subscriptions.push(migrateCopybooksToWorkspaceCommand);

    let resequenceColumnNumbersCommands = vscode.commands.registerCommand('cobolplugin.resequenceColumnNumbers', () => {
        if (vscode.window.activeTextEditor) {
            let langid = vscode.window.activeTextEditor.document.languageId;

            if (langid === 'COBOL' || langid === 'OpenCOBOL' || langid === 'ACUCOBOL') {

                let value = vscode.window.showInputBox({
                    prompt: 'Enter start line number and increment',
                    validateInput: (text: string): string | undefined => {
                        if (!text || text.indexOf(' ') === -1) {
                            return 'You must enter two numbers';
                        } else {
                            return undefined;
                        }
                    }
                }).then(value => {
                    // leave early
                    if (value === undefined) {
                        return;
                    }
                    let values: string[] = value.split(" ");
                    let startValue: number = Number.parseInt(values[0]);
                    let incrementValue: number = Number.parseInt(values[1]);
                    if (startValue >= 0 && incrementValue >= 1) {
                        let utils: COBOLUtils = new COBOLUtils();
                        utils.resequenceColumnNumbers(vscode.window.activeTextEditor, startValue, incrementValue);
                    } else {
                        vscode.window.showErrorMessage("Sorry invalid re-sequence given");
                    }
                });
            }
        }
    });
    context.subscriptions.push(resequenceColumnNumbersCommands);

    /* load the cache if we can */
    let cacheDirectory = VSQuickCOBOLParse.getCacheDirectory();
    if (cacheDirectory !== null && cacheDirectory.length > 0) {
        InMemoryGlobalCachesHelper.loadInMemoryGlobalSymbolCaches(cacheDirectory);
        InMemoryGlobalCachesHelper.loadInMemoryGlobalFileCache(cacheDirectory);
    }

    if (VSCOBOLConfiguration.get().process_metadata_cache_on_start) {
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


export function performance_now(): number {
    if (!process.env.BROWSER) {
        try {
            return require('performance-now').performance.now;
        }
        catch { }
    }

    return Date.now();
}
