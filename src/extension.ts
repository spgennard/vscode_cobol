"use strict";
import * as path from "path";
import * as vscode from "vscode";
import os from "os";

import { commands, workspace, StatusBarItem, StatusBarAlignment, ExtensionContext, languages, TextDocument, Position, CancellationToken, ProviderResult, Definition, window, extensions, ViewColumn, ConfigurationChangeEvent } from "vscode";
import * as opencopybook from "./opencopybook";

import { KeywordAutocompleteCompletionItemProvider } from "./vskeywordprovider";
import { CobolSymbolInformationProvider, JCLDocumentSymbolProvider, MFDirectivesSymbolProvider } from "./vssymbolprovider";

import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { CobolReferenceProvider } from "./vsreferenceprovider";
import { CobolLinterProvider, CobolLinterActionFixer } from "./cobollinter";
import { VSSourceTreeViewHandler } from "./vssourceviewtree";
import { CobolSourceCompletionItemProvider } from "./vscobolprovider";
import { VSCOBOLUtils } from "./vscobolutils";
import { ICOBOLSettings } from "./iconfiguration";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const propertiesReader = require("properties-reader");

import { VSWorkspaceFolders } from "./vscobolfolders";
import { COBOLSourceDefinition } from "./vssourcedefinitionprovider";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { VSCobScanner } from "./vscobscanner";
import { BldScriptTaskProvider } from "./bldTaskProvider";
// import { COBOLCaseFormatter } from "./caseformatter";
import { COBOLCallTargetProvider } from "./vscobolcalltargetprovider";
import { COBOLWorkspaceSymbolCacheHelper } from "./cobolworkspacecache";
import { SourceOrFolderTreeItem } from "./sourceItem";
import { VSSemanticProvider } from "./vssemanticprovider";
import { VSPPCodeLens } from "./vsppcodelens";
import { InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { VSCOBOLFileUtils } from "./vsfileutils";
import { COBOLOutputChannel, VSLogger } from "./vslogger";
import { VSExtensionUtils } from "./vsextutis";
import { vsMarginHandler } from "./vsmargindecorations";
import { commentUtils } from "./commenter";
import { colourCommentHandler } from "./vscolourcomments";
import { SnippetCompletionItemProvider } from "./vssnippetprovider";
import { ExtensionDefaults } from "./extensionDefaults";
import { VSCobolRenameProvider } from "./vsrenameprovider";
import { activateCommonCommands, checkForExtensionConflicts, install_call_hierarchy, isMicroFocusCOBOL_LSPActive, toggleMicroFocusLSP } from "./vscommon_commands";
import { VSHelpAndFeedViewHandler } from "./feedbacktree";
import { VSCustomIntelliseRules } from "./vscustomrules";
import { VSHoverProvider } from "./vshoverprovider";
import { COBOLTypeFormatter } from "./vsformatter";
import { TabUtils } from "./tabstopper";
import { VSTerminal } from "./vsterminals";

// import type MarkdownIt from 'markdown-it';
// import hijs from 'highlight.js/lib/core';

// try {
//     // hijs.registerLanguage('COBOL', hljsCOBOL);
//     hijs.registerLanguage('shell', require('highlight.js/lib/languages/shell'));
//     hijs.registerLanguage('COBOL', require('highlightjs-cobol'));
// } catch {
//     //
// }

export const progressStatusBarItem: StatusBarItem = window.createStatusBarItem(StatusBarAlignment.Left);

let bldscriptTaskProvider: vscode.Disposable | undefined;
let shown_enable_semantic_token_provider = false;
let activeEditor: vscode.TextEditor;

let unitTestTerminal: vscode.Terminal | undefined = undefined;
const terminalName = "UnitTest";
let updateDecorationsOnTextEditorEnabled = false;
let sharedContext: ExtensionContext;

function openChangeLog(currentContext: ExtensionContext): void {
    const thisExtension = extensions.getExtension(ExtensionDefaults.thisExtensionName);
    if (thisExtension !== undefined) {
        const extPath = `${thisExtension.extensionPath}`;
        const version = `${thisExtension.packageJSON.version}`;
        const glastVersion = currentContext.globalState.get(`${ExtensionDefaults.thisExtensionName}.version`);
        if (glastVersion !== version) {
            const verFile = path.join(extPath, `CHANGELOG_${version}.md`);
            if (VSExternalFeatures.isFile(verFile)) {
                const readmeUri = vscode.Uri.file(verFile);
                commands.executeCommand("markdown.showPreview", readmeUri, ViewColumn.One, { locked: true });
                currentContext.globalState.update(`${ExtensionDefaults.thisExtensionName}.version`, version);
            }
        }
        const lastDot = version.lastIndexOf(".");
        if (lastDot !== -1) {
            const lastSVersion = version.substring(0, lastDot);
            const glastsVersion = currentContext.globalState.get(`${ExtensionDefaults.thisExtensionName}.sversion`);

            if (glastsVersion !== lastSVersion) {
                const verFile = path.join(extPath, `CHANGELOG_${lastSVersion}.md`);
                if (VSExternalFeatures.isFile(verFile)) {
                    const readmeUri = vscode.Uri.file(verFile);
                    commands.executeCommand("markdown.showPreview", readmeUri, ViewColumn.One, { locked: true });
                    currentContext.globalState.update(`${ExtensionDefaults.thisExtensionName}.sversion`, lastSVersion);
                }
            }
        }
    }
}

async function setupLogChannel(hide: boolean, settings: ICOBOLSettings, quiet: boolean) {
    if (!quiet) {
        if (hide) {
            COBOLOutputChannel.hide();
        } else {
            VSLogger.logChannelSetPreserveFocus(true);
        }
    }
    COBOLOutputChannel.clear();

    const thisExtension = extensions.getExtension(ExtensionDefaults.thisExtensionName);

    if (thisExtension !== undefined) {
        const githubCopilotExtension = extensions.getExtension("GitHub.copilot");
        const mfExt = extensions.getExtension(ExtensionDefaults.rocketCOBOLExtension);

        if (vscode.env.uriScheme !== "vscode") {
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Warning: you are using a untested environment : ${vscode.env.uriScheme}`);
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Version                                     : ${vscode.version}`);
        } else {
            VSLogger.logMessage(`VSCode version                              : ${vscode.version}`);
        }
        VSLogger.logMessage(` Platform                                   : ${os.platform}`);
        VSLogger.logMessage(` Architecture                               : ${os.arch}`);
        VSLogger.logMessage("Extension Information:");
        VSLogger.logMessage(` Extension path                             : ${thisExtension.extensionPath}`);
        VSLogger.logMessage(` Version                                    : ${thisExtension.packageJSON.version}`);


        if (workspace.isTrusted) {
            VSLogger.logMessage(` UNC paths disabled                         : ${settings.disable_unc_copybooks_directories}`);
            VSLogger.logMessage(` Parse copybook for references              : ${settings.parse_copybooks_for_references}`);
            VSLogger.logMessage(` Editor maxTokenizationLineLength           : ${settings.editor_maxTokenizationLineLength}`);
            VSLogger.logMessage(` Semantic token provider enabled            : ${settings.enable_semantic_token_provider}`);
            try {
                const editor_semanticHighlighting_enabled = workspace.getConfiguration("editor.semanticHighlighting").get<number>("enabled");
                VSLogger.logMessage(` editor.semanticHighlighting.enabled        : ${editor_semanticHighlighting_enabled}`);
            } catch {
                //
            }

            try {
                const editor_semanticHighlighting_enabled = workspace.getConfiguration("editor.semanticHighlighting",
                    { languageId: ExtensionDefaults.defaultCOBOLLanguage }).get<number>("enabled");
                VSLogger.logMessage(` [COBOL]editor.semanticHighlighting.enabled : ${editor_semanticHighlighting_enabled}`);
            } catch {
                //
            }

            try {
                const workbench_theme = workspace.getConfiguration("workbench").get<string>("colorTheme");
                VSLogger.logMessage(` workbench color theme                      : ${workbench_theme}`);
            } catch {
                //
            }

            if (vscode.workspace.workspaceFile !== undefined) {
                VSLogger.logMessage(` Active workspacefile                       : ${vscode.workspace.workspaceFile}`);
            }
        }

        VSLogger.logMessage(` Is Workspace Trusted                       : ${workspace.isTrusted}`);

        if (mfExt !== undefined || githubCopilotExtension !== undefined) {
            VSLogger.logMessage("Other Extension Information:");
            if (githubCopilotExtension !== undefined) {
                VSLogger.logMessage(` GitHub Copilot                             : ${githubCopilotExtension.packageJSON.version}`);
            }
            if (mfExt !== undefined) {
                VSLogger.logMessage(` Rocket COBOL                               : ${mfExt.packageJSON.version}`);
                if (window.activeTextEditor) {
                    VSLogger.logMessage(`  microFocusCOBOL.languageServerAutostart   = ${isMicroFocusCOBOL_LSPActive(window.activeTextEditor.document)}`);
                }
            }
        }
    }
}


function activateDesktop(context: ExtensionContext, settings: ICOBOLSettings): void {

    context.subscriptions.push(commands.registerCommand("cobolplugin.clearGlobalCache", function () {
        window.showQuickPick(["Yes", "No"], { placeHolder: "Are you sure you want to clear the metadata?" }).then(function (data) {
            if (data === "Yes") {
                VSCOBOLUtils.clearGlobalCache(settings);
                VSLogger.logMessage("Metadata cache cleared");
            }
        });
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToCopybook", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLFileUtils.extractSelectionToCopybook(vscode.window.activeTextEditor, VSExternalFeatures);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToParagraph", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, true);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.extractSelectionToSection", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, false);
            }
        }
    }));

    // Open context menu on current file
    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.explorerRun", function (fileUri) {

        const fsPath = fileUri.fsPath
        if (fsPath.endsWith(".mfu")) {
            if (unitTestTerminal === undefined) {
                unitTestTerminal = vscode.window.createTerminal(terminalName);
            }

            unitTestTerminal.show(true);
            const enableAnsiColor = VSCOBOLUtils.getMFUnitAnsiColorConfig();

            const properties = propertiesReader(fileUri.fsPath);
            const prefRunner = properties.get("global.preferred-runner");
            unitTestTerminal.sendText(prefRunner + " -show-progress " +
                (enableAnsiColor ? " -dc:ansi " : " ") +
                fileUri.fsPath);

            return;
        }

        VSCOBOLUtils.runOrDebug(fsPath, false);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.explorerDebug", function (fileUri) {

        const fsPath = fileUri.fsPath
        if (fsPath.endsWith(".mfu")) {
            if (unitTestTerminal === undefined) {
                unitTestTerminal = vscode.window.createTerminal(terminalName);
            }

            unitTestTerminal.show(true);
            const enableAnsiColor = VSCOBOLUtils.getMFUnitAnsiColorConfig();

            const properties = propertiesReader(fileUri.fsPath);
            const prefRunner = properties.get("global.preferred-runner");
            unitTestTerminal.sendText(prefRunner + " -show-progress " +
                (enableAnsiColor ? " -dc:ansi " : " ") +
                fileUri.fsPath);

            return;
        }

        VSCOBOLUtils.runOrDebug(fsPath, true);
    }));


    context.subscriptions.push(commands.registerCommand("cobolplugin.processAllFilesInWorkspaceOnStartup", async () => {
        await VSCobScanner.processAllFilesInWorkspaceOutOfProcess(settings, false, false, -1);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.runCommand", function (si: SourceOrFolderTreeItem) {
        if (si !== undefined) {
            VSSourceTreeViewHandler.actionSourceViewItemFunction(si, false);
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.runDebugCommand", function (si: SourceOrFolderTreeItem) {
        if (si !== undefined) {
            VSSourceTreeViewHandler.actionSourceViewItemFunction(si, true);
        }
    }));

}

async function handleScopedChange(event:ConfigurationChangeEvent, scope?: vscode.ConfigurationScope) {
    const updated = event.affectsConfiguration(ExtensionDefaults.defaultEditorConfig, scope);
    const outline_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig, scope}.outline`, scope);
    const md_syms = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_symbols`, scope);
    const md_eps = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_entrypoints`, scope);
    const md_types = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_types`, scope);
    const md_metadata_files = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_files`, scope);
    const md_metadata_knowncopybooks = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.metadata_knowncopybooks`, scope);
    const md_copybookdirs = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.copybookdirs`, scope);
    const enable_semantic_token_provider = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_semantic_token_provider`, scope);
    const maintain_metadata_recursive_search = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.maintain_metadata_recursive_search`, scope);
    const enable_comments_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_comments_tags`, scope);
    const comments_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.comments_tags`, scope);
    const intellisense_style_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.intellisense_style`, scope);
    const enable_columns_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_columns_tags`, scope);
    const columns_tags_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.columns_tags`, scope);
    const intellisense_add_space_keywords_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.intellisense_add_space_keywords`, scope);
    const custom_intellisense_rules_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.custom_intellisense_rules`, scope);
    const tabstops_anchors_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.tabstops_anchors`, scope);
    const enable_program_information_changed = event.affectsConfiguration(`${ExtensionDefaults.defaultEditorConfig}.enable_program_information`, scope);


    if (updated) {
        VSCOBOLConfiguration.reinitWorkspaceSettingsScoped(VSExternalFeatures);
        const settings = VSCOBOLConfiguration.get_workspace_settings();
        if (!md_syms && !md_eps && !md_types && !md_metadata_files && !md_metadata_knowncopybooks && !enable_semantic_token_provider) {
            VSCOBOLSourceScanner.clearCOBOLCache();
            setupLogChannel(true, settings, true);
            VSCOBOLUtils.setupFilePaths(settings);
            async () => {
                await VSCOBOLUtils.setupUrlPaths(settings);
                await VSSourceTreeViewHandler.setupSourceViewTree(settings, true);
            }
        }

        if (md_syms) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings, settings.metadata_symbols, true);
        }

        if (md_eps) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings, settings.metadata_entrypoints, true);
        }

        if (md_types) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings, settings.metadata_types, true);
        }

        if (md_metadata_files) {
            COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(settings, VSExternalFeatures, settings.metadata_files, true);
        }

        if (enable_semantic_token_provider && !shown_enable_semantic_token_provider) {
            shown_enable_semantic_token_provider = true;
            vscode.window.showInformationMessage(`The configuration setting '${ExtensionDefaults.defaultEditorConfig}.enable_semantic_token_provider' has changed but you may not see the affects until you have either close/reload your documents or restarted this session`);
        }

        if (md_metadata_knowncopybooks) {
            COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, true);
        }

        if (md_copybookdirs) {
            VSCOBOLSourceScanner.clearCOBOLCache();
            setupLogChannel(true, settings, true);
            VSCOBOLUtils.setupFilePaths(settings);
            await VSCOBOLUtils.setupUrlPaths(settings);

            VSCOBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
            VSCOBOLUtils.populateDefaultCopyBooksSync(settings, true);
        }

        if (maintain_metadata_recursive_search) {
            VSCOBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
            VSCOBOLUtils.populateDefaultCopyBooksSync(settings, true);
        }

        if (outline_changed) {
            vscode.window.showInformationMessage(`The configuration setting '${ExtensionDefaults.defaultEditorConfig}.outline' has changed but you may not see the affects until you have either reloaded your window or restarted this session`);
        }

        if (custom_intellisense_rules_changed) {
            VSCustomIntelliseRules.Default.reFreshConfiguration(settings);
        }

        if (enable_comments_tags_changed || comments_tags_changed) {
            colourCommentHandler.setupTags();
        }

        if (enable_columns_tags_changed || columns_tags_changed) {
            vsMarginHandler.setupTags();
        }

        // ensure we update the map
        if (intellisense_style_changed) {
            SnippetCompletionItemProvider.Default.reInitCallMap(settings);
        }

        if (intellisense_add_space_keywords_changed) {
            KeywordAutocompleteCompletionItemProvider.Default4COBOL.reFreshConfiguration(settings);
        }

        if (tabstops_anchors_changed) {
            TabUtils.clearTabstopCache();
        }

        if (enable_program_information_changed && settings.enable_program_information) {
            install_call_hierarchy(settings, sharedContext)
        }
        checkForExtensionConflicts(settings, sharedContext);
    }
}
export async function activate(context: ExtensionContext) {
    sharedContext = context;
    const settings: ICOBOLSettings = VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);

    await setupLogChannel(true, settings, true);
    VSCOBOLUtils.setupFilePaths(settings);
    await VSCOBOLUtils.setupUrlPaths(settings);

    if (checkForExtensionConflicts(settings, context)) {
        throw new Error("Unable to activate extension due to conflicts");
    }

    activateDesktop(context, settings);
    activateCommonCommands(context);

    // re-init if something gets installed or removed
    context.subscriptions.push(vscode.extensions.onDidChange(() => {
        setupLogChannel(true, settings, false);
        VSLogger.logMessage("extensions changed");
    }));

    const onDidChangeConfiguration = workspace.onDidChangeConfiguration(async (event: ConfigurationChangeEvent) => {
        await handleScopedChange(event,undefined);
    });
    context.subscriptions.push(onDidChangeConfiguration);

    const collection = languages.createDiagnosticCollection("cobolDiag");
    const linter = new CobolLinterProvider(collection);
    const cobolfixer = new CobolLinterActionFixer();

    COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings, settings.metadata_symbols, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings, settings.metadata_entrypoints, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings, settings.metadata_types, false);
    COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(settings, VSExternalFeatures, settings.metadata_files, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, false);

    context.subscriptions.push(commands.registerCommand("cobolplugin.insertIgnoreCommentLine", function (docUri: vscode.Uri, offset: number, code: string) {
        cobolfixer.insertIgnoreCommentLine(docUri, offset, code);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.portCodeCommandLine", function (docUri: vscode.Uri, lineNumber: number, code: string) {
        cobolfixer.portCodeCommandLine(docUri, lineNumber, code);
    }));


    context.subscriptions.push(commands.registerCommand("cobolplugin.findCopyBookDirectory", function (docUri: vscode.Uri, linenum: number, code: string) {
        cobolfixer.findCopyBookDirectory(settings, docUri, linenum, code);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.commentline", function () {
        if (window.activeTextEditor !== undefined) {
            const langid = window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                if (settings.line_comment) {
                    commentUtils.processCommentLine(settings);
                } else {
                    commands.executeCommand("editor.action.commentLine");
                }
            }
            return;
        }

        commands.executeCommand("editor.action.commentLine");
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.processAllFilesInWorkspace", async () => {
        let settings: ICOBOLSettings;
        if (window.activeTextEditor !== undefined) {
            settings = VSCOBOLConfiguration.get_resource_settings(window.activeTextEditor.document, VSExternalFeatures);
        } else {
            settings = VSCOBOLConfiguration.get_workspace_settings();
        }
        if (InMemoryGlobalSymbolCache.defaultCallableSymbols.size < 500) {
            VSCobScanner.processAllFilesInWorkspaceOutOfProcess(settings, true, false, -1);
            return;
        }

        window.showQuickPick(["Yes", "No"], { placeHolder: "Your workspace is large, do you want to extra threads for your metadata scan?" }).then(function (data) {
            if (data === "Yes") {
                const cpuCount = os.cpus().length;
                const defCpuCount = cpuCount >= 4 ? Math.trunc(cpuCount / 2) : cpuCount;
                vscode.window.showInputBox({
                    prompt: "How many threads do you want to use?",
                    value: "" + defCpuCount,
                    validateInput: (threadString: string): string | undefined => {
                        const threadCount: number = Number.parseInt(threadString, 10);

                        if (threadCount < 2 || threadCount > (defCpuCount * 3)) {
                            return `Thread count must be between 2 and ${defCpuCount * 3}`;
                        } else {
                            return undefined;
                        }
                    }
                }).then(value => {
                    // leave early
                    if (value === undefined) {
                        return;
                    }

                    const threadCount: number = Number.parseInt(value, 10);
                    VSCobScanner.processAllFilesInWorkspaceOutOfProcess(settings, true, true, threadCount);

                });

            } else {
                VSCobScanner.processAllFilesInWorkspaceOutOfProcess(settings, true, false, -1);
            }
        });
    }));


    context.subscriptions.push(workspace.onDidChangeWorkspaceFolders(async () => {
        const settings: ICOBOLSettings = VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);

        await setupLogChannel(false, settings, true);
        VSCOBOLUtils.setupFilePaths(settings);
        await VSCOBOLUtils.setupUrlPaths(settings);
    }));

    // default to on.. but only when "extend mf" is enabled via "when" clause.. 
    vscode.commands.executeCommand("setContext", `${ExtensionDefaults.defaultEditorConfig}.enable_migrate2mf_tasks`, true);

    context.subscriptions.push(workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {

        VSExtensionUtils.flip_plaintext(doc);

        if (VSExtensionUtils.isSupportedLanguage(doc)) {
            if (window.activeTextEditor) {
                activeEditor = window.activeTextEditor;
                await triggerUpdateDecorations();
            }
        }

        //no metadata, then seed it with basic implicit program-id symbols based on the files in workspace
        const ws = VSWorkspaceFolders.get(settings);
        if (ws !== undefined) {
            await vscode.commands.executeCommand<vscode.SymbolInformation[]>("vscode.executeDocumentSymbolProvider", doc.uri);
            await VSCOBOLUtils.populateDefaultCallableSymbols(settings, false);
        }
    }));

    //eslint-disable-next-line @typescript-eslint/no-unused-vars
    context.subscriptions.push(vscode.window.onDidChangeVisibleTextEditors(async (viewEditors) => {
        if (updateDecorationsOnTextEditorEnabled) {
            for (const textEditor of viewEditors) {
                await updateDecorationsOnTextEditor(textEditor);
            }
        }
    }));

    // const onDidChangeTextEditorVisibleRanges = vscode.window.onDidChangeTextEditorVisibleRanges(async (tevr) => {
    //     activeEditor = tevr.textEditor;
    //     await triggerUpdateDecorations();
    // });
    // context.subscriptions.push(onDidChangeTextEditorVisibleRanges);

    context.subscriptions.push(workspace.onDidCloseTextDocument(async (doc: vscode.TextDocument) => {
        if (VSExtensionUtils.isSupportedLanguage(doc)) {
            const config = VSCOBOLConfiguration.get_resource_settings(doc, VSExternalFeatures);
            VSCOBOLSourceScanner.removeCachedObject(doc, config);
            VSCOBOLConfiguration.clearResourceCache(doc);
        }
    }));

    /* flip any already opened docs */
    for (let docid = 0; docid < workspace.textDocuments.length; docid++) {
        VSExtensionUtils.flip_plaintext(workspace.textDocuments[docid]);
    }

    await VSSourceTreeViewHandler.setupSourceViewTree(settings, false);
    VSHelpAndFeedViewHandler.setupSourceViewTree(settings, false);
    context.subscriptions.push(COBOLTypeFormatter.register(settings));

    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const ccbp = new opencopybook.COBOLCopyBookProvider(VSExternalFeatures);
            return ccbp.provideDefinition(doc, pos, ct);
        }
    }));

    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    }));

    context.subscriptions.push(languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csdp = new COBOLCallTargetProvider(VSExternalFeatures);
            return csdp.provideDefinition(doc, pos, ct);
        }
    }));

    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolReferenceProvider()));
    context.subscriptions.push(languages.registerCodeActionsProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), cobolfixer));

    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllJCLSelectors(settings), new KeywordAutocompleteCompletionItemProvider(false, settings)));
    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new KeywordAutocompleteCompletionItemProvider(true, settings)));

    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), SnippetCompletionItemProvider.Default.reInitCallMap(settings)));

    if (settings.outline) {
        const jclDocumentSymbolProvider = new JCLDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllJCLSelectors(settings), jclDocumentSymbolProvider));

        /* TODO: add .DIR keywords too */
        const symbolInformationProvider = new CobolSymbolInformationProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), symbolInformationProvider));

        const mfDirectivesProvider = new MFDirectivesSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllMFProvidersSelectors(settings), mfDirectivesProvider));

    }

    context.subscriptions.push(languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolSourceCompletionItemProvider(VSExternalFeatures)));

    // const cobolCommentProvider = new CobolCommentProvider(VSCOBOLConfiguration.get());
    // const cobolCommentProviderDisposible = languages.registerCompletionItemProvider(allCobolSelectors, cobolCommentProvider);
    // context.subscriptions.push(cobolCommentProviderDisposible);

    /* hover provider */
    context.subscriptions.push(languages.registerHoverProvider(VSExtensionUtils.getAllCobolSelectors(settings, false), {
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): ProviderResult<vscode.Hover> {
            return VSHoverProvider.provideHover(settings, document, position);
        }
    }));
    context.subscriptions.push(languages.registerRenameProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new VSCobolRenameProvider()));

    const updateDecorationsOnTextEditor = async (editor: vscode.TextEditor) => {
        await vsMarginHandler.updateDecorations(editor);
        if (editor && editor.document) {
            await linter.updateLinter(editor.document);
        }
        await colourCommentHandler.updateDecorations(editor);
    };

    const activeEditorupdateDecorations = async () => {
        await updateDecorationsOnTextEditor(activeEditor);
    };

    let timeout: NodeJS.Timeout | undefined = undefined;

    const triggerUpdateDecorations = async () => {
        if (timeout) {
            clearTimeout(timeout);
        }
        timeout = setTimeout(activeEditorupdateDecorations, 200);
    }

    window.onDidChangeActiveTextEditor(async (editor) => {
        if (!editor) {
            return;
        }
        activeEditor = editor;
        triggerUpdateDecorations();

    }, null, context.subscriptions);

    window.onDidChangeTextEditorSelection(async (event) => {
        if (!event.textEditor) {
            return;
        }
        triggerUpdateDecorations();

    }, null, context.subscriptions);

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    workspace.onDidChangeTextDocument(async (event) => {
        if (!window.activeTextEditor) {
            return;
        }

        if (event.document === activeEditor.document) {
            activeEditor = window.activeTextEditor;
            triggerUpdateDecorations();
        }

    }, null, context.subscriptions);

    if (window.activeTextEditor !== undefined) {
        activeEditor = window.activeTextEditor;
        triggerUpdateDecorations();
    }

    progressStatusBarItem.command = "cobolplugin.showCOBOLChannel";
    progressStatusBarItem.hide();
    context.subscriptions.push(progressStatusBarItem);

    if (workspace.isTrusted) {
        bldscriptTaskProvider = vscode.tasks.registerTaskProvider(BldScriptTaskProvider.BldScriptType, new BldScriptTaskProvider());
        context.subscriptions.push(bldscriptTaskProvider);
    }

    const provider = VSSemanticProvider.provider();
    vscode.languages.registerDocumentSemanticTokensProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), provider, VSSemanticProvider.getLegend());

    const codelensProvider = new VSPPCodeLens();
    languages.registerCodeLensProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), codelensProvider);

    vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);

    window.onDidChangeTextEditorSelection((e: vscode.TextEditorSelectionChangeEvent) => {
        if (!VSExtensionUtils.isSupportedLanguage(e.textEditor.document)) {
            return;
        }

        for (const sel of e.selections) {
            for (let startLine = sel.start.line; startLine <= sel.end.line; startLine++) {
                const textSelection = e.textEditor.document.lineAt(startLine).text;
                const line = textSelection.trimEnd();
                const sipos = VSCOBOLUtils.getStorageItemPosition(line);
                if (sipos !== -1) {
                    vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);
                    return;
                }
            }
        }

        vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", false);
    })

    // vscode.languages.registerSignatureHelpProvider(VSExtensionUtils.getAllCobolSelectors(settings), new class implements vscode.SignatureHelpProvider {
    //     provideSignatureHelp(
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         document: vscode.TextDocument,
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         position: vscode.Position,
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         token: vscode.CancellationToken,
    //         // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //         context: vscode.SignatureHelpContext
    //     ): vscode.ProviderResult<vscode.SignatureHelp> {

    //         // console.log(context.activeSignatureHelp);

    //         // Return fake signature help result
    //         const sigHelp = new vscode.SignatureHelp();
    //         sigHelp.activeParameter = 0;
    //         sigHelp.activeSignature = 0;
    //         sigHelp.signatures = [
    //             new vscode.SignatureInformation("1", "Paramter 1"),
    //             new vscode.SignatureInformation("2")
    //         ];
    //         return sigHelp;
    //     }
    // }, {
    //     triggerCharacters: ["by"],
    //     retriggerCharacters: [","]
    // });

    let toggleDone = false;
    for (const vte of vscode.window.visibleTextEditors) {
        // update document decorations
        await updateDecorationsOnTextEditor(vte);

        // if we have a document assigned to the 'Micro Focus' language id, then turn the lsp setting on
        if (!toggleDone && vte.document.languageId === ExtensionDefaults.microFocusCOBOLLanguageId) {
            const mfExt = extensions.getExtension(ExtensionDefaults.rocketCOBOLExtension);
            if (mfExt) {
                await toggleMicroFocusLSP(settings, vte.document, true);
            }
            toggleDone = true;
        }
    }

    // currently linux only
    if (process.platform === 'linux') {
        vscode.window.registerTerminalProfileProvider('bitlang.terminals', new VSTerminal(context));
    }

    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolReferenceProvider()));

    if (settings.process_metadata_cache_on_start) {
        try {
            if (settings.maintain_metadata_cache) {
                commands.executeCommand("cobolplugin.processAllFilesInWorkspaceOnStartup");
            }
        } catch {
            // just incase
        }
    }

    openChangeLog(context);

    updateDecorationsOnTextEditorEnabled = true;

    //     return {
    //         extendMarkdownIt(md: MarkdownIt) {
    //             return md.use(require('markdown-it-highlightjs/core'), {hijs});
    //         }
    //     };
}

export async function deactivateAsync(): Promise<void> {
    VSCOBOLUtils.saveGlobalCacheToWorkspace(VSCOBOLConfiguration.get_workspace_settings());
}


export async function deactivate(): Promise<void> {
    if (bldscriptTaskProvider) {
        bldscriptTaskProvider.dispose();
    }
    await deactivateAsync();
}
