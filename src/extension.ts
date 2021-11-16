"use strict";
import * as path from "path";
import * as vscode from "vscode";
import os from "os";

import { commands, workspace, StatusBarItem, StatusBarAlignment, ExtensionContext, languages, TextDocument, Position, CancellationToken, ProviderResult, Definition, window, extensions, ViewColumn, ConfigurationChangeEvent } from "vscode";
import * as cobolProgram from "./cobolprogram";
import * as tabstopper from "./tabstopper";
import * as opencopybook from "./opencopybook";
import * as commenter from "./commenter";

import { COBOLDocumentationCommentHandler } from "./doccomment";
import { KeywordAutocompleteCompletionItemProvider } from "./keywordprovider";
import { isSupportedLanguage } from "./margindecorations";
import { CobolSymbolInformationProvider, JCLDocumentSymbolProvider } from "./symbolprovider";

import { updateDecorations } from "./margindecorations";
import { COBOLFileUtils } from "./fileutils";

import { VSCOBOLSourceScanner, clearCOBOLCache } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { CobolReferenceProvider } from "./cobolreferenceprovider";
import { CobolLinterProvider, CobolLinterActionFixer } from "./cobollinter";
import { VSSourceTreeViewHandler } from "./sourceviewtree";
import { CobolSourceCompletionItemProvider } from "./cobolprovider";
import { COBOLUtils, FoldStyle, FoldAction } from "./cobolutils";
import { ICOBOLSettings } from "./iconfiguration";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const propertiesReader = require("properties-reader");

import { getVSWorkspaceFolders } from "./cobolfolders";
import { COBOLSourceDefinition } from "./sourcedefinitionprovider";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { VSCobScanner } from "./vscobscanner";
import { BldScriptTaskProvider } from "./bldTaskProvider";
import { COBOLCaseFormatter } from "./caseformatter";
import { COBOLCallTargetProvider } from "./cobolcalltargetprovider";
import { COBOLWorkspaceSymbolCacheHelper } from "./cobolworkspacecache";
import { SourceItem } from "./sourceItem";
import { VSSemanticProvider } from "./vssemanticprovider";
import { VSPPCodeLens } from "./vsppcodelens";
import { InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { VSCOBOLFileUtils } from "./vsfileutils";
import { VSCOBOLSourceScannerTools } from "./vssourcescannerutils";
import { COBOLOutputChannel, VSLogger } from "./vslogger";
import { VSExtensionUtils } from "./vsextutis";
// import { CobolDocumentSymbolProvider } from './documentsymbolprovider';

export const progressStatusBarItem: StatusBarItem = window.createStatusBarItem(StatusBarAlignment.Left);

let currentContext: ExtensionContext;
let bldscriptTaskProvider: vscode.Disposable | undefined;
let shown_enable_semantic_token_provider = false;
let messageBoxDone = false;


export const currentHostInformation = `${os.hostname()}/${os.userInfo().username}`;

const fileSearchDirectory: string[] = [];
let invalidSearchDirectory: string[] = [];
let unitTestTerminal: vscode.Terminal | undefined = undefined;
const terminalName = "UnitTest";

// setup
VSCOBOLConfiguration.externalFeatures = VSExternalFeatures;
VSCOBOLConfiguration.externalFeatures.setCombinedCopyBookSearchPath(fileSearchDirectory);

function openChangeLog(): void {
    const thisExtension = extensions.getExtension("bitlang.cobol");
    if (thisExtension !== undefined) {
        const extPath = `${thisExtension.extensionPath}`;
        const version = `${thisExtension.packageJSON.version}`;
        const glastVersion = currentContext.globalState.get("bitlang.cobol.version");
        if (glastVersion !== version) {
            const verFile = path.join(extPath, `CHANGELOG_${version}.md`);
            if (VSExternalFeatures.isFile(verFile)) {
                const readmeUri = vscode.Uri.file(verFile);
                commands.executeCommand("markdown.showPreview", readmeUri, ViewColumn.One, { locked: true });
                currentContext.globalState.update("bitlang.cobol.version", version);
            }
        }
        const lastDot = version.lastIndexOf(".");
        if (lastDot !== -1) {
            const lastSVersion = version.substr(0, lastDot);
            const glastsVersion = currentContext.globalState.get("bitlang.cobol.sversion");

            if (glastsVersion !== lastSVersion) {
                const verFile = path.join(extPath, `CHANGELOG_${lastSVersion}.md`);
                if (VSExternalFeatures.isFile(verFile)) {
                    const readmeUri = vscode.Uri.file(verFile);
                    commands.executeCommand("markdown.showPreview", readmeUri, ViewColumn.One, { locked: true });
                    currentContext.globalState.update("bitlang.cobol.sversion", lastSVersion);
                }
            }
        }
    }
}

const blessed_extensions: string[] = [
    "HCLTechnologies.hclappscancodesweep",    // code scanner
    "Micro-Focus-AMC.mfcobol"                 // Micro Focus COBOL Extension
];

const known_problem_extensions: string[][] = [
    ["control flow extension", "BroadcomMFD.ccf"]              // control flow extension
];

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function getExtensionInformation(grab_info_for_ext: vscode.Extension<any>, reasons: string[]): string {
    let dupExtensionMessage = "";

    dupExtensionMessage += `\nThe extension ${grab_info_for_ext.packageJSON.name} from ${grab_info_for_ext.packageJSON.publisher} has conflicting functionality\n`;
    if (reasons.length !== 0) {
        for (const reason of reasons) {
            dupExtensionMessage += ` Reason        : ${reason}\n`;
        }
    }

    if (grab_info_for_ext.packageJSON.id !== undefined) {
        dupExtensionMessage += ` Id            : ${grab_info_for_ext.packageJSON.id}\n`;

        if (grab_info_for_ext.packageJSON.description !== undefined) {
            dupExtensionMessage += ` Description   : ${grab_info_for_ext.packageJSON.description}\n`;
        }
        if (grab_info_for_ext.packageJSON.version !== undefined) {
            dupExtensionMessage += ` Version       : ${grab_info_for_ext.packageJSON.version}\n`;
        }
        if (grab_info_for_ext.packageJSON.repository !== undefined && grab_info_for_ext.packageJSON.repository.url !== undefined) {
            dupExtensionMessage += ` Repository    : ${grab_info_for_ext.packageJSON.repository.url}\n`;
        }
        if (grab_info_for_ext.packageJSON.bugs !== undefined && grab_info_for_ext.packageJSON.bugs.url !== undefined) {
            dupExtensionMessage += ` Bug Reporting : ${grab_info_for_ext.packageJSON.bugs.url}\n`;
        }
        if (grab_info_for_ext.packageJSON.bugs !== undefined && grab_info_for_ext.packageJSON.bugs.email !== undefined) {
            dupExtensionMessage += ` Bug Email     : ${grab_info_for_ext.packageJSON.bugs.email}\n`;
        }
        if (dupExtensionMessage.length !== 0) {
            dupExtensionMessage += "\n";
        }
    }

    return dupExtensionMessage;
}


function checkForExtensionConflicts(): string {
    let dupExtensionMessage = "";

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    for (const ext of extensions.all) {
        const reason = [];
        let ignore_blessed = false;
        if (ext !== undefined && ext.packageJSON !== undefined) {
            if (ext.packageJSON.id !== undefined) {
                if (ext.packageJSON.id === "bitlang.cobol") {
                    continue;
                }

                for (const blessed_extension of blessed_extensions) {
                    if (blessed_extension === ext.packageJSON.id) {
                        ignore_blessed = true;
                    }
                }

                if (!ignore_blessed) {
                    for (const [type_of_extension, known_problem_extension] of known_problem_extensions) {
                        if (known_problem_extension === ext.packageJSON.id) {
                            reason.push(`contributes ${type_of_extension} that does not match syntax provided`);
                        }
                    }
                }
            }

            if (!ignore_blessed) {
                let extMarkedAsDebugger = false;
                //categories

                if (ext.packageJSON.categories !== undefined) {
                    const categoriesBody = ext.packageJSON.categories;
                    if (categoriesBody !== undefined && categoriesBody instanceof Object) {
                        for (const key in categoriesBody) {
                            try {
                                const element = categoriesBody[key];
                                if (element !== undefined) {
                                    const l = `${element}`.toUpperCase();
                                    if (l === "DEBUGGERS") {
                                        extMarkedAsDebugger = true;
                                    }
                                }
                            } catch {
                                // just incase
                            }
                        }
                    }
                }

                if (ext.packageJSON.contributes !== undefined) {
                    const grammarsBody = ext.packageJSON.contributes.grammars;
                    const languagesBody = ext.packageJSON.contributes.languages;

                    // check for unexpected duplicate COBOL language
                    if (grammarsBody !== undefined && grammarsBody instanceof Object) {
                        for (const key in grammarsBody) {
                            try {
                                const element = grammarsBody[key];
                                if (element !== undefined && element.language !== undefined) {
                                    const l = `${element.language}`.toUpperCase();
                                    if (l === "COBOL") {
                                        reason.push("contributes conflicting grammar");
                                    }
                                }
                            } catch {
                                // just incase
                            }
                        }
                    }

                    // check for language id
                    if (languagesBody !== undefined && languagesBody instanceof Object) {
                        for (const key in languagesBody) {
                            const languageElement = languagesBody[key];
                            try {

                                if (languageElement !== undefined && languageElement.id !== undefined) {
                                    const l = `${languageElement.id}`.toUpperCase();
                                    if (l === "COBOL") {
                                        reason.push("contributes language id");
                                    }
                                }
                            }
                            catch {
                                // just incase
                            }
                        }
                    }

                    if (extMarkedAsDebugger) {
                        const debuggerBody = ext.packageJSON.contributes.debuggers;
                        const breakpointsBody = ext.packageJSON.contributes.breakpoints;
                        if (debuggerBody !== undefined && debuggerBody instanceof Object) {
                            for (const key in debuggerBody) {
                                try {
                                    const debuggerElement = debuggerBody[key];
                                    if (debuggerElement !== undefined && debuggerElement.enableBreakpointsFor !== undefined) {
                                        if (debuggerElement.enableBreakpointsFor.languageIds !== undefined) {
                                            for (const bpLangidKey in debuggerElement.enableBreakpointsFor.languageIds) {
                                                const languageElement = debuggerElement.enableBreakpointsFor.languageIds[bpLangidKey];
                                                const l = `${languageElement}`.toUpperCase();
                                                if (l === "COBOL") {
                                                    reason.push("extension includes debugger for a different COBOL vendor");
                                                }
                                            }
                                        }
                                    }
                                }
                                catch {
                                    // just incase
                                }
                            }
                        }

                        if (breakpointsBody !== undefined && breakpointsBody instanceof Object) {
                            for (const key in breakpointsBody) {
                                const bpLangKey = breakpointsBody[key];
                                try {
                                    if (bpLangKey !== undefined && bpLangKey.language !== undefined) {
                                        const bpLang = `${bpLangKey.language}`;
                                        if (bpLang === "COBOL") {
                                            reason.push("extension includes debugger for a different COBOL vendor");
                                        }
                                    }
                                }
                                catch {
                                    // just incase
                                }
                            }
                        }
                    }
                }
            }

            if (reason.length !== 0) {
                dupExtensionMessage += getExtensionInformation(ext, reason);
            }
        }
    }

    return dupExtensionMessage;
}

function activateLogChannelAndPaths(hide: boolean, settings: ICOBOLSettings, quiet: boolean) {
    if (!quiet) {
        if (hide) {
            COBOLOutputChannel.hide();
        } else {
            VSLogger.logChannelSetPreserveFocus(true);
        }
    }
    COBOLOutputChannel.clear();

    const thisExtension = extensions.getExtension("bitlang.cobol");

    if (thisExtension !== undefined) {
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
            } catch
            {
                //
            }

            try {
                const editor_semanticHighlighting_enabled = workspace.getConfiguration("editor.semanticHighlighting",
                    { languageId: "COBOL" }).get<number>("enabled");
                VSLogger.logMessage(` [COBOL]editor.semanticHighlighting.enabled : ${editor_semanticHighlighting_enabled}`);
            } catch
            {
                //
            }

            try {
                const workbench_theme = workspace.getConfiguration("workbench").get<string>("colorTheme");
                VSLogger.logMessage(` workbench color theme                      : ${workbench_theme}`);
            } catch
            {
                //
            }
            if (vscode.workspace.workspaceFile !== undefined) {
                VSLogger.logMessage(` Active workspacefile                       : ${vscode.workspace.workspaceFile}`);
            }
        }
        VSLogger.logMessage(` Is Workspace Trusted                       : ${workspace.isTrusted}`);

    }

    fileSearchDirectory.length = 0;

    const extsdir = settings.copybookdirs;
    invalidSearchDirectory = settings.invalid_copybookdirs;
    invalidSearchDirectory.length = 0;

    // step 1 look through the copybook default dirs for "direct" paths and include them in search path
    for (const ddir of extsdir) {
        if (workspace.isTrusted === false) {
            // copybooks that have a direct path or a network are only available in full trust
            if (COBOLFileUtils.isDirectPath(ddir) || COBOLFileUtils.isNetworkPath(ddir)) {
                invalidSearchDirectory.push(ddir);
                continue;
            }
        } else {
            if (settings.disable_unc_copybooks_directories && COBOLFileUtils.isNetworkPath(ddir)) {
                VSLogger.logMessage(" Copybook directory " + ddir + " has been marked as invalid, as it is a unc filename");
                invalidSearchDirectory.push(ddir);
            }
            else if (COBOLFileUtils.isDirectPath(ddir)) {
                VSLogger.logWarningMessage(` non portable copybook directory ${ddir} defined`);
                if (workspace !== undefined && getVSWorkspaceFolders() !== undefined) {
                    if (VSCOBOLFileUtils.isPathInWorkspace(ddir) === false) {
                        if (COBOLFileUtils.isNetworkPath(ddir)) {
                            VSLogger.logMessage(" The directory " + ddir + " for performance should be part of the workspace");
                        }
                    }
                }

                const startTime = VSExternalFeatures.performance_now();
                if (COBOLFileUtils.isDirectory(ddir)) {
                    const totalTimeInMS = VSExternalFeatures.performance_now() - startTime;
                    const timeTaken = totalTimeInMS.toFixed(2);
                    if (totalTimeInMS <= 2000) {
                        fileSearchDirectory.push(ddir);
                    } else {
                        VSLogger.logMessage(" Slow copybook directory dropped " + ddir + " as it took " + timeTaken + "ms");
                        invalidSearchDirectory.push(ddir);
                    }
                } else {
                    invalidSearchDirectory.push(ddir);
                }
            }
        }
    }

    // step 2
    const ws = getVSWorkspaceFolders();
    if (ws !== undefined) {
        for (const folder of ws) {
            // place the workspace folder in the copybook path
            fileSearchDirectory.push(folder.uri.fsPath);

            /* now add any extra directories that are below this workspace folder */
            for (const extdir of extsdir) {
                try {
                    if (COBOLFileUtils.isDirectPath(extdir) === false) {
                        const sdir = path.join(folder.uri.fsPath, extdir);

                        if (COBOLFileUtils.isDirectory(sdir)) {
                            if (COBOLFileUtils.isNetworkPath(sdir) && VSCOBOLFileUtils.isPathInWorkspace(sdir) === false) {
                                VSLogger.logMessage(" The directory " + sdir + " for performance should be part of the workspace");
                            }

                            fileSearchDirectory.push(sdir);
                        } else {
                            invalidSearchDirectory.push(sdir);
                        }
                    }
                }
                catch (e) {
                    VSLogger.logException("dir", e as Error);
                }
            }
        }
    }


    
    const filterfileSearchDirectory = fileSearchDirectory.filter((elem, pos) => fileSearchDirectory.indexOf(elem) === pos);
    fileSearchDirectory.length = 0;
    for(const fsd of filterfileSearchDirectory) {
        fileSearchDirectory.push(fsd);
    }
    
    invalidSearchDirectory = invalidSearchDirectory.filter((elem, pos) => invalidSearchDirectory.indexOf(elem) === pos);

    if (thisExtension !== undefined) {
        const ws = getVSWorkspaceFolders();
        if (ws !== undefined) {
            VSLogger.logMessage("  Workspace Folders:");
            for (const folder of ws) {
                VSLogger.logMessage("   => " + folder.name + " @ " + folder.uri.fsPath);
            }
        }

        let extsdir = fileSearchDirectory;
        if (extsdir.length !== 0) {
            VSLogger.logMessage("  Combined Workspace and CopyBook Folders to search:");
            for (const sdir of extsdir) {
                VSLogger.logMessage("   => " + sdir);
            }
        }

        extsdir = invalidSearchDirectory;
        if (extsdir.length !== 0) {
            VSLogger.logMessage("  Invalid CopyBook directories (" + extsdir.length + ")");
            for (const sdir of extsdir) {
                VSLogger.logMessage("   => " + sdir);
            }
        }

        VSLogger.logMessage("");
    }

    const checkForExtensionConflictsMessage = checkForExtensionConflicts();

    // display the message
    if (checkForExtensionConflictsMessage.length !== 0) {
        VSLogger.logMessage(checkForExtensionConflictsMessage);
    }

    if (checkForExtensionConflictsMessage.length !== 0 && settings.ignore_unsafe_extensions === false && messageBoxDone === false) {
        messageBoxDone = true;

        window.showInformationMessage(
            "COBOL Extension has located duplicate or conflicting functionality",
            { modal: true },
            "More information?").then(function (data) {
                if (data === "More information?") {
                    VSLogger.logChannelSetPreserveFocus(false);
                }
            });
    }
}


export function getCurrentContext(): ExtensionContext {
    return currentContext;
}


export async function activate(context: ExtensionContext): Promise<void> {
    currentContext = context;
    const settings: ICOBOLSettings = VSCOBOLConfiguration.reinit();

    // re-init if something gets installed or removed
    const onExtChange = vscode.extensions.onDidChange(() => {
        activateLogChannelAndPaths(true, settings, false);
        VSLogger.logMessage("extensions changed");
    });
    context.subscriptions.push(onExtChange);

    const onDidChangeConfiguration = workspace.onDidChangeConfiguration((event: ConfigurationChangeEvent) => {
        const updated = event.affectsConfiguration("coboleditor");
        const md_syms = event.affectsConfiguration("coboleditor.metadata_symbols");
        const md_eps = event.affectsConfiguration("coboleditor.metadata_entrypoints");
        const md_types = event.affectsConfiguration("coboleditor.metadata_types");
        const md_metadata_files = event.affectsConfiguration("coboleditor.metadata_files");
        const md_metadata_knowncopybooks = event.affectsConfiguration("coboleditor.metadata_knowncopybooks");
        const enable_semantic_token_provider = event.affectsConfiguration("coboleditor.enable_semantic_token_provider");
        const maintain_metadata_recursive_search = event.affectsConfiguration("coboleditor.maintain_metadata_recursive_search");

        if (updated) {
            const settings: ICOBOLSettings = VSCOBOLConfiguration.reinit();
            if (!md_syms && !md_eps && !md_types && !md_metadata_files && !md_metadata_knowncopybooks && !enable_semantic_token_provider) {
                clearCOBOLCache();
                activateLogChannelAndPaths(true, settings, true);
                VSSourceTreeViewHandler.setupSourceViewTree(settings, true);
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
                vscode.window.showInformationMessage("The configuration setting 'coboleditor.enable_semantic_token_provider' has changed but you may not see the affects until you have either close/reload your documents or restarted this session");
            }

            if (md_metadata_knowncopybooks) {
                COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, true);
            }

            if (maintain_metadata_recursive_search) {
                COBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
            }
        }
    });
    context.subscriptions.push(onDidChangeConfiguration);

    const collection = languages.createDiagnosticCollection("cobolDiag");
    const linter = new CobolLinterProvider(collection, settings);
    const cobolfixer = new CobolLinterActionFixer();
    activateLogChannelAndPaths(true, settings, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings, settings.metadata_symbols, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings, settings.metadata_entrypoints, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings, settings.metadata_types, false);
    COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(settings, VSExternalFeatures, settings.metadata_files, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalKnownCopybooksFromArray(settings, settings.metadata_knowncopybooks, false);

    const insertIgnoreCommentLineCommand = commands.registerCommand("cobolplugin.insertIgnoreCommentLine", function (docUri: vscode.Uri, offset: number, code: string) {
        cobolfixer.insertIgnoreCommentLine(docUri, offset, code);
    });

    const move2pdCommand = commands.registerCommand("cobolplugin.move2pd", function () {
        cobolProgram.move2pd();
    });

    const move2ddCommand = commands.registerCommand("cobolplugin.move2dd", function () {
        cobolProgram.move2dd();
    });

    const move2wsCommand = commands.registerCommand("cobolplugin.move2ws", function () {
        cobolProgram.move2ws();
    });

    const move2anyforwardCommand = commands.registerCommand("cobolplugin.move2anyforward", function () {
        cobolProgram.move2anyforward();
    });

    const move2anybackwardsCommand = commands.registerCommand("cobolplugin.move2anybackwards", function () {
        cobolProgram.move2anybackwards();
    });

    const tabCommand = commands.registerCommand("cobolplugin.tab", function () {
        tabstopper.processTabKey(true);
    });

    const unTabCommand = commands.registerCommand("cobolplugin.revtab", function () {
        tabstopper.processTabKey(false);
    });

    const commentLine = commands.registerCommand("cobolplugin.commentline", function () {
        if (window.activeTextEditor !== undefined) {
            const langid = window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                if (settings.line_comment) {
                    commenter.processCommentLine();
                } else {
                    commands.executeCommand("editor.action.commentLine");
                }
            }
            return;
        }

        commands.executeCommand("editor.action.commentLine");
    });

    const changeSourceFormat = commands.registerCommand("cobolplugin.change_source_format", function () {
        // margindecorations.changeSourceFormat();
        const action = "Open Settings";
        window.showWarningMessage("Change coboleditor setting?", action).then((selection) => {
            if (action === selection) {
                const ws = workspace.workspaceFile;
                if (ws === undefined || ws === null) {
                    commands.executeCommand("workbench.action.openGlobalSettings");
                }
                else {
                    commands.executeCommand("workbench.action.openWorkspaceSettings");
                }
            }
        });
    });

    const changeLanguageToAcu = commands.registerCommand("cobolplugin.change_lang_to_acu", function () {
        const act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "ACUCOBOL");
    });

    const changeLanguageToCOBOL = commands.registerCommand("cobolplugin.change_lang_to_cobol", function () {
        const act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "COBOL");
    });

    const checkWorkspaceForMissingCopybookDirs = commands.registerCommand("cobolplugin.checkWorkspaceForMissingCopybookDirs", async () => {
        await VSCOBOLSourceScannerTools.checkWorkspaceForMissingCopybookDirs();
    });

    const processAllFilesInWorkspaceOutOfProcessOnStartup = commands.registerCommand("cobolplugin.processAllFilesInWorkspaceOnStartup", async () => {
        await VSCobScanner.processAllFilesInWorkspaceOutOfProcess(false, false, -1);
    });

    const processAllFilesInWorkspaceOutOfProcess = commands.registerCommand("cobolplugin.processAllFilesInWorkspace", async () => {

        if (InMemoryGlobalSymbolCache.defaultCallableSymbols.size < 500) {
            VSCobScanner.processAllFilesInWorkspaceOutOfProcess(true, false, -1);
            return;
        }

        window.showQuickPick(["Yes", "No"], { placeHolder: "Your workspace is large, do you want to extra threads for your metadata scan?" }).then(function (data) {
            if (data === "Yes") {
                const cpuCount = os.cpus().length;
                const defCpuCount = cpuCount >= 4 ? (cpuCount / 2) : cpuCount;
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
                    VSCobScanner.processAllFilesInWorkspaceOutOfProcess(true, true, threadCount);

                });

            } else {
                VSCobScanner.processAllFilesInWorkspaceOutOfProcess(true, false, -1);
            }
        });
    });

    const clearGlobalCache = commands.registerCommand("cobolplugin.clearGlobalCache", function () {
        window.showQuickPick(["Yes", "No"], { placeHolder: "Are you sure you want to clear the metadata?" }).then(function (data) {
            if (data === "Yes") {
                COBOLUtils.clearGlobalCache();
                VSLogger.logMessage("Metadata cache cleared");
            }
        });
    });
    context.subscriptions.push(clearGlobalCache);

    const onDidChangeWorkspaceFolders = workspace.onDidChangeWorkspaceFolders(async () => {
        const settings: ICOBOLSettings = VSCOBOLConfiguration.reinit();

        activateLogChannelAndPaths(false, settings, true);
    });
    context.subscriptions.push(onDidChangeWorkspaceFolders);

    // default to on.. but only when "extend mf" is enabled via "when" clause.. 
    vscode.commands.executeCommand("setContext", "coboleditor.enable_migrate2mf_tasks", true);

    // handle Micro Focus .lst files!
    const onDidOpenTextDocumentHandler = workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {

        VSExtensionUtils.flip_plaintext(doc);

        //no metadata, then seed it work basic implicit program-id symbols based on the files in workspace
        const ws = getVSWorkspaceFolders();
        if (ws !== undefined) {
            if (isSupportedLanguage(doc)) {
                await COBOLUtils.populateDefaultCallableSymbols(settings, false);
                await vscode.commands.executeCommand<vscode.SymbolInformation[]>("vscode.executeDocumentSymbolProvider", doc.uri);
            }
        }
    });
    context.subscriptions.push(onDidOpenTextDocumentHandler);

    const onDidCloseTextDocumentHandler = workspace.onDidCloseTextDocument(async (doc: vscode.TextDocument) => {
        if (isSupportedLanguage(doc)) {
            VSCOBOLSourceScanner.removeCachedObject(doc, settings);
        }
    });
    context.subscriptions.push(onDidCloseTextDocumentHandler);

    /* flip any already opened docs */
    for (let docid = 0; docid < workspace.textDocuments.length; docid++) {
        VSExtensionUtils.flip_plaintext(workspace.textDocuments[docid]);
    }

    VSSourceTreeViewHandler.setupSourceViewTree(settings, false);

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

    context.subscriptions.push(checkWorkspaceForMissingCopybookDirs);
    context.subscriptions.push(processAllFilesInWorkspaceOutOfProcess);
    context.subscriptions.push(processAllFilesInWorkspaceOutOfProcessOnStartup);

    context.subscriptions.push(COBOLDocumentationCommentHandler.register());
    context.subscriptions.push(COBOLCaseFormatter.register(settings));

    context.subscriptions.push(insertIgnoreCommentLineCommand);

    const copyBookProvider = languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const ccbp = new opencopybook.COBOLCopyBookProvider(VSExternalFeatures);
            return ccbp.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(copyBookProvider);

    const sourcedefProvider = languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(sourcedefProvider);

    const COBOLCallTargetProviderProvider = languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csdp = new COBOLCallTargetProvider(VSExternalFeatures);
            return csdp.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(COBOLCallTargetProviderProvider);

    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings), new CobolReferenceProvider()));
    context.subscriptions.push(languages.registerCodeActionsProvider(VSExtensionUtils.getAllCobolSelectors(settings), cobolfixer));

    const jclSelectors = [
        { scheme: "file", language: "JCL" }
    ];
    const completionJCLItemProvider = new KeywordAutocompleteCompletionItemProvider(false);
    const completionJCLItemProviderDisposable = languages.registerCompletionItemProvider(jclSelectors, completionJCLItemProvider);
    context.subscriptions.push(completionJCLItemProviderDisposable);

    const keywordProvider = new KeywordAutocompleteCompletionItemProvider(true);
    const keywordProviderDisposible = languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings), keywordProvider);
    context.subscriptions.push(keywordProviderDisposible);

    if (settings.outline) {
        const jclDocumentSymbolProvider = new JCLDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(jclSelectors, jclDocumentSymbolProvider));

        /* TODO: add .DIR keywords too */
        const symbolInformationProvider = new CobolSymbolInformationProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllCobolSelectors(settings), symbolInformationProvider));

        // const documentSymbolProvider2 = new CobolDocumentSymbolProvider();
        // context.subscriptions.push(languages.registerDocumentSymbolProvider(VSExtensionUtils.getAllCobolSelectors(settings), documentSymbolProvider2));
    }

    const cobolProvider = new CobolSourceCompletionItemProvider(settings, VSExternalFeatures);
    const cobolProviderDisposible = languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings), cobolProvider);
    context.subscriptions.push(cobolProviderDisposible);

    // const cobolCommentProvider = new CobolCommentProvider(VSCOBOLConfiguration.get());
    // const cobolCommentProviderDisposible = languages.registerCompletionItemProvider(allCobolSelectors, cobolCommentProvider);
    // context.subscriptions.push(cobolCommentProviderDisposible);

    /* hover provider */
    // const disposable4hover_more_info = languages.registerHoverProvider(allCobolSelectors, {
    //     // eslint-disable-next-line @typescript-eslint/no-unused-vars
    //     provideHover(document: vscode.DocumentSelector, position: vscode.HoverProvider, token: vscode.CancellationToken) {
    //         const txt = document.getText(document.getWordRangeAtPosition(position));
    //         const txtTarget: CallTarget | undefined = getCallTarget(txt);
    //         if (txtTarget !== undefined) {
    //             return new Hover("### " + txtTarget.api + "\n" + txtTarget.description + "\n\n#### [More information?](" + txtTarget.url + ")");
    //         }
    //     }
    // });
    // context.subscriptions.push(disposable4hover_more_info);

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

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    workspace.onDidChangeTextDocument(event => {
        if (!window.activeTextEditor) {
            return;
        }
        updateDecorations(window.activeTextEditor);
        linter.updateLinter(window.activeTextEditor.document);
    }, null, context.subscriptions);

    if (window.activeTextEditor !== undefined) {
        updateDecorations(window.activeTextEditor);
        linter.updateLinter(window.activeTextEditor.document);
    }

    progressStatusBarItem.command = "cobolplugin.showCOBOLChannel";
    progressStatusBarItem.hide();
    context.subscriptions.push(progressStatusBarItem);


    // Open context menu on current file
    const disposable4mfurun = vscode.commands.registerCommand("cobolplugin.mfurunMenu", function (fileUri) {

        if (unitTestTerminal === undefined) {
            unitTestTerminal = vscode.window.createTerminal(terminalName);
        }

        unitTestTerminal.show(true);
        const enableAnsiColor = COBOLUtils.getMFUnitAnsiColorConfig();

        const properties = propertiesReader(fileUri.fsPath);
        const prefRunner = properties.get("global.preferred-runner");
        unitTestTerminal.sendText(prefRunner + " -show-progress " +
            (enableAnsiColor ? " -dc:ansi " : " ") +
            fileUri.fsPath);
    });
    context.subscriptions.push(disposable4mfurun);


    const disposable4debugterminal = vscode.commands.registerCommand("cobolplugin.runDebugCommand", function (si: SourceItem) {
        if (si !== undefined) {
            VSSourceTreeViewHandler.actionSourceViewItemFunction(si, true);
        }
    });
    context.subscriptions.push(disposable4debugterminal);

    const disposable4terminal = vscode.commands.registerCommand("cobolplugin.runCommand", function (si: SourceItem) {
        if (si !== undefined) {
            VSSourceTreeViewHandler.actionSourceViewItemFunction(si, false);
        }
    });
    context.subscriptions.push(disposable4terminal);

    //
    const removeAllCommentsCommand = vscode.commands.registerCommand("cobolplugin.removeAllComments", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.RemoveComments(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeAllCommentsCommand);

    const removeIdentificationAreaCommand = vscode.commands.registerCommand("cobolplugin.removeIdentificationArea", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.RemoveIdentificationArea(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeIdentificationAreaCommand);

    const removeColumnNumbersCommand = vscode.commands.registerCommand("cobolplugin.removeColumnNumbers", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.removeColumnNumbers(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeColumnNumbersCommand);

    const makeKeywordsLowercaseCommands = vscode.commands.registerCommand("cobolplugin.makeKeywordsLowercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.LowerCase, langid);
            }
        }
    });
    context.subscriptions.push(makeKeywordsLowercaseCommands);

    const makeKeywordsUppercaseCommands = vscode.commands.registerCommand("cobolplugin.makeKeywordsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makeKeywordsUppercaseCommands);

    const makeKeywordsCamelCaseCommands = vscode.commands.registerCommand("cobolplugin.makeKeywordsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.CamelCase, langid);
            }
        }
    });
    context.subscriptions.push(makeKeywordsCamelCaseCommands);

    const makeFieldsLowercaseCommand = vscode.commands.registerCommand("cobolplugin.makeFieldsLowercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makeFieldsLowercaseCommand);

    const makeFieldsUppercaseCommand = vscode.commands.registerCommand("cobolplugin.makeFieldsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makeFieldsUppercaseCommand);

    const makeFieldsCamelCaseCommand = vscode.commands.registerCommand("cobolplugin.makeFieldsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.CamelCase, langid);
            }
        }
    });
    context.subscriptions.push(makeFieldsCamelCaseCommand);

    const makePerformTargetsLowerCaseCommand = vscode.commands.registerCommand("cobolplugin.makePerformTargetsLowerCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.LowerCase, langid);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsLowerCaseCommand);

    const makePerformTargetsUpperCaseCommand = vscode.commands.registerCommand("cobolplugin.makePerformTargetsUpperCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsUpperCaseCommand);

    const makePerformTargetsCamelCaseCommand = vscode.commands.registerCommand("cobolplugin.makePerformTargetsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.CamelCase, langid);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsCamelCaseCommand);

    const extractSelectionToParagraphCommand = vscode.commands.registerCommand("cobolplugin.extractSelectionToParagraph", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, true);
            }
        }
    });
    context.subscriptions.push(extractSelectionToParagraphCommand);

    const extractSelectionToSectionCommand = vscode.commands.registerCommand("cobolplugin.extractSelectionToSection", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, false);
            }
        }
    });
    context.subscriptions.push(extractSelectionToSectionCommand);

    const extractSelectionToCopybookCommand = vscode.commands.registerCommand("cobolplugin.extractSelectionToCopybook", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                VSCOBOLFileUtils.extractSelectionToCopybook(vscode.window.activeTextEditor, VSExternalFeatures);
            }
        }
    });
    context.subscriptions.push(extractSelectionToCopybookCommand);

    // const showDocumentationCommand = vscode.commands.registerCommand('cobolplugin.showDocumentation', () => {
    //     if (vscode.window.activeTextEditor) {
    //         const langid = vscode.window.activeTextEditor.document.languageId;

    //         if (langid === 'COBOL' ||  langid === 'ACUCOBOL') {
    //             COBOLDocumentationGenerator.showCOBOLDOCDocumentation(vscode.window.activeTextEditor, settings);
    //         }
    //     }
    // });
    // context.subscriptions.push(showDocumentationCommand);

    const migrateCopybooksToWorkspaceCommand = vscode.commands.registerCommand("cobolplugin.migrateCopybooksToWorkspace", () => {
        COBOLUtils.migrateCopybooksToWorkspace();
    });
    context.subscriptions.push(migrateCopybooksToWorkspaceCommand);

    const showCOBOLChannel = vscode.commands.registerCommand("cobolplugin.showCOBOLChannel", () => {
        VSLogger.logChannelSetPreserveFocus(true);
    });
    context.subscriptions.push(showCOBOLChannel);

    const resequenceColumnNumbersCommands = vscode.commands.registerCommand("cobolplugin.resequenceColumnNumbers", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {

                vscode.window.showInputBox({
                    prompt: "Enter start line number and increment",
                    validateInput: (text: string): string | undefined => {
                        if (!text || text.indexOf(" ") === -1) {
                            return "You must enter two numbers";
                        } else {
                            return undefined;
                        }
                    }
                }).then(value => {
                    // leave early
                    if (value === undefined) {
                        return;
                    }
                    const values: string[] = value.split(" ");
                    const startValue: number = Number.parseInt(values[0], 10);
                    const incrementValue: number = Number.parseInt(values[1], 10);
                    if (startValue >= 0 && incrementValue >= 1) {
                        COBOLUtils.resequenceColumnNumbers(vscode.window.activeTextEditor, startValue, incrementValue);
                    } else {
                        vscode.window.showErrorMessage("Sorry invalid re-sequence given");
                    }
                });
            }
        }
    });
    context.subscriptions.push(resequenceColumnNumbersCommands);

    if (workspace.isTrusted) {
        bldscriptTaskProvider = vscode.tasks.registerTaskProvider(BldScriptTaskProvider.BldScriptType, new BldScriptTaskProvider());
        context.subscriptions.push(bldscriptTaskProvider);
    }

    const provider = VSSemanticProvider.provider();
    vscode.languages.registerDocumentSemanticTokensProvider(VSExtensionUtils.getAllCobolSelectors(settings), provider, VSSemanticProvider.getLegend());

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const actionCodelens = commands.registerCommand("coboleditor.ppcodelenaction", (args: string) => {
        VSPPCodeLens.actionCodeLens(args);
    });
    context.subscriptions.push(actionCodelens);
    const codelensProvider = new VSPPCodeLens();
    languages.registerCodeLensProvider(VSExtensionUtils.getAllCobolSelectors(settings), codelensProvider);

    const indentToCursorCommand = commands.registerCommand("cobolplugin.indentToCursor", () => {
        COBOLUtils.indentToCursor();
    });
    context.subscriptions.push(indentToCursorCommand);

    const leftAdjustLineCommand = commands.registerCommand("cobolplugin.leftAdjustLine", () => {
        COBOLUtils.leftAdjustLine();
    });
    context.subscriptions.push(leftAdjustLineCommand);
  

    const transposeCommand = vscode.commands.registerTextEditorCommand("cobolplugin.transposeSelection", (textEditor, edit) => {
        COBOLUtils.transposeSelection(textEditor, edit);
    });
    context.subscriptions.push(transposeCommand);

    if (settings.process_metadata_cache_on_start) {
        try {
            if (settings.maintain_metadata_cache) {
                commands.executeCommand("cobolplugin.processAllFilesInWorkspaceOnStartup");
            }
        } catch {
            // just incase
        }
    }

    openChangeLog();
}

export async function deactivateAsync(): Promise<void> {
    COBOLUtils.saveGlobalCacheToWorkspace(VSCOBOLConfiguration.get());
}


export async function deactivate(): Promise<void> {
    if (bldscriptTaskProvider) {
        bldscriptTaskProvider.dispose();
    }
    await deactivateAsync();
}
