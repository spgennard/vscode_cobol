'use strict';
import * as path from 'path';
import * as fs from 'fs';
import * as vscode from "vscode";
import os from 'os';

import { commands, workspace, StatusBarItem, StatusBarAlignment, ExtensionContext, languages, TextDocument, Position, CancellationToken, ProviderResult, Definition, window, OutputChannel, extensions, ViewColumn, ConfigurationChangeEvent } from 'vscode';
import * as cobolProgram from './cobolprogram';
import * as tabstopper from './tabstopper';
import * as opencopybook from './opencopybook';
import * as commenter from './commenter';

import { COBOLDocumentationCommentHandler } from './doccomment';
import { KeywordAutocompleteCompletionItemProvider } from './keywordprovider';
import { enableMarginCobolMargin, isEnabledViaWorkspace4cobol } from './margindecorations';
import { jclStatements } from "./keywords/jclstatements";
import { acuKeywords, cobolKeywords, cobolProcedureKeywords, cobolRegisters, cobolStorageKeywords } from "./keywords/cobolKeywords";
import { CobolDocumentSymbolProvider, JCLDocumentSymbolProvider } from './symbolprovider';

import updateDecorations from './margindecorations';
import { COBOLFileUtils } from './opencopybook';

import VSCOBOLSourceScanner, { clearCOBOLCache } from './vscobolscanner';
import { VSCOBOLConfiguration } from './configuration';
import { CobolReferenceProvider } from './cobolreferenceprovider';
import { CobolLinterProvider, CobolLinterActionFixer } from './cobollinter';
import { SourceViewTree } from './sourceviewtree';
import { CobolSourceCompletionItemProvider } from './cobolprovider';
import { COBOLUtils, FoldStyle, FoldAction } from './cobolutils';
import { COBOLSettings, ICOBOLSettings } from './iconfiguration';

// eslint-disable-next-line @typescript-eslint/no-var-requires
const propertiesReader = require('properties-reader');

import util from 'util';
import { getWorkspaceFolders } from './cobolfolders';
// import { COBOLDocumentationGenerator } from './coboldocgenerator';
// import { CobolCommentProvider } from './cobolcommentprovider';
import { COBOLSourceScannerUtils } from './cobolsourcescannerutils';
import { COBOLSourceDefinition } from './sourcedefinitionprovider';
import { CachedCOBOLSourceDefinition } from './cachedsourcedefinitionprovider';
import { CacheDirectoryStrategy } from './externalfeatures';
import { VSExternalFeatures } from './vsexternalfeatures';
import { VSCobScanner } from './vscobscanner';
import { BldScriptTaskProvider } from './bldTaskProvider';
import { COBOLCaseFormatter } from './caseformatter';
import { COBOLCallTargetProvider } from './cobolcalltargetprovider';
import { COBOLWorkspaceSymbolCacheHelper } from './cobolworkspacecache';
import { COBOLPreprocessorHelper } from './cobolsourcescanner';
import { SourceItem } from './sourceItem';
import { VSSemanticProvider } from './vssemanticprovider';

export const progressStatusBarItem: StatusBarItem = window.createStatusBarItem(StatusBarAlignment.Left);

let currentContext: ExtensionContext;
const COBOLOutputChannel: OutputChannel = window.createOutputChannel("COBOL");

let sourceTreeView: SourceViewTree | undefined = undefined;
let sourceTreeWatcher: vscode.FileSystemWatcher | undefined = undefined;

let bldscriptTaskProvider: vscode.Disposable | undefined;


export function getAllCobolSelectors(): vscode.DocumentSelector {
    return [
        { scheme: 'file', language: 'COBOL_MF_LISTFILE' },
        { scheme: 'file', language: 'COBOL' },
        { scheme: 'file', language: 'COBOLIT' },
        { scheme: 'file', language: 'ACUCOBOL' }    // alias
    ];
}


export function isKnownCOBOLLanguageId(langid: string): boolean {
    if (langid === 'COBOL' || langid === 'ACUCOBOL' || langid === 'COBOLIT') {
        return true;
    }
    return false
}

export const ExternalFeatures = new VSExternalFeatures();

export const currentHostInformation = `${os.hostname()}/${os.userInfo().username}`;

export function getCombinedCopyBookSearchPath(): string[] {
    return fileSearchDirectory;
}

export function isDirectory(sdir: string): boolean {
    try {
        const f = fs.statSync(sdir, { bigint: true });
        if (f && f.isDirectory()) {
            return true;
        }
    }
    catch {
        return false;
    }
    return false;
}



export class COBOLStatUtils {

    public static async isFileASync(sdir: string): Promise<boolean> {
        try {
            const statSDir = await workspace.fs.stat(vscode.Uri.file(sdir));
            return (statSDir.type & vscode.FileType.File) === vscode.FileType.File;
        } catch {
            return false;
        }
    }

    public static isFile(sdir: string): boolean {
        try {
            if (fs.existsSync(sdir)) {
                // not on windows, do extra check for +x perms (protects exe & dirs)
                if (!COBOLFileUtils.isWin32) {
                    try {
                        fs.accessSync(sdir, fs.constants.F_OK | fs.constants.X_OK);
                        return false;
                    }
                    catch {
                        return true;
                    }
                }

                return true;
            }
        }
        catch {
            return false;
        }
        return false;
    }

    public static isPathInWorkspace(ddir: string): boolean {
        const ws = getWorkspaceFolders();
        if (workspace === undefined || ws === undefined) {
            return false;
        }

        const fullPath = path.normalize(ddir);
        for (const folder of ws) {
            if (folder.uri.fsPath === fullPath) {
                return true;
            }
        }

        return false;
    }

    public static getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        const ws = getWorkspaceFolders();
        if (workspace === undefined || ws === undefined) {
            return undefined;
        }
        for (const folder of ws) {
            if (folder.uri.scheme === 'file') {
                const folderPath = folder.uri.path;
                const possibleFile = path.join(folderPath, sdir);
                if (COBOLStatUtils.isFile(possibleFile)) {
                    const stat4src = fs.statSync(possibleFile, { bigint: true });
                    if (sdirMs === stat4src.mtimeMs) {
                        return possibleFile;
                    }
                }
            }
        }

        return undefined;
    }

    public static getShortWorkspaceFilename(ddir: string): string | undefined {
        const ws = getWorkspaceFolders();
        if (workspace === undefined || ws === undefined) {
            return undefined;
        }

        const fullPath = path.normalize(ddir);
        let bestShortName = "";
        for (const folder of ws) {
            if (folder.uri.scheme === 'file') {
                const folderPath = folder.uri.path;
                if (fullPath.startsWith(folderPath)) {
                    const possibleShortPath = fullPath.substr(1 + folderPath.length);
                    if (bestShortName.length === 0) {
                        bestShortName = possibleShortPath;
                    } else {
                        if (possibleShortPath.length < possibleShortPath.length) {
                            bestShortName = possibleShortPath;
                        }
                    }
                }
            }
        }

        return bestShortName.length === 0 ? undefined : bestShortName;
    }
}


let fileSearchDirectory: string[] = [];
let invalidSearchDirectory: string[] = [];
let unitTestTerminal: vscode.Terminal | undefined = undefined;
let commandTerminal: vscode.Terminal | undefined = undefined;
const terminalName = "UnitTest";
const commandTerminalName = "COBOL Application";


const blessed_extensions: string[] = [
    "HCLTechnologies.hclappscancodesweep",      // code scanner
];

let blessed_extension_active = false;

const known_problem_extensions: string[] = [
    "OlegKunitsyn.gnucobol-debug",              // debugger
    "BroadcomMFD.debugger-for-mainframe",       // debugger
    "rechinformatica.rech-cobol-debugger",      // debugger
    "BroadcomMFD.ccf"                           // control flow extension
];

function checkForExtensionConflicts(settings: ICOBOLSettings): string {
    let dupExtensionMessage = "";
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    let grab_info_for_ext: vscode.Extension<any> | undefined = undefined;
    let reason = "";

    dupExtensionMessage = "";

    for (const ext of extensions.all) {
        if (ext !== undefined && ext.packageJSON !== undefined) {
            let continue_now = blessed_extension_active;
            if (ext.packageJSON.id !== undefined) {
                if (ext.packageJSON.id === 'bitlang.cobol') {
                    continue;
                }
                for (const blessed_extension of blessed_extensions) {
                    if (blessed_extension === ext.packageJSON.id) {
                        blessed_extension_active = continue_now = true;
                    }
                }
                for (const known_problem_extension of known_problem_extensions) {
                    if (known_problem_extension === ext.packageJSON.id) {
                        grab_info_for_ext = ext;
                        reason = "debugger support is problematic with this extension";
                        continue_now = true;
                    }
                }
                if (continue_now) {
                    continue;
                }
            }
        }


        if (ext.packageJSON.contributes !== undefined) {
            const grammarsBody = ext.packageJSON.contributes.grammars;
            if (grammarsBody !== undefined) {
                if (grammarsBody instanceof Object) {
                    for (const key in grammarsBody) {
                        const element = grammarsBody[key];
                        if (element.language !== undefined) {
                            const l = `${element.language}`.toUpperCase();
                            if (l === 'COBOL') {
                                grab_info_for_ext = ext;
                                reason = "contributes grammar";
                            }
                        }
                    }
                }
            }

            const languagesBody = ext.packageJSON.contributes.languages;
            if (languagesBody !== undefined) {
                if (languagesBody instanceof Object) {
                    for (const key in languagesBody) {
                        const languageElement = languagesBody[key];
                        if (languageElement.id !== undefined) {
                            const l = `${languageElement.id}`.toUpperCase();
                            if (l === 'COBOL') {
                                grab_info_for_ext = ext;
                                if (reason.length !== 0) {
                                    reason += ", ";
                                }
                                reason += "contributes language id";
                            }
                        }
                    }
                }
            }
        }

        if (grab_info_for_ext !== undefined) {
            if (dupExtensionMessage.length === 0) {
                if (settings.ignore_unsafe_extensions === false) {
                    dupExtensionMessage += " The COBOL Extension from bitlang may produce duplicate results due to\n";
                    dupExtensionMessage += "  other extensions for COBOL being present.\n\n";
                    dupExtensionMessage += " If you do not want see this warning message, change the setting\n";
                    dupExtensionMessage += "  coboleditor.ignore_unsafe_extensions to true and restart vscode\n\n";
                    dupExtensionMessage += "NOTE: If you think the extension is being reported as 'unsafe' unfairly\n";
                    dupExtensionMessage += "      please raise issue @ https://github.com/spgennard/vscode_cobol/issues\n";
                }
            }
            dupExtensionMessage += `\nThe extension ${grab_info_for_ext.packageJSON.name} from ${grab_info_for_ext.packageJSON.publisher} has conflicting functionality\n`;

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
                if (reason.length !== 0) {
                    dupExtensionMessage += ` Reason        : ${reason}\n`;
                }
                if (dupExtensionMessage.length !== 0) {
                    dupExtensionMessage += "\n";
                }
            }
        }
    }

    return dupExtensionMessage;
}

let checkForExtensionConflictsMessage = "";
let messageBoxDone = false;

function initExtensionSearchPaths(config: ICOBOLSettings, checkForConflicts: boolean) {

    if (checkForConflicts) {
        checkForExtensionConflictsMessage = "";
        checkForExtensionConflictsMessage = checkForExtensionConflicts(config);
        if (checkForExtensionConflictsMessage.length !== 0 && config.ignore_unsafe_extensions === false && messageBoxDone === false) {
            messageBoxDone = true;
            window.showInformationMessage("COBOL Extension that has found duplicate or conflicting functionality", { modal: true });
        }
    } else {
        checkForExtensionConflictsMessage = "";
    }
    fileSearchDirectory.length = 0;

    const extsdir = config.copybookdirs;
    invalidSearchDirectory = config.invalid_copybookdirs;
    invalidSearchDirectory.length = 0;

    // step 1 look through the copybook default dirs for "direct" paths and include them in search path
    for (const ddir of extsdir) {
        if (config.disable_unc_copybooks_directories && COBOLFileUtils.isNetworkPath(ddir)) {
            logMessage(" Copybook directory " + ddir + " has been marked as invalid, as it is a unc filename");
            invalidSearchDirectory.push(ddir);
        }
        else if (COBOLFileUtils.isDirectPath(ddir)) {
            logWarningMessage(` non portable copybook directory ${ddir} defined`);
            if (workspace !== undefined && getWorkspaceFolders() !== undefined) {
                if (COBOLStatUtils.isPathInWorkspace(ddir) === false) {
                    if (COBOLFileUtils.isNetworkPath(ddir)) {
                        logMessage(" The directory " + ddir + " for performance should be part of the workspace");
                    }
                }
            }

            const startTime = performance_now();
            if (isDirectory(ddir)) {
                const totalTimeInMS = performance_now() - startTime;
                const timeTaken = totalTimeInMS.toFixed(2);
                if (totalTimeInMS <= 2000) {
                    fileSearchDirectory.push(ddir);
                } else {
                    logMessage(" Slow copybook directory dropped " + ddir + " as it took " + timeTaken + "ms");
                    invalidSearchDirectory.push(ddir);
                }
            } else {
                invalidSearchDirectory.push(ddir);
            }
        }
    }

    // step 2
    const ws = getWorkspaceFolders();
    if (ws !== undefined) {
        for (const folder of ws) {
            // place the workspace folder in the copybook path

            fileSearchDirectory.push(folder.uri.fsPath);

            /* now add any extra directories that are below this workspace folder */
            for (const extdir of extsdir) {
                try {
                    if (COBOLFileUtils.isDirectPath(extdir) === false) {
                        const sdir = path.join(folder.uri.fsPath, extdir);

                        if (isDirectory(sdir)) {
                            if (COBOLFileUtils.isNetworkPath(sdir) && COBOLStatUtils.isPathInWorkspace(sdir) === false) {
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

function activateLogChannelAndPaths(hide: boolean, settings: ICOBOLSettings, quiet: boolean) {
    if (!quiet) {
        if (hide) {
            COBOLOutputChannel.hide();
        } else {
            logChannelSetPreserveFocus(true);
        }
    }
    COBOLOutputChannel.clear();

    const thisExtension = extensions.getExtension("bitlang.cobol");
    if (thisExtension !== undefined) {
        logMessage(`VSCode version                      : ${vscode.version}`);
        logMessage(` Platform                           : ${os.platform}`);
        logMessage(` Architecture                       : ${os.arch}`);
        logMessage("Extension Information:");
        logMessage(` Extension path                     : ${thisExtension.extensionPath}`);
        logMessage(` Version                            : ${thisExtension.packageJSON.version}`);
        logMessage(` UNC paths disabled                 : ${settings.disable_unc_copybooks_directories}`);
        logMessage(` Parse copybook for references      : ${settings.parse_copybooks_for_references}`);
        logMessage(` Editor maxTokenizationLineLength   : ${settings.editor_maxTokenizationLineLength}`)
        logMessage(` Semantic token provider enabled    : ${settings.enable_semantic_token_provider}`);
        try {
            const editor_semanticHighlighting_enabled = workspace.getConfiguration('editor.semanticHighlighting').get<number>("enabled");
            logMessage(` editor.semanticHighlighting.enabled: ${editor_semanticHighlighting_enabled}`);
        } catch
        {
            //
        }
        try {
            const workbench_theme = workspace.getConfiguration('workbench').get<string>("colorTheme");
            logMessage(` workbench color theme              : ${workbench_theme}`);
        } catch
        {
            //
        }
        if (vscode.workspace.workspaceFile !== undefined) {
            logMessage(` Active workspacefile               : ${vscode.workspace.workspaceFile}`);
        }

        if (VSCOBOLConfiguration.isOnDiskCachingEnabled()) {
            logMessage("----------------------------------------------------------------------");
            logMessage(" Deprecated Features settings");
            logMessage("");
            logMessage(" Caching");
            logMessage(`  Cache Strategy                    : ${settings.cache_metadata}`);
            const cacheDir = VSCOBOLSourceScanner.getCacheDirectory();
            if (cacheDir !== undefined) {
                logMessage(`  Cache directory  : ${cacheDir}`);
            }
            logMessage("----------------------------------------------------------------------");
        }

        dumpPreProcInfo(settings);

    }

    initExtensionSearchPaths(settings, false);

    if (thisExtension !== undefined) {
        const ws = getWorkspaceFolders();
        if (ws !== undefined) {
            logMessage("  Workspace Folders:");
            for (const folder of ws) {
                logMessage("   => " + folder.name + " @ " + folder.uri.fsPath);
            }
        }

        let extsdir = getCombinedCopyBookSearchPath();
        if (extsdir.length !== 0) {
            logMessage("  Combined Workspace and CopyBook Folders to search:");
            for (const sdir of extsdir) {
                logMessage("   => " + sdir);
            }
        }

        extsdir = invalidSearchDirectory;
        if (extsdir.length !== 0) {
            logMessage("  Invalid CopyBook directories (" + extsdir.length + ")");
            for (const sdir of extsdir) {
                logMessage("   => " + sdir);
            }
        }

        logMessage("");
    }
}

export function dumpPreProcInfo(settings: COBOLSettings): void {
    for (const extName of settings.preprocessor_extensions) {
        if (COBOLPreprocessorHelper.preprocessorsExts.has(extName)) {
            const activePreProc = COBOLPreprocessorHelper.preprocessorsExts.get(extName);

            if (activePreProc !== undefined) {
                logMessage(` Active preprocessor              : ${activePreProc.id}`);
                logMessage(`                                  : ${activePreProc.description}`);
                logMessage(`                                  : ${activePreProc.bugReportEmail}`);
                logMessage(`                                  : ${activePreProc.bugReportUrl}`);
            }
            continue;
        }
        logMessage('');
        logMessage(` Registered preprocessor          : ${extName}`);
        logMessage('');
    }
}

export function getCurrentContext(): ExtensionContext {
    return currentContext;
}

function flip_plaintext(doc: TextDocument) {
    if (doc === undefined) {
        return;
    }

    const settings = VSCOBOLConfiguration.get();
    if ((blessed_extension_active || settings.ignore_unsafe_extensions) && doc.languageId === 'cobol') {
        vscode.languages.setTextDocumentLanguage(doc, "COBOL");
        return;
    }

    if (doc.languageId === 'plaintext' || doc.languageId === 'tsql') {  // one tsql ext grabs .lst!
        const lineCount = doc.lineCount;
        if (lineCount >= 3) {
            const firstLine = doc.lineAt((0)).text;
            const secondLine = doc.lineAt(1).text;

            if ((firstLine.length >= 1 && firstLine.charCodeAt(0) === 12) && secondLine.startsWith("* Micro Focus COBOL ")) {
                vscode.languages.setTextDocumentLanguage(doc, "COBOL_MF_LISTFILE");
                return;
            }

            //NOTE: If we have more.. refactor..
            if (firstLine.startsWith("Pro*COBOL: Release")) {
                vscode.languages.setTextDocumentLanguage(doc, "COBOL_PCOB_LISTFILE");
                return;
            }

            if ((firstLine.indexOf("ACUCOBOL-GT ") !== -1) && (firstLine.indexOf("Page:") !== -1)) {
                vscode.languages.setTextDocumentLanguage(doc, "COBOL_ACU_LISTFILE");
                return;
            }
        }
    }
}

function setupSourceViewTree(config: ICOBOLSettings, reinit: boolean) {

    if ((config.sourceview === false || reinit) && sourceTreeView !== undefined) {
        sourceTreeWatcher?.dispose();
        sourceTreeView = undefined;
    }

    if (config.sourceview && sourceTreeView === undefined) {
        sourceTreeView = new SourceViewTree(config);
        sourceTreeWatcher = workspace.createFileSystemWatcher('**/*');

        sourceTreeWatcher.onDidCreate((uri) => {
            if (sourceTreeView !== undefined) {
                sourceTreeView.checkFile(uri);
            }
        });

        sourceTreeWatcher.onDidDelete((uri) => {
            if (sourceTreeView !== undefined) {
                sourceTreeView.clearFile(uri);
            }
        });

        window.registerTreeDataProvider('flat-source-view', sourceTreeView);
        return;
    }

}

function actionSourceViewItemFunction(si: SourceItem, debug: boolean) {
    if (commandTerminal === undefined) {
        commandTerminal = vscode.window.createTerminal(commandTerminalName);
    }

    let prefRunner = "";
    let prefRunnerDebug = "";
    let fsPath = "";
    if (si !== undefined && si.uri !== undefined) {
        fsPath = si.uri.path as string;
    }

    if (COBOLFileUtils.isWin32) {
        if (fsPath.endsWith("acu")) {
            prefRunner = "wrun32";
            prefRunnerDebug = debug ? "-d " : "";
        } else if (fsPath.endsWith("int") || fsPath.endsWith("gnt")) {
            prefRunner = "cobrun";
            prefRunnerDebug = debug ? "(+A) " : "";
        }
    } else {
        // todo consider adding threaded version, cobmode
        if (fsPath.endsWith("int") || fsPath.endsWith("gnt")) {
            prefRunner = debug ? "anim" : "cobrun";
        } else if (fsPath.endsWith("acu")) {
            prefRunner = "runcbl";
            prefRunnerDebug = debug ? "-d " : "";
        }
    }

    commandTerminal.show(true);
    commandTerminal.sendText(`${prefRunner} ${prefRunnerDebug}${fsPath}`);
}

export async function activate(context: ExtensionContext): Promise<void> {
    currentContext = context;

    // re-init if something gets installed or removed
    const onExtChange = vscode.extensions.onDidChange(() => {
        const settings: ICOBOLSettings = VSCOBOLConfiguration.init();
        activateLogChannelAndPaths(true, settings, false);
        logMessage("extensions changed");
    });
    context.subscriptions.push(onExtChange);


    const onDidChangeConfiguration = workspace.onDidChangeConfiguration((event: ConfigurationChangeEvent) => {
        const updated = event.affectsConfiguration(`coboleditor`);
        const md_syms = event.affectsConfiguration("coboleditor.metadata_symbols");
        const md_eps = event.affectsConfiguration("coboleditor.metadata_entrypoints");
        const md_types = event.affectsConfiguration("coboleditor.metadata_types");
        const md_metadata_files = event.affectsConfiguration("coboleditor.metadata_files");
        const enable_semantic_token_provider = event.affectsConfiguration("coboleditor.enable_semantic_token_provider");
        if (updated) {
            const settings: ICOBOLSettings = VSCOBOLConfiguration.init();
            if (!md_syms && !md_eps && !md_types && !md_metadata_files) {
                clearCOBOLCache();
                activateLogChannelAndPaths(true, settings, true);
                setupSourceViewTree(settings, true);
            }
            if (md_syms) {
                COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings.metadata_symbols, true);
            }

            if (md_eps) {
                COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings.metadata_entrypoints, true);
            }

            if (md_types) {
                COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings.metadata_types, true);
            }

            if (md_metadata_files) {
                COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(ExternalFeatures, settings.metadata_files, true);
            }

            if (enable_semantic_token_provider) {
                vscode.window.showInformationMessage("The configuration setting 'coboleditor.enable_semantic_token_provider' has changed but you may not see the affects until you have either close/reload your documents or restarted this session");
            }
        }
    });
    context.subscriptions.push(onDidChangeConfiguration);

    const settings: ICOBOLSettings = VSCOBOLConfiguration.init();

    const collection = languages.createDiagnosticCollection('cobolDiag');
    const linter = new CobolLinterProvider(collection, VSCOBOLConfiguration.get());
    const cobolfixer = new CobolLinterActionFixer();
    initExtensionSearchPaths(settings, true);
    activateLogChannelAndPaths(true, settings, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalCacheFromArray(settings.metadata_symbols, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalEntryCacheFromArray(settings.metadata_entrypoints, false);
    COBOLWorkspaceSymbolCacheHelper.loadGlobalTypesCacheFromArray(settings.metadata_types, false);
    COBOLWorkspaceSymbolCacheHelper.loadFileCacheFromArray(ExternalFeatures, settings.metadata_files, false);

    const insertIgnoreCommentLineCommand = commands.registerCommand("cobolplugin.insertIgnoreCommentLine", function (docUri: vscode.Uri, offset: number, code: string) {
        cobolfixer.insertIgnoreCommentLine(docUri, offset, code);
    });

    const move2pdCommand = commands.registerCommand('cobolplugin.move2pd', function () {
        cobolProgram.move2pd();
    });

    const move2ddCommand = commands.registerCommand('cobolplugin.move2dd', function () {
        cobolProgram.move2dd();
    });

    const move2wsCommand = commands.registerCommand('cobolplugin.move2ws', function () {
        cobolProgram.move2ws();
    });

    const move2anyforwardCommand = commands.registerCommand('cobolplugin.move2anyforward', function () {
        cobolProgram.move2anyforward();
    });

    const move2anybackwardsCommand = commands.registerCommand('cobolplugin.move2anybackwards', function () {
        cobolProgram.move2anybackwards();
    });

    const tabCommand = commands.registerCommand('cobolplugin.tab', function () {
        if (settings.enable_tabstop) {
            tabstopper.processTabKey(true);
        } else {
            commands.executeCommand("tab");
        }
    });

    const unTabCommand = commands.registerCommand('cobolplugin.revtab', function () {
        if (settings.enable_tabstop) {
            tabstopper.processTabKey(false);
        } else {
            commands.executeCommand("outdent");
        }
    });

    const commentLine = commands.registerCommand('cobolplugin.commentline', function () {
        if (window.activeTextEditor !== undefined) {
            const langid = window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
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

    const changeSourceFormat = commands.registerCommand('cobolplugin.change_source_format', function () {
        // margindecorations.changeSourceFormat();
        const action = 'Open Settings';
        window.showWarningMessage("Change coboleditor setting?", action).then((selection) => {
            if (action === selection) {
                const ws = workspace.getWorkspaceFolder;
                if (ws === undefined || ws === null) {
                    commands.executeCommand("workbench.action.openGlobalSettings");
                }
                else {
                    commands.executeCommand("workbench.action.openWorkspaceSettings");
                }
            }
        });
    });

    const changeLanguageToAcu = commands.registerCommand('cobolplugin.change_lang_to_acu', function () {
        const act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "ACUCOBOL");
    });

    const changeLanguageToCOBOL = commands.registerCommand('cobolplugin.change_lang_to_cobol', function () {
        const act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        languages.setTextDocumentLanguage(act.document, "COBOL");
    });

    const toggleCOBOLMargin = commands.registerCommand('cobolplugin.toggle_margin', function () {
        const act = window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        enableMarginCobolMargin(!isEnabledViaWorkspace4cobol());
        updateDecorations(act);
    });


    const checkWorkspaceForMissingCopybookDirs = commands.registerCommand('cobolplugin.checkWorkspaceForMissingCopybookDirs', async () => {
        await VSCOBOLSourceScanner.checkWorkspaceForMissingCopybookDirs();
    });

    const processAllFilesInWorkspaceOutOfProcessDeprecated = commands.registerCommand('cobolplugin.deprecated.processAllFilesInWorkspace', async () => {
        await VSCobScanner.processAllFilesInWorkspaceOutOfProcess(true, true);
    });

    const processAllFilesInWorkspaceOutOfProcess = commands.registerCommand('cobolplugin.processAllFilesInWorkspace', async () => {
        await VSCobScanner.processAllFilesInWorkspaceOutOfProcess(true, false);
    });

    const dumpMetadata = commands.registerCommand('cobolplugin.deprecated.dumpMetaData', function () {
        const cacheDirectory = VSCOBOLSourceScanner.getCacheDirectory();
        if (cacheDirectory !== undefined) {
            COBOLSourceScannerUtils.dumpMetaData(settings, cacheDirectory);
        } else {
            logMessage("Metadata caching is turned off (or invalid)");
        }
    });

    const clearMetaData = commands.registerCommand('cobolplugin.deprecated.clearMetaData', function () {
        const cacheDirectory = VSCOBOLSourceScanner.getCacheDirectory();
        if (cacheDirectory !== undefined) {
            VSCOBOLSourceScanner.clearMetaData(settings, cacheDirectory);
        } else {
            logMessage("Metadata caching is turned off (or invalid)");
        }
    });

    const onDidChangeWorkspaceFolders = workspace.onDidChangeWorkspaceFolders(() => {
        const settings: ICOBOLSettings = VSCOBOLConfiguration.init();
        activateLogChannelAndPaths(false, settings, true);
    });
    context.subscriptions.push(onDidChangeWorkspaceFolders);

    // handle Micro Focus .lst files!
    const onDidOpenTextDocumentHandler = workspace.onDidOpenTextDocument((doc) => {
        flip_plaintext(doc);
    });
    context.subscriptions.push(onDidOpenTextDocumentHandler);

    /* flip any already opened docs */
    for (let docid = 0; docid < workspace.textDocuments.length; docid++) {
        flip_plaintext(workspace.textDocuments[docid]);
    }

    setupSourceViewTree(VSCOBOLConfiguration.get(), false);

    const onDidSaveTextDocumentHandler = workspace.onDidSaveTextDocument(async (doc) => {
        if (settings.process_metadata_cache_on_file_save) {
            await VSCobScanner.processSavedFile(doc.uri.fsPath, settings);
        }
    });
    context.subscriptions.push(onDidSaveTextDocumentHandler);

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

    context.subscriptions.push(toggleCOBOLMargin);
    context.subscriptions.push(checkWorkspaceForMissingCopybookDirs);
    context.subscriptions.push(processAllFilesInWorkspaceOutOfProcessDeprecated);
    context.subscriptions.push(processAllFilesInWorkspaceOutOfProcess);

    context.subscriptions.push(dumpMetadata);
    context.subscriptions.push(clearMetaData);

    context.subscriptions.push(COBOLDocumentationCommentHandler.register());
    context.subscriptions.push(COBOLCaseFormatter.register());

    context.subscriptions.push(insertIgnoreCommentLineCommand);

    const copyBookProvider = languages.registerDefinitionProvider(getAllCobolSelectors(), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const ccbp = new opencopybook.COBOLCopyBookProvider();
            return ccbp.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(copyBookProvider);

    const sourcedefProvider = languages.registerDefinitionProvider(getAllCobolSelectors(), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(sourcedefProvider);

    if (VSCOBOLConfiguration.isOnDiskCachingEnabled()) {
        const cachedSourcedefProvider = languages.registerDefinitionProvider(getAllCobolSelectors(), {
            provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
                const csdp = new CachedCOBOLSourceDefinition();
                return csdp.provideDefinition(doc, pos, ct);
            }
        });
        context.subscriptions.push(cachedSourcedefProvider);
    }

    const COBOLCallTargetProviderProvider = languages.registerDefinitionProvider(getAllCobolSelectors(), {
        provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
            const csdp = new COBOLCallTargetProvider();
            return csdp.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(COBOLCallTargetProviderProvider);

    context.subscriptions.push(languages.registerReferenceProvider(getAllCobolSelectors(), new CobolReferenceProvider()));
    context.subscriptions.push(languages.registerCodeActionsProvider(getAllCobolSelectors(), cobolfixer));

    const jclSelectors = [
        { scheme: 'file', language: 'JCL' }
    ];
    const completionJCLItemProvider = new KeywordAutocompleteCompletionItemProvider(jclStatements, false);
    const completionJCLItemProviderDisposable = languages.registerCompletionItemProvider(jclSelectors, completionJCLItemProvider);
    context.subscriptions.push(completionJCLItemProviderDisposable);

    const allKeywords = cobolKeywords.concat(cobolStorageKeywords)
        .concat(cobolRegisters)
        .concat(cobolProcedureKeywords)
        .concat(acuKeywords);

    const allKeywordsUnique = [...new Set(allKeywords)];
    const keywordProvider = new KeywordAutocompleteCompletionItemProvider(allKeywordsUnique, true);
    const keywordProviderDisposible = languages.registerCompletionItemProvider(getAllCobolSelectors(), keywordProvider);
    context.subscriptions.push(keywordProviderDisposible);

    if (settings.outline) {
        const jclDocumentSymbolProvider = new JCLDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(jclSelectors, jclDocumentSymbolProvider));

        /* TODO: add .DIR keywords too */
        const documentSymbolProvider = new CobolDocumentSymbolProvider();
        context.subscriptions.push(languages.registerDocumentSymbolProvider(getAllCobolSelectors(), documentSymbolProvider));
    }

    const cobolProvider = new CobolSourceCompletionItemProvider(VSCOBOLConfiguration.get());
    const cobolProviderDisposible = languages.registerCompletionItemProvider(getAllCobolSelectors(), cobolProvider);
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

    progressStatusBarItem.command = "cobolplugin.showCOBOLChannel";
    progressStatusBarItem.hide();
    context.subscriptions.push(progressStatusBarItem);

    if (window.activeTextEditor !== undefined) {
        updateDecorations(window.activeTextEditor);
        linter.updateLinter(window.activeTextEditor.document);
    }

    // Open context menu on current file
    const disposable4mfurun = vscode.commands.registerCommand('cobolplugin.mfurunMenu', function (fileUri) {

        if (unitTestTerminal === undefined) {
            unitTestTerminal = vscode.window.createTerminal(terminalName);
        }

        unitTestTerminal.show(true);
        const enableAnsiColor = COBOLUtils.getMFUnitAnsiColorConfig();

        const properties = propertiesReader(fileUri.fsPath);
        const prefRunner = properties.get('global.preferred-runner');
        unitTestTerminal.sendText(prefRunner + " -show-progress " +
            (enableAnsiColor ? " -dc:ansi " : " ") +
            fileUri.fsPath);
    });
    context.subscriptions.push(disposable4mfurun);


    const disposable4debugterminal = vscode.commands.registerCommand('cobolplugin.runDebugCommand', function (si: SourceItem) {
        if (si !== undefined) {
            actionSourceViewItemFunction(si, true);
        }
    });
    context.subscriptions.push(disposable4debugterminal);

    const disposable4terminal = vscode.commands.registerCommand('cobolplugin.runCommand', function (si: SourceItem) {
        if (si !== undefined) {
            actionSourceViewItemFunction(si, false);
        }
    });
    context.subscriptions.push(disposable4terminal);

    //
    const removeAllCommentsCommand = vscode.commands.registerCommand('cobolplugin.removeAllComments', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.RemoveComments(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeAllCommentsCommand);

    const removeIdentificationAreaCommand = vscode.commands.registerCommand('cobolplugin.removeIdentificationArea', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.RemoveIdentificationArea(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeIdentificationAreaCommand);

    const removeColumnNumbersCommand = vscode.commands.registerCommand('cobolplugin.removeColumnNumbers', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.removeColumnNumbers(vscode.window.activeTextEditor);
            }
        }
    });
    context.subscriptions.push(removeColumnNumbersCommand);

    const makeKeywordsLowercaseCommands = vscode.commands.registerCommand('cobolplugin.makeKeywordsLowercase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.LowerCase);
            }
        }
    });
    context.subscriptions.push(makeKeywordsLowercaseCommands);

    const makeKeywordsUppercaseCommands = vscode.commands.registerCommand('cobolplugin.makeKeywordsUppercase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makeKeywordsUppercaseCommands);

    const makeKeywordsCamelCaseCommands = vscode.commands.registerCommand('cobolplugin.makeKeywordsCamelCase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.CamelCase);
            }
        }
    });
    context.subscriptions.push(makeKeywordsCamelCaseCommands);

    const makeFieldsLowercaseCommand = vscode.commands.registerCommand('cobolplugin.makeFieldsLowercase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makeFieldsLowercaseCommand);

    const makeFieldsUppercaseCommand = vscode.commands.registerCommand('cobolplugin.makeFieldsUppercase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makeFieldsUppercaseCommand);

    const makeFieldsCamelCaseCommand = vscode.commands.registerCommand('cobolplugin.makeFieldsCamelCase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.CamelCase);
            }
        }
    });
    context.subscriptions.push(makeFieldsCamelCaseCommand);

    const makePerformTargetsLowerCaseCommand = vscode.commands.registerCommand('cobolplugin.makePerformTargetsLowerCase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.LowerCase);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsLowerCaseCommand);

    const makePerformTargetsUpperCaseCommand = vscode.commands.registerCommand('cobolplugin.makePerformTargetsUpperCase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.UpperCase);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsUpperCaseCommand);

    const makePerformTargetsCamelCaseCommand = vscode.commands.registerCommand('cobolplugin.makePerformTargetsCamelCase', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.foldToken(vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.CamelCase);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsCamelCaseCommand);

    const extractSelectionToParagraphCommand = vscode.commands.registerCommand('cobolplugin.extractSelectionToParagraph', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, true);
            }
        }
    });
    context.subscriptions.push(extractSelectionToParagraphCommand);

    const extractSelectionToSectionCommand = vscode.commands.registerCommand('cobolplugin.extractSelectionToSection', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.extractSelectionTo(vscode.window.activeTextEditor, false);
            }
        }
    });
    context.subscriptions.push(extractSelectionToSectionCommand);

    const extractSelectionToCopybookCommand = vscode.commands.registerCommand('cobolplugin.extractSelectionToCopybook', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {
                COBOLUtils.extractSelectionToCopybook(vscode.window.activeTextEditor);
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

    const migrateCopybooksToWorkspaceCommand = vscode.commands.registerCommand('cobolplugin.migrateCopybooksToWorkspace', () => {
        COBOLUtils.migrateCopybooksToWorkspace();
    });
    context.subscriptions.push(migrateCopybooksToWorkspaceCommand);

    const showCOBOLChannel = vscode.commands.registerCommand('cobolplugin.showCOBOLChannel', () => {
        logChannelSetPreserveFocus(true);
    });
    context.subscriptions.push(showCOBOLChannel);

    const resequenceColumnNumbersCommands = vscode.commands.registerCommand('cobolplugin.resequenceColumnNumbers', () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (isKnownCOBOLLanguageId(langid)) {

                vscode.window.showInputBox({
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

    bldscriptTaskProvider = vscode.tasks.registerTaskProvider(BldScriptTaskProvider.BldScriptType, new BldScriptTaskProvider());
    context.subscriptions.push(bldscriptTaskProvider);

    const provider = VSSemanticProvider.provider();
    vscode.languages.registerDocumentSemanticTokensProvider(getAllCobolSelectors(), provider, VSSemanticProvider.getLegend());

    //no metadata, then seed it work basic implicit program-id symbols based on the files in workspace
    const ws = workspace.getWorkspaceFolder;

    if (ws !== undefined) {
        const pm = COBOLUtils.populateDefaultCallableSymbols();
        pm.then(() => {
            return;
        });
    }

    // display the message
    if (checkForExtensionConflictsMessage.length !== 0) {
        logMessage(checkForExtensionConflictsMessage);
    }

    const v = vscode.window.activeColorTheme;
    v.kind;
    if (settings.process_metadata_cache_on_start) {
        const depMode = settings.cache_metadata !== CacheDirectoryStrategy.Off;
        const pm = VSCobScanner.processAllFilesInWorkspaceOutOfProcess(false, depMode);
        pm.then(() => {
            return;
        });
    }

    openChangeLog();
}

export async function deactivateAsync(): Promise<void> {
    COBOLUtils.saveGlobalCacheToWorkspace(VSCOBOLConfiguration.get());
}

function openChangeLog(): void {
    const thisExtension = extensions.getExtension("bitlang.cobol");
    if (thisExtension !== undefined) {
        const extPath = `${thisExtension.extensionPath}`;
        const version = `${thisExtension.packageJSON.version}`;
        const lastVersion = currentContext.globalState.get("bitlang.cobol.version");
        if (lastVersion !== version) {
            const verFile = path.join(extPath, `CHANGELOG_${version}.md`);
            if (fs.existsSync(verFile)) {
                const readmeUri = vscode.Uri.file(verFile);
                commands.executeCommand("markdown.showPreview", readmeUri, ViewColumn.One, { locked: true });
                currentContext.globalState.update("bitlang.cobol.version", version);
            }
        }
    }
}
export async function deactivate(): Promise<void> {
    COBOLSourceScannerUtils.cleanup();  // drop the tmp file
    if (bldscriptTaskProvider) {
        bldscriptTaskProvider.dispose();
    }
    await deactivateAsync();
}

export function logException(message: string, ex: Error): void {
    logMessage(ex.name + ": " + message);
    if (ex !== undefined && ex.stack !== undefined) {
        logMessage(ex.stack);
    }
}

export const logTimeThreshold = 500;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
    const fixedTimeTaken = " (" + timeTaken.toFixed(2) + "ms)";

    if (timeTaken < logTimeThreshold) {
        return false;
    }

    if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
        const m: string = util.format(message, parameters);
        COBOLOutputChannel.appendLine(m.padEnd(60) + fixedTimeTaken);
    } else {
        COBOLOutputChannel.appendLine(message.padEnd(60) + fixedTimeTaken);
    }

    return true;
}


// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function logMessage(message: string, ...parameters: any[]): void {
    if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
        COBOLOutputChannel.appendLine(util.format(message, parameters));
    } else {
        COBOLOutputChannel.appendLine(message);
    }
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function logWarningMessage(message: string, ...parameters: any[]): void {
    const trimmedLeftCount = message.length - message.trimLeft().length;
    const spacesToLeft = " ".repeat(trimmedLeftCount);

    // TODO: Could this be colorized?
    if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
        COBOLOutputChannel.appendLine(`${spacesToLeft}WARNING: ${util.format(message, parameters)}`);
    } else {
        COBOLOutputChannel.appendLine(`${spacesToLeft}WARNING: ${message}`);
    }
}

export function logChannelSetPreserveFocus(preserveFocus: boolean): void {
    COBOLOutputChannel.show(preserveFocus);
}

export function logChannelHide(): void {
    COBOLOutputChannel.hide();
}

export function performance_now(): number {
    if (!process.env.BROWSER) {
        try {
            // eslint-disable-next-line @typescript-eslint/no-var-requires
            return require('performance-now').performance.now;
        }
        catch {
            return Date.now();
        }
    }

    return Date.now();
}
