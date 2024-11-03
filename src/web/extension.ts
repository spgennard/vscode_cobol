// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import { IVSCOBOLSettings, VSCOBOLConfiguration } from "../vsconfiguration";
import { ICOBOLSettings } from "../iconfiguration";
import { VSExtensionUtils } from "../vsextutis";
import { CobolSymbolInformationProvider } from "../vssymbolprovider";
import { VSExternalFeatures } from "../vsexternalfeatures";
import { VSCOBOLSourceScanner } from "../vscobolscanner";
import { vsMarginHandler } from "../vsmargindecorations";
import { commentUtils } from "../commenter";
import { COBOLOutputChannel, VSLogger } from "../vslogger";
import { COBOLSourceDefinition } from "../vssourcedefinitionprovider";
import { KeywordAutocompleteCompletionItemProvider } from "../vskeywordprovider";
import { CobolSourceCompletionItemProvider } from "../vscobolprovider";
import { VSCOBOLUtils } from "../vscobolutils";
import { VSSemanticProvider } from "../vssemanticprovider";
import { ExtensionDefaults } from "../extensionDefaults";
import { commands, extensions, languages, ProviderResult, workspace } from "vscode";
import { VSCobolRenameProvider } from "../vsrenameprovider";
import { VSPPCodeLens } from "../vsppcodelens";
import { activateCommonCommands } from "../vscommon_commands";
import { VSWorkspaceFolders } from "../vscobolfolders";
import { VSSourceTreeViewHandler } from "../vssourceviewtree";
import { VSHelpAndFeedViewHandler } from "../feedbacktree";
import { VSHoverProvider } from "../vshoverprovider";
import { CobolReferenceProvider } from "../vsreferenceprovider";

const fileSearchDirectory: string[] = [];
const URLSearchDirectory: string[] = [];
let invalidSearchDirectory: string[] = [];

function showExtensionInformation(): void {
    const thisExtension = vscode.extensions.getExtension(ExtensionDefaults.thisExtensionName);

    if (thisExtension !== undefined) {
        if (vscode.env.uriScheme !== "vscode") {
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Warning: you are using a untested environment : ${vscode.env.uriScheme}`);
            VSLogger.logMessage("----------------------------------------------------------------------");
        }
        VSLogger.logMessage(`Version                                     : ${vscode.version}`);
        VSLogger.logMessage("Extension Information:");
        VSLogger.logMessage(` Extension path                             : ${thisExtension.extensionPath}`);
        VSLogger.logMessage(` Version                                    : ${thisExtension.packageJSON.version}`);
    }
}

const blessed_extensions: string[] = [
    "HCLTechnologies.hclappscancodesweep",      // code scanner
    ExtensionDefaults.rocketCOBOLExtension,  // Rocket COBOL extension
    "bitlang.cobol"
];

const known_problem_extensions: string[][] = [
    ["A control flow extension that is not compatible with this dialect of COBOL", "BroadcomMFD.ccf"],             // control flow extension
    ["COBOL debugger for different dialect of COBOL", "COBOLworx.cbl-gdb"]
];

let conflictsFound = false;
let conflictingDebuggerFound = false;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function getExtensionInformation(grab_info_for_ext: vscode.Extension<any>, reasons: string[]): string {
    let dupExtensionMessage = "";

    if (grab_info_for_ext.packageJSON === undefined) {
        return dupExtensionMessage
    }
    
    if (grab_info_for_ext.packageJSON !== undefined && grab_info_for_ext.packageJSON.publisher === "bitlang") {
        return dupExtensionMessage;
    }

    if (grab_info_for_ext.packageJSON.id !== undefined) {
        dupExtensionMessage += `\nThe extension ${grab_info_for_ext.packageJSON.name} from ${grab_info_for_ext.packageJSON.publisher} has conflicting functionality\n`;
        dupExtensionMessage += " Solution      : Disable or uninstall this extension, eg: use command:\n";
        dupExtensionMessage += `                 code --uninstall-extension ${grab_info_for_ext.packageJSON.id}\n`;
    }

    if (reasons.length !== 0) {
        let rcount = 1;
        const reasonMessage = reasons.length === 1 ? "Reason " : "Reasons";
        for (const reason of reasons) {
            if (rcount === 1) {
                dupExtensionMessage += ` ${reasonMessage}       : ${reason}\n`;
            } else {
                dupExtensionMessage += `               : ${reason}\n`;
            }
            rcount++;
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
                if (ext.packageJSON.id === ExtensionDefaults.thisExtensionName) {
                    continue;
                }

                for (const blessed_extension of blessed_extensions) {
                    if (blessed_extension === ext.packageJSON.id) {
                        ignore_blessed = true;
                    }
                }

                if (!ignore_blessed) {
                    for (const [type_of_extension, known_problem_extension] of known_problem_extensions) {
                        if (known_problem_extension.toLowerCase() === ext.packageJSON.id.toLowerCase()) {
                            reason.push(`contributes '${type_of_extension}'`);
                            if (type_of_extension.includes("debugger")) {
                                conflictingDebuggerFound = true;
                            }
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
                                    if (l === ExtensionDefaults.defaultCOBOLLanguage) {
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
                                    if (l === ExtensionDefaults.defaultCOBOLLanguage) {
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
                                    if (debuggerElement !== undefined) {
                                        // if (debuggerElement.enableBreakpointsFor !== undefined) {
                                        //     if (debuggerElement.enableBreakpointsFor.languageIds !== undefined) {
                                        //         for (const bpLangidKey in debuggerElement.enableBreakpointsFor.languageIds) {
                                        //             const languageElement = debuggerElement.enableBreakpointsFor.languageIds[bpLangidKey];
                                        //             const l = `${languageElement}`;
                                        //             if (l === ExtensionDefaults.defaultCOBOLLanguage) {
                                        //                 reason.push("extension includes a debug breakpoint support for a different COBOL vendor");
                                        //                 conflictingDebuggerFound = true;
                                        //             }
                                        //         }
                                        //     }
                                        // }
                                        const debuggerLanguages = debuggerElement.languages;
                                        if (debuggerLanguages !== undefined && debuggerLanguages instanceof Object) {
                                            for (const keyLanguage of debuggerLanguages) {
                                                if (keyLanguage === ExtensionDefaults.defaultCOBOLLanguage) {
                                                    reason.push(`extension includes a debugger for a different COBOL vendor -> ${debuggerElement.label} of debugger type ${debuggerElement.type}`);
                                                    conflictingDebuggerFound = true;
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
                            try {
                                for (const bpLangKey of breakpointsBody) {
                                    if (bpLangKey !== undefined && bpLangKey.language !== undefined) {
                                        const bpLang = `${bpLangKey.language}`;
                                        if (bpLang === ExtensionDefaults.defaultCOBOLLanguage) {
                                            reason.push("extension includes debug breakpoint support for a different COBOL vendor");
                                            conflictingDebuggerFound = true;
                                        }
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

            if (reason.length !== 0) {
                dupExtensionMessage += getExtensionInformation(ext, reason);
            }
        }
    }

    return dupExtensionMessage;
}

async function setupLogChannelAndPaths(hide: boolean, settings: ICOBOLSettings) {

    fileSearchDirectory.length = 0;
    URLSearchDirectory.length = 0;

    const extsdir = settings.copybookdirs;
    invalidSearchDirectory = settings.invalid_copybookdirs;
    invalidSearchDirectory.length = 0;

    const ws = VSWorkspaceFolders.get(settings);
    const wsURLs = VSWorkspaceFolders.get_filtered("", settings);

    if (wsURLs !== undefined) {
        for (const folder of wsURLs) {
            // place the workspace folder in the copybook path
            URLSearchDirectory.push(folder.uri.toString());

            /* now add any extra directories that are below this workspace folder */
            for (const extdir of extsdir) {
                try {
                    const sdir = `${folder.uri.toString()}/${extdir}`;

                    const sdirStat = await vscode.workspace.fs.stat(vscode.Uri.parse(sdir));
                    if (sdirStat.type & vscode.FileType.Directory) {
                        URLSearchDirectory.push(sdir);
                    } else {
                        invalidSearchDirectory.push("URL as " + sdir);
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
    for (const fsd of filterfileSearchDirectory) {
        fileSearchDirectory.push(fsd);
    }

    invalidSearchDirectory = invalidSearchDirectory.filter((elem, pos) => invalidSearchDirectory.indexOf(elem) === pos);

    if (ws !== undefined && ws.length !== 0) {
        VSLogger.logMessage("  Workspace Folders:");
        for (const folder of ws) {
            VSLogger.logMessage("   => " + folder.name + " @ " + folder.uri.fsPath);
        }
    } else {
        if (wsURLs !== undefined && wsURLs.length !== 0) {
            VSLogger.logMessage("  Workspace Folders (URLs):");
            for (const folder of wsURLs) {
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

        const extdirURL = URLSearchDirectory;
        if (extdirURL.length !== 0) {
            VSLogger.logMessage("  Combined Workspace and CopyBook Folders to search (URL):");
            for (const sdir of extdirURL) {
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

    if (settings.maintain_metadata_recursive_search) {
        VSCOBOLUtils.populateDefaultCallableSymbolsSync(settings, true);
        VSCOBOLUtils.populateDefaultCopyBooksSync(settings, true);
    }
}

export async function activate(context: vscode.ExtensionContext) {
    const settings: IVSCOBOLSettings = VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);
    settings.file_search_directory = fileSearchDirectory;
    VSExternalFeatures.setURLCopyBookSearchPath(URLSearchDirectory);

    await setupLogChannelAndPaths(true, settings);

    showExtensionInformation();

    const checkForExtensionConflictsMessage = checkForExtensionConflicts();

    // display the message
    if (checkForExtensionConflictsMessage.length !== 0) {
        conflictsFound = true;
        VSLogger.logMessage(checkForExtensionConflictsMessage);

        if (conflictingDebuggerFound) {
            COBOLOutputChannel.show();
            for (const veditor of vscode.window.visibleTextEditors) {
                const doc = veditor.document;
                if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, doc.languageId)) {
                    VSLogger.logMessage(`Document ${doc.fileName} changed to plaintext to avoid errors, as the COBOL extension is inactive`);
                    languages.setTextDocumentLanguage(doc, "plaintext");
                }
            }

            const onDidOpenTextDocumentHandler = workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {
                if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, doc.languageId)) {
                    VSLogger.logMessage(`Document ${doc.fileName} changed to plaintext to avoid errors, as the COBOL extension is inactive`);
                    languages.setTextDocumentLanguage(doc, "plaintext");
                }
            });
            context.subscriptions.push(onDidOpenTextDocumentHandler);
        }

        VSLogger.logChannelSetPreserveFocus(false);

        if (conflictingDebuggerFound) {
            const msg = "This Extension is now inactive until conflict is resolved";
            VSLogger.logMessage(`\n${msg}\nRestart 'vscode' once the conflict is resolved or you can disabled the ${ExtensionDefaults.thisExtensionName} extension`);

            const mfExt = extensions.getExtension(ExtensionDefaults.rocketCOBOLExtension);
            if (mfExt !== undefined) {
                VSLogger.logMessage("\nYou already have a 'Rocket COBOL' compatible debugger installed, so may not need the above extension(s)");
            } else {
                VSLogger.logMessage(`\nIf you want a 'Rocket COBOL' compatible debugger install the extension using the following command\ncode --install-extension ${ExtensionDefaults.rocketCOBOLExtension}`);
            }
            throw new Error(msg);
        }
    }

    if (conflictsFound && conflictingDebuggerFound) {
        throw new Error("Unable to activate extension due to conflicts");
    }

    activateCommonCommands(context, settings);

    // re-init if something gets installed or removed
    const onExtChange = vscode.extensions.onDidChange(() => {
        // activateLogChannelAndPaths(true, settings, false);
        VSLogger.logMessage("extensions changed");
    });
    context.subscriptions.push(onExtChange);

    context.subscriptions.push(vscode.workspace.onDidChangeConfiguration((event: vscode.ConfigurationChangeEvent) => {
        const updated = event.affectsConfiguration(ExtensionDefaults.defaultEditorConfig);

        if (updated) {
            VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);
        }
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.commentline", function () {
        if (vscode.window.activeTextEditor !== undefined) {
            commentUtils.processCommentLine(settings);
        }
    }));

    const cobolSelectors = VSExtensionUtils.getAllCobolSelectors(settings, true);

    try {
        if (settings.outline) {
            context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(cobolSelectors, new CobolSymbolInformationProvider()));
        }
    } catch (e) {
        VSExternalFeatures.logException("during registerDocumentSymbolProvider", e as Error);
    }

    const onDidCloseTextDocumentHandler = vscode.workspace.onDidCloseTextDocument(async (doc: vscode.TextDocument) => {
        if (VSExtensionUtils.isSupportedLanguage(doc)) {
            VSCOBOLSourceScanner.removeCachedObject(doc, settings);
        }
    });
    context.subscriptions.push(onDidCloseTextDocumentHandler);

    vscode.window.onDidChangeActiveTextEditor(editor => {
        if (!editor) {
            return;
        }
        vsMarginHandler.updateDecorations(editor);
        // linter.updateLinter(editor.document);

    }, null, context.subscriptions);

    vscode.window.onDidChangeTextEditorSelection(event => {
        if (!event.textEditor) {
            return;
        }
        vsMarginHandler.updateDecorations(event.textEditor);
        //cobolusage.updateDiagnostics(event.textEditor.document);
    }, null, context.subscriptions);

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    vscode.workspace.onDidChangeTextDocument(event => {
        if (!vscode.window.activeTextEditor) {
            return;
        }
        vsMarginHandler.updateDecorations(vscode.window.activeTextEditor);
        // linter.updateLinter(window.activeTextEditor.document);
    }, null, context.subscriptions);

    if (vscode.window.activeTextEditor !== undefined) {
        vsMarginHandler.updateDecorations(vscode.window.activeTextEditor);
        // linter.updateLinter(window.activeTextEditor.document);
    }
    // const onDidOpenTextDocumentHandler = vscode.workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {
    //     VSLogger.logMessage(`Open document ${doc.fileName} / ${doc.uri.scheme}`);
    // });
    // context.subscriptions.push(onDidOpenTextDocumentHandler);

    const sourcedefProvider = vscode.languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), {
        provideDefinition(doc: vscode.TextDocument, pos: vscode.Position, ct: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(sourcedefProvider);

    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), new CobolReferenceProvider()));

    const keywordProvider = new KeywordAutocompleteCompletionItemProvider(true, settings);
    const keywordProviderDisposible = vscode.languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), keywordProvider);
    context.subscriptions.push(keywordProviderDisposible);

    const cobolProvider = new CobolSourceCompletionItemProvider(settings, VSExternalFeatures);
    const cobolProviderDisposible = vscode.languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), cobolProvider);
    context.subscriptions.push(cobolProviderDisposible);

    const provider = VSSemanticProvider.provider();
    vscode.languages.registerDocumentSemanticTokensProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), provider, VSSemanticProvider.getLegend());

    const codelensProvider = new VSPPCodeLens();
    languages.registerCodeLensProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), codelensProvider);

    vscode.window.onDidChangeTextEditorSelection((e: vscode.TextEditorSelectionChangeEvent) => {
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
    });

    await VSSourceTreeViewHandler.setupSourceViewTree(settings, false);
    VSHelpAndFeedViewHandler.setupSourceViewTree(settings, false);
    
    /* hover provider */
    context.subscriptions.push(languages.registerHoverProvider(VSExtensionUtils.getAllCobolSelectors(settings, false), {
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): ProviderResult<vscode.Hover> {
            return VSHoverProvider.provideHover(settings, document, position);
        }
    }));

    const renameProvider = new VSCobolRenameProvider();
    const renameProviderDisposable = languages.registerRenameProvider(VSExtensionUtils.getAllCobolSelectors(settings, true), renameProvider);
    context.subscriptions.push(renameProviderDisposable);
}

// this method is called when your extension is deactivated
export function deactivate() {
    //
}
