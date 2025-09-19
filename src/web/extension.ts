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
import { VSLogger } from "../vslogger";
import { COBOLSourceDefinition } from "../vssourcedefinitionprovider";
import { KeywordAutocompleteCompletionItemProvider } from "../vskeywordprovider";
import { CobolSourceCompletionItemProvider } from "../vscobolprovider";
import { VSCOBOLUtils } from "../vscobolutils";
import { VSSemanticProvider } from "../vssemanticprovider";
import { ExtensionDefaults } from "../extensionDefaults";
import { commands, languages, ProviderResult } from "vscode";
import { VSCobolRenameProvider } from "../vsrenameprovider";
import { VSPPCodeLens } from "../vsppcodelens";
import { activateCommonCommands, checkForExtensionConflicts } from "../vscommon_commands";
import { VSWorkspaceFolders } from "../vscobolfolders";
import { VSSourceTreeViewHandler } from "../vssourceviewtree";
import { VSHelpAndFeedViewHandler } from "../feedbacktree";
import { VSHoverProvider } from "../vshoverprovider";
import { CobolReferenceProvider } from "../vsreferenceprovider";

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

async function setupLogChannelAndPaths(hide: boolean, settings: ICOBOLSettings) {

    URLSearchDirectory.length = 0;

    const extsdir = settings.copybookdirs;
    invalidSearchDirectory = settings.invalid_copybookdirs;
    invalidSearchDirectory.length = 0;

    const perfile_copybookdirs = settings.perfile_copybookdirs;
    const ws = VSWorkspaceFolders.get(settings);
    const wsURLs = VSWorkspaceFolders.getFiltered("", settings);

    const fileSearchDirectory = settings.file_search_directory;
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

        if (fileSearchDirectory.length !== 0) {
            VSLogger.logMessage("  Combined Workspace and CopyBook Folders to search:");
            for (const sdir of fileSearchDirectory) {
                VSLogger.logMessage("   => " + sdir);
            }
        }

        if (URLSearchDirectory.length !== 0) {
            VSLogger.logMessage("  Combined Workspace and CopyBook Folders to search (URL):");
            for (const sdir of URLSearchDirectory) {
                VSLogger.logMessage("   => " + sdir);
            }
        }

        if (perfile_copybookdirs.length !== 0) {
            VSLogger.logMessage("  Per File CopyBook directories (" + perfile_copybookdirs.length + ")");
            for (const sdir of perfile_copybookdirs) {
                VSLogger.logMessage("   => " + sdir);
            }
        }

        if (invalidSearchDirectory.length !== 0) {
            VSLogger.logMessage("  Invalid CopyBook directories (" + invalidSearchDirectory.length + ")");
            for (const sdir of invalidSearchDirectory) {
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
    const _settings: IVSCOBOLSettings = VSCOBOLConfiguration.reinitWorkspaceSettings(VSExternalFeatures);
    VSExternalFeatures.setURLCopyBookSearchPath(URLSearchDirectory);

    await setupLogChannelAndPaths(true, _settings);

    showExtensionInformation();

    if (checkForExtensionConflicts(_settings, context)) {
        throw new Error("Unable to activate extension due to conflicts");
    }

    activateCommonCommands(context);

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
        if (vscode.window.activeTextEditor === undefined) {
            return;
        }
        const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

        if (vscode.window.activeTextEditor !== undefined) {
            commentUtils.processCommentLine(settings);
        }
    }));

    const cobolSelectors = VSExtensionUtils.getAllCobolSelectors(_settings, true);

    try {
        if (_settings.outline) {
            context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(cobolSelectors, new CobolSymbolInformationProvider()));
        }
    } catch (e) {
        VSExternalFeatures.logException("during registerDocumentSymbolProvider", e as Error);
    }

    const onDidCloseTextDocumentHandler = vscode.workspace.onDidCloseTextDocument(async (doc: vscode.TextDocument) => {
        if (vscode.window.activeTextEditor === undefined) {
            return;
        }
        const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);

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

    const sourcedefProvider = vscode.languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(_settings, true), {
        provideDefinition(doc: vscode.TextDocument, pos: vscode.Position, ct: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(sourcedefProvider);

    context.subscriptions.push(languages.registerReferenceProvider(VSExtensionUtils.getAllCobolSelectors(_settings, true), new CobolReferenceProvider()));

    const keywordProvider = new KeywordAutocompleteCompletionItemProvider(true, _settings);
    const keywordProviderDisposible = vscode.languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(_settings, true), keywordProvider);
    context.subscriptions.push(keywordProviderDisposible);

    const cobolProvider = new CobolSourceCompletionItemProvider(VSExternalFeatures);
    const cobolProviderDisposible = vscode.languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(_settings, true), cobolProvider);
    context.subscriptions.push(cobolProviderDisposible);

    const provider = VSSemanticProvider.provider();
    vscode.languages.registerDocumentSemanticTokensProvider(VSExtensionUtils.getAllCobolSelectors(_settings, true), provider, VSSemanticProvider.getLegend());

    const codelensProvider = new VSPPCodeLens();
    languages.registerCodeLensProvider(VSExtensionUtils.getAllCobolSelectors(_settings, true), codelensProvider);

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

    await VSSourceTreeViewHandler.setupSourceViewTree(_settings, false);
    VSHelpAndFeedViewHandler.setupSourceViewTree(_settings, false);
    
    /* hover provider */
    context.subscriptions.push(languages.registerHoverProvider(VSExtensionUtils.getAllCobolSelectors(_settings, false), {
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): ProviderResult<vscode.Hover> {
            if (vscode.window.activeTextEditor === undefined) {
                return;
            }
            const settings = VSCOBOLConfiguration.get_resource_settings(vscode.window.activeTextEditor.document, VSExternalFeatures);
            
            return VSHoverProvider.provideHover(settings, document, position);
        }
    }));

    const renameProvider = new VSCobolRenameProvider();
    const renameProviderDisposable = languages.registerRenameProvider(VSExtensionUtils.getAllCobolSelectors(_settings, true), renameProvider);
    context.subscriptions.push(renameProviderDisposable);
}

// this method is called when your extension is deactivated
export function deactivate() {
    //
}
