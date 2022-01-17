// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import { VSCOBOLConfiguration } from "../vsconfiguration";
import { ICOBOLSettings } from "../iconfiguration";
import { VSExtensionUtils } from "../vsextutis";
import { CobolSymbolInformationProvider } from "../symbolprovider";
import { VSExternalFeatures } from "../vsexternalfeatures";
import { COBOLProgramCommands } from "../cobolprogram";
import { TabUtils } from "../tabstopper";
import { VSCOBOLSourceScanner } from "../vscobolscanner";
import { VSmargindecorations } from "../margindecorations";
import { commentUtils } from "../commenter";
import { VSLogger } from "../vslogger";
import { COBOLSourceDefinition } from "../sourcedefinitionprovider";
import { KeywordAutocompleteCompletionItemProvider } from "../keywordprovider";
import { CobolSourceCompletionItemProvider } from "../cobolprovider";
import { AlignStyle, COBOLUtils, FoldAction, FoldStyle } from "../cobolutils";
import { VSSemanticProvider } from "../vssemanticprovider";

function showExtensionInformation():void {
    const thisExtension = vscode.extensions.getExtension("bitlang.cobol");

    if (thisExtension !== undefined) {
        if (vscode.env.uriScheme !== "vscode") {
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Warning: you are using a untested environment : ${vscode.env.uriScheme}`);
            VSLogger.logMessage("----------------------------------------------------------------------");
            VSLogger.logMessage(`Version                                     : ${vscode.version}`);
        } else {
            VSLogger.logMessage(`VSCode version                              : ${vscode.version}`);
        }
        VSLogger.logMessage("Extension Information:");
        VSLogger.logMessage(` Extension path                             : ${thisExtension.extensionPath}`);
        VSLogger.logMessage(` Version                                    : ${thisExtension.packageJSON.version}`);
    }
}
export function activate(context: vscode.ExtensionContext) {
    VSCOBOLConfiguration.externalFeatures = VSExternalFeatures;

    const commands = vscode.commands;
    const settings: ICOBOLSettings = VSCOBOLConfiguration.reinit();
    
    context.subscriptions.push(vscode.workspace.onDidChangeConfiguration((event: vscode.ConfigurationChangeEvent) => {
        const updated = event.affectsConfiguration("coboleditor");

        if (updated) {
            VSCOBOLConfiguration.reinit();
        }
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2pd", function () {
        COBOLProgramCommands.move2pd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2dd", function () {
        COBOLProgramCommands.move2dd();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2ws", function () {
        COBOLProgramCommands.move2ws();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anyforward", function () {
        COBOLProgramCommands.move2anyforward();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.move2anybackwards", function () {
        COBOLProgramCommands.move2anybackwards();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.tab", function () {
        TabUtils.processTabKey(true);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.revtab", function () {
        TabUtils.processTabKey(false);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.commentline", function () {
        if (vscode.window.activeTextEditor !== undefined) {
            commentUtils.processCommentLine();
        }
    }));

    const cobolSelectors = VSExtensionUtils.getAllCobolSelectors(settings);

    try {
        context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(cobolSelectors, new CobolSymbolInformationProvider()));
    } catch(e) {
        VSExternalFeatures.logException("during registerDocumentSymbolProvider",e as Error);
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
        VSmargindecorations.updateDecorations(editor);
        // linter.updateLinter(editor.document);

    }, null, context.subscriptions);

    vscode.window.onDidChangeTextEditorSelection(event => {
        if (!event.textEditor) {
            return;
        }
        VSmargindecorations.updateDecorations(event.textEditor);
        //cobolusage.updateDiagnostics(event.textEditor.document);
    }, null, context.subscriptions);

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    vscode.workspace.onDidChangeTextDocument(event => {
        if (!vscode.window.activeTextEditor) {
            return;
        }
        VSmargindecorations.updateDecorations(vscode.window.activeTextEditor);
        // linter.updateLinter(window.activeTextEditor.document);
    }, null, context.subscriptions);

    if (vscode.window.activeTextEditor !== undefined) {
        VSmargindecorations.updateDecorations(vscode.window.activeTextEditor);
        // linter.updateLinter(window.activeTextEditor.document);
    }
    // const onDidOpenTextDocumentHandler = vscode.workspace.onDidOpenTextDocument(async (doc: vscode.TextDocument) => {
    //     VSLogger.logMessage(`Open document ${doc.fileName} / ${doc.uri.scheme}`);
    // });
    // context.subscriptions.push(onDidOpenTextDocumentHandler);

    const sourcedefProvider = vscode.languages.registerDefinitionProvider(VSExtensionUtils.getAllCobolSelectors(settings), {
        provideDefinition(doc: vscode.TextDocument, pos: vscode.Position, ct: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
            const csd = new COBOLSourceDefinition();
            return csd.provideDefinition(doc, pos, ct);
        }
    });
    context.subscriptions.push(sourcedefProvider);
    
    const keywordProvider = new KeywordAutocompleteCompletionItemProvider(true);
    const keywordProviderDisposible = vscode.languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings), keywordProvider);
    context.subscriptions.push(keywordProviderDisposible);

    const cobolProvider = new CobolSourceCompletionItemProvider(settings, VSExternalFeatures);
    const cobolProviderDisposible = vscode.languages.registerCompletionItemProvider(VSExtensionUtils.getAllCobolSelectors(settings), cobolProvider);
    context.subscriptions.push(cobolProviderDisposible);


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
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.LowerCase, langid);
            }
        }
    });
    context.subscriptions.push(makeKeywordsLowercaseCommands);

    const makeKeywordsUppercaseCommands = vscode.commands.registerCommand("cobolplugin.makeKeywordsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makeKeywordsUppercaseCommands);

    const makeKeywordsCamelCaseCommands = vscode.commands.registerCommand("cobolplugin.makeKeywordsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.Keywords, FoldStyle.CamelCase, langid);
            }
        }
    });
    context.subscriptions.push(makeKeywordsCamelCaseCommands);

    const makeFieldsLowercaseCommand = vscode.commands.registerCommand("cobolplugin.makeFieldsLowercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makeFieldsLowercaseCommand);

    const makeFieldsUppercaseCommand = vscode.commands.registerCommand("cobolplugin.makeFieldsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makeFieldsUppercaseCommand);

    const makeFieldsCamelCaseCommand = vscode.commands.registerCommand("cobolplugin.makeFieldsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, FoldStyle.CamelCase, langid);
            }
        }
    });
    context.subscriptions.push(makeFieldsCamelCaseCommand);

    const makePerformTargetsLowerCaseCommand = vscode.commands.registerCommand("cobolplugin.makePerformTargetsLowerCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.LowerCase, langid);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsLowerCaseCommand);

    const makePerformTargetsUpperCaseCommand = vscode.commands.registerCommand("cobolplugin.makePerformTargetsUpperCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.UpperCase, langid);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsUpperCaseCommand);

    const makePerformTargetsCamelCaseCommand = vscode.commands.registerCommand("cobolplugin.makePerformTargetsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, vscode.window.activeTextEditor, FoldAction.PerformTargets, FoldStyle.CamelCase, langid);
            }
        }
    });
    context.subscriptions.push(makePerformTargetsCamelCaseCommand);

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

    const provider = VSSemanticProvider.provider();
    vscode.languages.registerDocumentSemanticTokensProvider(VSExtensionUtils.getAllCobolSelectors(settings), provider, VSSemanticProvider.getLegend());

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

    const alignStorageFirst = vscode.commands.registerCommand("cobolplugin.alignStorageFirst", () => {
        COBOLUtils.alignStorage(AlignStyle.First);
    });
    context.subscriptions.push(alignStorageFirst);

    const alignStorageWide= vscode.commands.registerCommand("cobolplugin.alignStorageWide", () => {
        COBOLUtils.alignStorage(AlignStyle.Wide);
    });
    context.subscriptions.push(alignStorageWide);
    vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);

    showExtensionInformation();
}

// this method is called when your extension is deactivated
export function deactivate() {
    //
}
