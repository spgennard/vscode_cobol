import * as vscode from "vscode";
import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";
import { VSExtensionUtils } from "./vsextutis";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { COBOLProgramCommands } from "./cobolprogram";
import { TabUtils } from "./tabstopper";
import { VSLogger } from "./vslogger";
import { AlignStyle, COBOLUtils, FoldAction } from "./cobolutils";
import { commands } from "vscode";
import { VSPPCodeLens } from "./vsppcodelens";
import { ExtensionDefaults } from "./extensionDefaults";
import { COBOLSourceScanner } from "./cobolsourcescanner";
import path from "path";
import fs from "fs";
import { VSWorkspaceFolders } from "./cobolfolders";
import { VSDiagCommands } from "./vsdiagcommands";
import { CopyBookDragDropProvider } from "./vscopybookdragdroprovider";

function newFile(title: string, template: string, doclang: string) {
    let fpath = "";
    let fdir = "";
    const ws = VSWorkspaceFolders.get();
    if (ws) {
        fdir = ws[0].uri.fsPath;
    } else {
        fdir = process.cwd();
    }

    vscode.window.showInputBox({
        title: title,
        prompt: `In directory : ${fdir}`,
        value: "untitled",
        validateInput: (text: string): string | undefined => {
            if (!text || !COBOLSourceScanner.isValidLiteral(text)) {
                return "Invalid program name";
            }

            fpath = path.join(fdir, text + ".cbl");

            if (fs.existsSync(fpath)) {
                return `File already exists (${fpath})`;
            }

            return undefined;
        }
    }
    ).then(async function (data) {
        const ws = VSWorkspaceFolders.get();
        if (ws) {
            fpath = path.join(ws[0].uri.fsPath, data + ".cbl");
        } else {
            fpath = path.join(process.cwd(), data + ".cbl");
        }
        const furl = vscode.Uri.file(fpath).with({ scheme: "untitled" });
        await vscode.workspace.openTextDocument(furl).then(async document => {
            const el = template;
            const lines = vscode.workspace.getConfiguration().get<string[]>(el, []);
            const linesArray = [...lines];
            const editor = await vscode.window.showTextDocument(document);
            if (editor !== undefined) {
                const linesAsOne = linesArray.join("\n");
                await editor.insertSnippet(new vscode.SnippetString(linesAsOne), new vscode.Range(0, 0, 1 + linesArray.length, 0));
                await vscode.languages.setTextDocumentLanguage(document, doclang);
            }
        });
    });
}

function isMicroFocusLSPActive(): boolean {
    const mfeditorConfig = vscode.workspace.getConfiguration("microFocusCOBOL");
    return mfeditorConfig.get<boolean>("languageServerAutostart", true);
}

function isMicroFocusPLI_LSPActive(): boolean {
    const mfeditorConfig = vscode.workspace.getConfiguration("microFocusPLI");
    return mfeditorConfig.get<boolean>("languageServer.autostart", true);
}

export async function toggleMicroFocusLSP(settings: ICOBOLSettings, onOrOff: boolean): Promise<void> {
    // is it disabled?
    if (settings.enable_microfocus_lsp_when_active === false) {
        return;
    }

    // is it already set?
    if (isMicroFocusLSPActive() !== onOrOff) {
        const mfeditorConfig = vscode.workspace.getConfiguration("microFocusCOBOL");
        if (VSWorkspaceFolders.get() === undefined) {
            await mfeditorConfig.update("languageServerAutostart", onOrOff, vscode.ConfigurationTarget.Global);
        } else {
            await mfeditorConfig.update("languageServerAutostart", onOrOff, vscode.ConfigurationTarget.Workspace);
        }
    }

    if (isMicroFocusPLI_LSPActive() != onOrOff) {
        const microFocusPLIConfig = vscode.workspace.getConfiguration("microFocusPLI");
        if (VSWorkspaceFolders.get() === undefined) {
            microFocusPLIConfig.update("languageServer.autostart", onOrOff, vscode.ConfigurationTarget.Global);
        } else {
            microFocusPLIConfig.update("languageServer.autostart", onOrOff, vscode.ConfigurationTarget.Workspace);
        }
    }
}

export function activateCommonCommands(context: vscode.ExtensionContext, settings: ICOBOLSettings) {
    context.subscriptions.push(commands.registerCommand("cobolplugin.change_lang_to_acu", function () {
        const act = vscode.window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        vscode.languages.setTextDocumentLanguage(act.document, "ACUCOBOL");
        COBOLUtils.enforceFileExtensions(settings, act, VSExternalFeatures, true, "ACUCOBOL");
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.change_lang_to_cobol", async function () {
        const act = vscode.window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        vscode.languages.setTextDocumentLanguage(act.document, ExtensionDefaults.defaultCOBOLLanguage);

        const mfExt = vscode.extensions.getExtension(ExtensionDefaults.microFocusCOBOLExtension);
        if (mfExt) {
            await toggleMicroFocusLSP(settings, false);
        }

        COBOLUtils.enforceFileExtensions(settings, act, VSExternalFeatures, true, ExtensionDefaults.defaultCOBOLLanguage);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.change_lang_to_mfcobol", async function () {
        const act = vscode.window.activeTextEditor;
        if (act === null || act === undefined) {
            return;
        }

        vscode.languages.setTextDocumentLanguage(act.document, ExtensionDefaults.microFocusCOBOLLanguageId);
        COBOLUtils.enforceFileExtensions(settings, act, VSExternalFeatures, true, ExtensionDefaults.microFocusCOBOLLanguageId);

        await toggleMicroFocusLSP(settings, true);

        // invoke 'Micro Focus LSP Control'
        if (settings.enable_microfocus_lsp_lang_server_control) {
            vscode.commands.executeCommand("mfcobol.languageServer.controls");
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

    context.subscriptions.push(commands.registerCommand("cobolplugin.tab", async function () {
        await TabUtils.processTabKey(true);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.revtab", async function () {
        await TabUtils.processTabKey(false);
    }));

    // context.subscriptions.push(commands.registerCommand("web_cobolplugin.commentline", function () {
    //     if (vscode.window.activeTextEditor !== undefined) {
    //         commentUtils.processCommentLine();
    //     }
    // }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.removeAllComments", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.RemoveComments(vscode.window.activeTextEditor);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.removeIdentificationArea", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.RemoveIdentificationArea(vscode.window.activeTextEditor);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.removeColumnNumbers", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.removeColumnNumbers(vscode.window.activeTextEditor);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeKeywordsLowercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.Keywords, langid, intellisenseStyle.LowerCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeKeywordsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.Keywords, langid, intellisenseStyle.UpperCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeKeywordsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.Keywords, langid, intellisenseStyle.CamelCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeFieldsLowercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, langid, intellisenseStyle.LowerCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeFieldsUppercase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, langid, intellisenseStyle.UpperCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makeFieldsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.ConstantsOrVariables, langid, intellisenseStyle.CamelCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makePerformTargetsLowerCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.PerformTargets, langid, intellisenseStyle.LowerCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makePerformTargetsUpperCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.PerformTargets, langid, intellisenseStyle.UpperCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.makePerformTargetsCamelCase", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
                COBOLUtils.foldToken(VSExternalFeatures, settings, vscode.window.activeTextEditor, FoldAction.PerformTargets, langid, intellisenseStyle.CamelCase);
            }
        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.showCOBOLChannel", () => {
        VSLogger.logChannelSetPreserveFocus(true);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.resequenceColumnNumbers", () => {
        if (vscode.window.activeTextEditor) {
            const langid = vscode.window.activeTextEditor.document.languageId;

            if (VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {

                vscode.window.showInputBox({
                    prompt: "Enter start line number and increment",
                    validateInput: (text: string): string | undefined => {
                        if (!text || text.indexOf(" ") === -1) {
                            return "You must enter two spaced delimited numbers (start increment)";
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
    }));

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    context.subscriptions.push(commands.registerCommand("cobolplugin.ppcodelenaction", (args: string) => {
        VSPPCodeLens.actionCodeLens(args);
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.indentToCursor", () => {
        COBOLUtils.indentToCursor();
    }));

    context.subscriptions.push(commands.registerCommand("cobolplugin.leftAdjustLine", () => {
        COBOLUtils.leftAdjustLine();
    }));

    context.subscriptions.push(vscode.commands.registerTextEditorCommand("cobolplugin.transposeSelection", (textEditor, edit) => {
        COBOLUtils.transposeSelection(textEditor, edit);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageFirst", () => {
        COBOLUtils.alignStorage(AlignStyle.First);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageLeft", () => {
        COBOLUtils.alignStorage(AlignStyle.Left);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageCenter", () => {
        COBOLUtils.alignStorage(AlignStyle.Center);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.alignStorageRight", () => {
        COBOLUtils.alignStorage(AlignStyle.Right);
    }));
    vscode.commands.executeCommand("setContext", "cobolplugin.enableStorageAlign", true);

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.padTo72", () => {
        COBOLUtils.padTo72();
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.enforceFileExtensions", () => {
        if (vscode.window.activeTextEditor) {
            const dialects = ["COBOL", "ACUCOBOL", "RMCOBOL", "COBOLIT"];

            if (settings.enable_language_switcher) {
                const mfExt = vscode.extensions.getExtension(ExtensionDefaults.microFocusCOBOLExtension);
                if (mfExt !== undefined) {
                    dialects.push(ExtensionDefaults.microFocusCOBOLLanguageId);
                }
            }

            vscode.window.showQuickPick(dialects, { placeHolder: "Which Dialect do you prefer?" }).then(function (dialect) {
                if (vscode.window.activeTextEditor && dialect) {
                    COBOLUtils.enforceFileExtensions(settings, vscode.window.activeTextEditor, VSExternalFeatures, true, dialect);
                }
            });

        }
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToCOBOLHEX", () => {
        COBOLUtils.selectionToHEX(true);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToHEX", () => {
        COBOLUtils.selectionToHEX(false);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionHEXToASCII", () => {
        COBOLUtils.selectionHEXToASCII();
    }));


    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToCOBOLNXHEX", () => {
        COBOLUtils.selectionToNXHEX(true);
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.selectionToNXHEX", () => {
        COBOLUtils.selectionToNXHEX(false);
    }));


    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.newFile_MicroFocus", async function () {
        newFile("COBOL program name?", "coboleditor.template_microfocus", "COBOL");
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.newFile_MicroFocus_mfunit", async function () {
        newFile("COBOL Unit Test program name?", "coboleditor.template_microfocus_mfunit", "COBOL");
    }));

    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.newFile_ACUCOBOL", async function () {
        newFile("ACUCOBOL program name?", "coboleditor.template_acucobol", "ACUCOBOL");
    }));


    context.subscriptions.push(vscode.commands.registerCommand("cobolplugin.dumpAllSymbols", async function () {
        await VSDiagCommands.DumpAllSymbols();
    }));

    const langIds = settings.valid_cobol_language_ids;
    const mfExt = vscode.extensions.getExtension(ExtensionDefaults.microFocusCOBOLExtension);
    if (mfExt) {
        langIds.push(ExtensionDefaults.microFocusCOBOLLanguageId);
    }

    for (const langid of langIds) {
        if (langid !== ExtensionDefaults.microFocusCOBOLLanguageId) {
            context.subscriptions.push(getLangStatusItem("Output Window", "cobolplugin.showCOBOLChannel", "Show", settings, langid + "_1", langid));
        }

        switch (langid) {
            case "ACUCOBOL":
                context.subscriptions.push(getLangStatusItem("Switch to COBOL", "cobolplugin.change_lang_to_cobol", "Change", settings, langid + "_2", langid));
                break;
            case "COBOL":
                {
                    context.subscriptions.push(getLangStatusItem("Switch to ACUCOBOL", "cobolplugin.change_lang_to_acu", "Change", settings, langid + "_3", langid));

                    if (mfExt !== undefined) {
                        context.subscriptions.push(getLangStatusItem("Switch to 'Micro Focus COBOL'", "cobolplugin.change_lang_to_mfcobol", "Change", settings, langid + "_5", langid));
                    }
                }
                break;
            case ExtensionDefaults.microFocusCOBOLLanguageId:
                context.subscriptions.push(getLangStatusItem("Switch to 'BitLang COBOL'", "cobolplugin.change_lang_to_cobol", "Change", settings, langid + "_4", langid));
                break;
        }

        context.subscriptions.push(vscode.languages.registerDocumentDropEditProvider(VSExtensionUtils.getAllCobolSelector(langid), new CopyBookDragDropProvider(settings)));
    }
}

function getLangStatusItem(text: string, command: string, title: string, settings: ICOBOLSettings, id: string, langid: string): vscode.LanguageStatusItem {
    const langStatusItem = vscode.languages.createLanguageStatusItem(id, VSExtensionUtils.getAllCobolSelector(langid));
    langStatusItem.text = text;
    langStatusItem.command = {
        command: command,
        title: title
    };
    return langStatusItem;
}
