import * as vscode from "vscode";
import path from "path";
import { COBOLSourceScanner, SourceScannerUtils, COBOLTokenStyle } from "./cobolsourcescanner";
import { cobolRegistersDictionary, cobolStorageKeywordDictionary, getCOBOLKeywordDictionary } from "./keywords/cobolKeywords";
import { VSLogger } from "./vslogger";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { InMemoryGlobalCacheHelper, InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { COBOLFileUtils } from "./fileutils";
import { VSWorkspaceFolders } from "./cobolfolders";
import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";
import { COBOLFileSymbol } from "./cobolglobalcache";
import { VSCOBOLFileUtils } from "./vsfileutils";
import { IExternalFeatures } from "./externalfeatures";
import { ExtensionDefaults } from "./extensionDefaults";
import { VSCustomIntelliseRules } from "./vscustomrules";
import { SplitTokenizer } from "./splittoken";

let commandTerminal: vscode.Terminal | undefined = undefined;
const commandTerminalName = "COBOL Application";

export enum FoldAction {
    PerformTargets = 1,
    ConstantsOrVariables = 2,
    Keywords = 3
}

export enum AlignStyle {
    First = 1,
    Left = 2,
    Center = 3,
    Right = 4
}

export class COBOLUtils {
    static mfExtension = vscode.extensions.getExtension("micro-focus-amc.mfcobol");

    private static getProgramGlobPattern(config: ICOBOLSettings): string {
        let globString = config.maintain_metadata_recursive_search ? "**/*.{" : "*.{";

        for (const ext of config.program_extensions) {
            if (ext.length !== 0) {
                if (globString.endsWith("{")) {
                    globString += ext;
                } else {
                    globString += "," + ext;
                }
            }
        }

        globString += "}";

        return globString;
    }

    private static getCopyBookGlobPattern(config: ICOBOLSettings): string {
        let globString = config.maintain_metadata_recursive_search ? "**/*.{" : "*.{";

        for (const ext of config.copybookexts) {
            if (ext.length !== 0) {
                if (globString.endsWith("{")) {
                    globString += ext;
                } else {
                    globString += "," + ext;
                }
            }
        }

        globString += "}";

        return globString;
    }
    static prevWorkSpaceUri: vscode.Uri | undefined = undefined;

    static populateDefaultCallableSymbolsSync(settings: ICOBOLSettings, reset: boolean): void {
        (async () => COBOLUtils.populateDefaultCallableSymbols(settings, reset))();
    }

    static async populateDefaultCallableSymbols(settings: ICOBOLSettings, reset: boolean): Promise<void> {
        const ws = VSWorkspaceFolders.get();
        if (ws === undefined) {
            return;
        }

        const wsf = vscode.workspace.workspaceFile;
        if (wsf === undefined) {
            InMemoryGlobalSymbolCache.defaultCallableSymbols.clear();
            return;
        }

        // reset 
        if (reset) {
            COBOLUtils.prevWorkSpaceUri = undefined;
        } else {
            // already cached?
            if (wsf.fsPath === COBOLUtils.prevWorkSpaceUri?.fsPath) {
                return;
            }
        }

        // stash away current ws
        COBOLUtils.prevWorkSpaceUri = wsf;
        const globPattern = COBOLUtils.getProgramGlobPattern(settings);

        await vscode.workspace.findFiles(globPattern).then((uris: vscode.Uri[]) => {
            uris.forEach((uri: vscode.Uri) => {
                const fullPath = uri.fsPath;
                const fileName = InMemoryGlobalCacheHelper.getFilenameWithoutPath(fullPath);
                const fileNameNoExt = path.basename(fileName, path.extname(fileName));
                const callableSymbolFromFilenameLower = fileNameNoExt.toLowerCase();
                const c = InMemoryGlobalSymbolCache.defaultCallableSymbols.get(callableSymbolFromFilenameLower);
                if (c === undefined) {
                    InMemoryGlobalSymbolCache.defaultCallableSymbols.set(callableSymbolFromFilenameLower, fullPath);
                }
            });
        });
    }

    static populateDefaultCopyBooksSync(settings: ICOBOLSettings, reset: boolean): void {
        (async () => COBOLUtils.populateDefaultCopyBooks(settings, reset))();
    }

    static async populateDefaultCopyBooks(settings: ICOBOLSettings, reset: boolean): Promise<void> {
        const ws = VSWorkspaceFolders.get();
        if (ws === undefined) {
            return;
        }

        const wsf = vscode.workspace.workspaceFile;
        if (wsf === undefined) {
            InMemoryGlobalSymbolCache.defaultCopybooks.clear();
            return;
        }

        // reset 
        if (reset) {
            COBOLUtils.prevWorkSpaceUri = undefined;
        } else {
            // already cached?
            if (wsf.fsPath === COBOLUtils.prevWorkSpaceUri?.fsPath) {
                return;
            }
        }

        // stash away current ws
        COBOLUtils.prevWorkSpaceUri = wsf;
        const globPattern = COBOLUtils.getCopyBookGlobPattern(settings);

        await vscode.workspace.findFiles(globPattern).then((uris: vscode.Uri[]) => {
            uris.forEach((uri: vscode.Uri) => {
                const fullPath = uri.fsPath;
                const fileName = InMemoryGlobalCacheHelper.getFilenameWithoutPath(fullPath);
                const fileNameNoExt = path.basename(fileName, path.extname(fileName));
                const c = InMemoryGlobalSymbolCache.defaultCopybooks.get(fileNameNoExt);
                if (c === undefined) {
                    InMemoryGlobalSymbolCache.defaultCopybooks.set(fileNameNoExt, fullPath);
                }
            });
        });
    }


    private static typeToArray(types: string[], prefix: string, typeMap: Map<string, COBOLFileSymbol[]>) {
        for (const [i] of typeMap.entries()) {
            const fileSymbol = typeMap.get(i);
            if (fileSymbol !== undefined) {
                fileSymbol.forEach(function (value: COBOLFileSymbol) {
                    types.push(`${prefix},${i},${value.filename},${value.lnum}`);
                });
            }
        }
    }

    public static clearGlobalCache(): void {
        // only update if we have a workspace
        if (VSWorkspaceFolders.get() === undefined) {
            return;
        }

        const editorConfig = vscode.workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
        editorConfig.update("metadata_symbols", [], false);
        editorConfig.update("metadata_entrypoints", [], false);
        editorConfig.update("metadata_types", [], false);
        editorConfig.update("metadata_files", [], false);
        editorConfig.update("metadata_knowncopybooks", [], false);
        InMemoryGlobalSymbolCache.isDirty = false;
    }

    public static saveGlobalCacheToWorkspace(settings: ICOBOLSettings, update = true): void {
        // only update if we have a workspace
        if (VSWorkspaceFolders.get() === undefined) {
            return;
        }

        // unless we say we want single folder support, never apply an update to it
        if (vscode.workspace.workspaceFile === undefined) {
            return;
        }

        // only update when we are caching
        if (settings.maintain_metadata_cache === false) {
            return;
        }

        if (InMemoryGlobalSymbolCache.isDirty) {
            const symbols: string[] = settings.metadata_symbols;
            const entrypoints: string[] = settings.metadata_entrypoints;
            const types: string[] = settings.metadata_types;
            const files: string[] = settings.metadata_files;
            const knownCopybooks: string[] = settings.metadata_knowncopybooks;
            symbols.length = 0;
            entrypoints.length = 0;
            types.length = 0;
            files.length = 0;
            knownCopybooks.length = 0;

            for (const [i] of InMemoryGlobalSymbolCache.callableSymbols.entries()) {
                if (i !== null && i.length !== 0) {
                    const fileSymbol = InMemoryGlobalSymbolCache.callableSymbols.get(i);
                    if (fileSymbol !== undefined) {
                        fileSymbol.forEach(function (value: COBOLFileSymbol) {
                            update = true;

                            // do not save a callable that is in the defaultCallableSymbol map
                            if (InMemoryGlobalSymbolCache.defaultCallableSymbols.has(i) === false) {
                                if (value.lnum !== 0) {
                                    symbols.push(`${i},${value.filename},${value.lnum}`);
                                } else {
                                    symbols.push(`${i},${value.filename}`);
                                }
                            }
                        });
                    }
                }
                else {
                    update = true;
                }
            }

            for (const [i] of InMemoryGlobalSymbolCache.entryPoints.entries()) {
                if (i !== null && i.length !== 0) {
                    const fileSymbol = InMemoryGlobalSymbolCache.entryPoints.get(i);
                    if (fileSymbol !== undefined) {
                        fileSymbol.forEach(function (value: COBOLFileSymbol) {
                            entrypoints.push(`${i},${value.filename},${value.lnum}`);
                        });
                    }
                }
            }

            for (const [encodedKey,] of InMemoryGlobalSymbolCache.knownCopybooks.entries()) {
                knownCopybooks.push(encodedKey);
            }

            COBOLUtils.typeToArray(types, "T", InMemoryGlobalSymbolCache.types);
            COBOLUtils.typeToArray(types, "I", InMemoryGlobalSymbolCache.interfaces);
            COBOLUtils.typeToArray(types, "E", InMemoryGlobalSymbolCache.enums);

            for (const [fileName] of InMemoryGlobalSymbolCache.sourceFilenameModified.entries()) {
                if (fileName !== null && fileName.length !== 0) {
                    const cws = InMemoryGlobalSymbolCache.sourceFilenameModified.get(fileName);
                    if (cws !== undefined) {
                        files.push(`${cws.lastModifiedTime},${cws.workspaceFilename}`);
                    }
                }
            }

            if (update) {
                try {
                    const editorConfig = vscode.workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
                    editorConfig.update("metadata_symbols", symbols, false);
                    editorConfig.update("metadata_entrypoints", entrypoints, false);
                    editorConfig.update("metadata_types", types, false);
                    editorConfig.update("metadata_files", files, false);
                    editorConfig.update("metadata_knowncopybooks", knownCopybooks, false);
                    InMemoryGlobalSymbolCache.isDirty = false;
                } catch (e) {
                    VSLogger.logException("Failed to update metadata", e as Error);
                }
            }
        }
    }

    public static inCopybookdirs(config: ICOBOLSettings, copybookdir: string): boolean {
        for (const ext of config.copybookdirs) {
            if (ext === copybookdir) {
                return true;
            }
        }

        return false;
    }

    public static migrateCopybooksToWorkspace(settings: ICOBOLSettings): void {
        const fileSearchDirectory = [];
        const extsdir = settings.copybookdirs;

        let updateCopybookdirs = false;
        for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
            const ddir = extsdir[extsdirpos];

            if (COBOLFileUtils.isDirectPath(ddir)) {
                const ws = VSWorkspaceFolders.get();
                if (vscode.workspace !== undefined && ws !== undefined) {
                    if (VSCOBOLFileUtils.isPathInWorkspace(ddir) === false) {
                        if (COBOLFileUtils.isNetworkPath(ddir)) {
                            VSLogger.logMessage(" Adding " + ddir + " to workspace");
                            const uriToFolder = vscode.Uri.file(path.normalize(ddir));
                            vscode.workspace.updateWorkspaceFolders(ws.length, 0, { uri: uriToFolder });
                            updateCopybookdirs = true;
                        }
                    }
                }
            }
        }

        const ws = VSWorkspaceFolders.get();
        if (ws !== undefined) {
            for (const folder of ws) {
                for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                    try {
                        const extdir = extsdir[extsdirpos];

                        let sdir: string;

                        if (COBOLFileUtils.isDirectPath(extdir) === false) {
                            sdir = path.join(folder.uri.fsPath, extdir);

                            if (COBOLFileUtils.isDirectory(sdir)) {
                                if (COBOLFileUtils.isNetworkPath(sdir)) {
                                    if (VSCOBOLFileUtils.isPathInWorkspace(sdir) === false) {
                                        VSLogger.logMessage(" Adding " + sdir + " to workspace");
                                        const uriToFolder = vscode.Uri.file(path.normalize(sdir));
                                        vscode.workspace.updateWorkspaceFolders(ws.length, 0, { uri: uriToFolder });
                                        updateCopybookdirs = true;
                                    } else {
                                        fileSearchDirectory.push(sdir);
                                    }

                                    VSLogger.logMessage(" The directory " + sdir + " for performance should be part of the workspace");
                                } else {
                                    fileSearchDirectory.push(sdir);
                                }
                            }
                        }
                    }
                    catch (e) {
                        VSLogger.logException("dir", e as Error);
                    }
                }
            }
        }

        // update copybookdirs with optimized version
        if (updateCopybookdirs) {
            const editorConfig = vscode.workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
            editorConfig.update("copybookdirs", fileSearchDirectory);
            VSLogger.logMessage("Copybook settings and workspace has been updated.");
        } else {
            VSLogger.logMessage("No copybook directories have been migrated");
        }

    }


    public static indentToCursor(): void {
        if (vscode.window.activeTextEditor) {
            const editor = vscode.window.activeTextEditor;
            const sel = editor.selection;
            const line = editor.document.lineAt(sel.start.line).text;
            const lineTrimmed = line.trimStart();

            editor.edit(edit => {
                const posStartOfLine = new vscode.Position(sel.start.line, 0);
                const endOfLine = new vscode.Position(sel.start.line, line.length);
                const ran = new vscode.Range(posStartOfLine, endOfLine);
                edit.delete(ran);
                // }).then(ok => {
                //     editor.edit(edit => {
                // const posStartOfLine = new vscode.Position(sel.start.line,0);
                const replaceLine = " ".repeat(sel.start.character) + lineTrimmed;
                edit.insert(posStartOfLine, replaceLine);
                // })
                // eslint-disable-next-line @typescript-eslint/no-unused-vars
            }).then(ok => {
                editor.selection = sel;
            });
        }
    }

    public static leftAdjustLine(): void {
        if (vscode.window.activeTextEditor) {
            const editor = vscode.window.activeTextEditor;
            const sel = editor.selection;
            const line = editor.document.lineAt(sel.start.line).text;
            const lineTrimmed = line.trimStart();
            const posStartOfLine = new vscode.Position(sel.start.line, 0);

            editor.edit(edit => {
                const endOfLine = new vscode.Position(sel.start.line, line.length);
                const ran = new vscode.Range(posStartOfLine, endOfLine);
                edit.delete(ran);
                edit.insert(posStartOfLine, lineTrimmed);
                // })
                // eslint-disable-next-line @typescript-eslint/no-unused-vars
            }).then(ok => {
                editor.selection = new vscode.Selection(posStartOfLine, posStartOfLine);
            });
        }
    }

    public static transposeSelection(editor: vscode.TextEditor, textEditor: vscode.TextEditorEdit): void {
        for (const selection of editor.selections) {
            if (selection.anchor.isEqual(selection.active)) {
                const position = new vscode.Position(selection.active.line, selection.active.character);
                const lastPositionInLine = editor.document.lineAt(position.line).range.end;
                if (!(position.isEqual(lastPositionInLine) || position.character == 0)) {
                    const nextSelection = new vscode.Selection(position, new vscode.Position(position.line, position.character + 1));
                    const nextChar = editor.document.getText(nextSelection);
                    textEditor.delete(nextSelection);
                    const prevPosition = new vscode.Position(position.line, position.character - 1);
                    textEditor.insert(prevPosition, nextChar);
                }
            } else {
                const startPosition = new vscode.Position(selection.anchor.line, selection.anchor.character);
                const endPosition = new vscode.Position(selection.active.line, selection.active.character);
                const nextSelection = new vscode.Selection(startPosition, new vscode.Position(startPosition.line, startPosition.character + 1));
                const nextChar = editor.document.getText(nextSelection);
                textEditor.delete(nextSelection);
                const prevPosition = new vscode.Position(endPosition.line, endPosition.character);
                textEditor.insert(prevPosition, nextChar);
            }
        }
    }

    public static extractSelectionTo(activeTextEditor: vscode.TextEditor, para: boolean): void {
        const sel = activeTextEditor.selection;

        const ran = new vscode.Range(sel.start, sel.end);
        const text = activeTextEditor.document.getText(ran);


        // TODO - Should put something in place to search forward of current line
        //        to find the best position..

        vscode.window.showInputBox({
            prompt: para ? "New paragraph name?" : "New section name?",
            validateInput: (text: string): string | undefined => {
                if (!text || text.indexOf(" ") !== -1) {
                    return "Invalid paragraph or section";
                } else {
                    return undefined;
                }
            }
        }).then(value => {
            // leave early
            if (value === undefined) {
                return;
            }

            activeTextEditor.edit(edit => {
                edit.replace(ran, "           perform "
                    + value + "\n\n       "
                    + value
                    + (para ? ".\n" : " section.\n")
                    + text
                    + "\n           .\n");
            });
        });

    }

    private static pad(num: number, size: number): string {
        let s = num + "";
        while (s.length < size) {
            s = "0" + s;
        }
        return s;
    }

    public static getMFUnitAnsiColorConfig(): boolean {
        const editorConfig = vscode.workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
        let expEnabled = editorConfig.get<boolean>("mfunit.diagnostic.color");
        if (expEnabled === undefined || expEnabled === null) {
            expEnabled = false;
        }
        return expEnabled;
    }

    public static resequenceColumnNumbers(activeEditor: vscode.TextEditor | undefined, startValue: number, increment: number): void {
        if (activeEditor === undefined) {
            return;
        }

        const edits = new vscode.WorkspaceEdit();
        const uri = activeEditor.document.uri;

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const lineAt = activeEditor.document.lineAt(l);
            const text = lineAt.text;

            if (text.length > 6) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l, 6);
                const range = new vscode.Range(startPos, endPos);
                const padString = COBOLUtils.pad(startValue + (l * increment), 6);
                edits.replace(uri, range, padString);
            }
        }

        vscode.workspace.applyEdit(edits);
    }

    public static removeColumnNumbers(activeEditor: vscode.TextEditor): void {
        const edits = new vscode.WorkspaceEdit();
        const uri = activeEditor.document.uri;

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const lineAt = activeEditor.document.lineAt(l);
            const text = lineAt.text;

            if (text.length > 6) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l, 6);
                const range = new vscode.Range(startPos, endPos);
                edits.replace(uri, range, "      ");
            }
        }
        vscode.workspace.applyEdit(edits);
    }

    public static RemoveIdentificationArea(activeEditor: vscode.TextEditor): void {
        const edits = new vscode.WorkspaceEdit();
        const uri = activeEditor.document.uri;

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const lineAt = activeEditor.document.lineAt(l);
            const text = lineAt.text;

            if (text.length > 73) {
                const startPos = new vscode.Position(l, 72);
                const endPos = new vscode.Position(l, text.length);
                const range = new vscode.Range(startPos, endPos);
                edits.delete(uri, range);
            }
        }
        vscode.workspace.applyEdit(edits);
    }

    public static RemoveComments(activeEditor: vscode.TextEditor): void {
        const uri = activeEditor.document.uri;
        const edits = new vscode.WorkspaceEdit();
        const delimiters: string[] = [];
        const removeRanges: boolean[] = [];

        delimiters.push("\\*>");
        removeRanges.push(true);
        delimiters.push("^......\\*");
        removeRanges.push(false);

        // traverse all the lines
        for (let l = 0; l < activeEditor.document.lineCount; l++) {
            const line = activeEditor.document.lineAt(l);
            let matched = false;
            for (let i = 0; i < delimiters.length; i++) {
                if (!matched) {
                    const expression = delimiters[i].replace(/\//ig, "\\/");
                    const removeRange = removeRanges[i];
                    const regEx = new RegExp(expression, "ig");
                    const match = regEx.exec(line.text);
                    if (match) {
                        if (removeRange) {
                            const startPos = new vscode.Position(l, match.index);
                            const endPos = new vscode.Position(l, line.text.length);
                            const range = new vscode.Range(startPos, endPos);
                            edits.delete(uri, range);
                            activeEditor.document.getText(range);
                        } else {
                            const startPos = new vscode.Position(l, match.index);
                            const endPos = new vscode.Position(l + 1, 0);
                            const range = new vscode.Range(startPos, endPos);
                            edits.delete(uri, range);
                        }

                        matched = true;
                    }
                }
            }
        }

        vscode.workspace.applyEdit(edits);
    }

    private static isValidKeywordOrStorageKeyword(languageId: string, keyword: string): boolean {
        const keywordLower = keyword.toLowerCase();
        const isKeyword = getCOBOLKeywordDictionary(languageId).has(keywordLower);
        if (isKeyword) {
            return true;
        }

        if (cobolStorageKeywordDictionary.has(keywordLower)) {
            return true;
        }

        return cobolRegistersDictionary.has(keywordLower);
    }

    public static foldTokenLine(text: string, current: COBOLSourceScanner | undefined, action: FoldAction, foldConstantsToUpper: boolean, languageid: string, settings: ICOBOLSettings, defaultFoldStyle: intellisenseStyle): string {
        let newtext = text;
        const args: string[] = [];

        SplitTokenizer.splitArgument(text, args);
        const textLower = text.toLowerCase();
        let lastPos = 0;
        let foldstyle: intellisenseStyle = defaultFoldStyle; //settings.intellisense_style;
        for (let ic = 0; ic < args.length; ic++) {
            let arg = args[ic];
            if (arg.endsWith(".")) {
                arg = arg.substr(0, arg.length - 1);
            }

            const argLower = arg.toLowerCase();
            const ipos = textLower.indexOf(argLower, lastPos);
            let actionIt = false;

            switch (action) {
                case FoldAction.PerformTargets:
                    if (current !== undefined) {
                        actionIt = current.sections.has(argLower);
                        if (actionIt === false) {
                            actionIt = current.paragraphs.has(argLower);
                            foldstyle = VSCustomIntelliseRules.Default.findCustomIStyle(settings, argLower, foldstyle);
                        }
                    }
                    break;

                case FoldAction.ConstantsOrVariables:
                    if (current !== undefined) {
                        actionIt = current.constantsOrVariables.has(argLower);
                        if (actionIt) {
                            foldstyle = VSCustomIntelliseRules.Default.findCustomIStyle(settings, arg, foldstyle);
                            if (foldConstantsToUpper) {
                                const cvars = current.constantsOrVariables.get(argLower);
                                if (cvars !== undefined) {
                                    for (const cvar of cvars) {
                                        if (cvar.tokenType === COBOLTokenStyle.Constant) {
                                            foldstyle = intellisenseStyle.UpperCase;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break;

                case FoldAction.Keywords:
                    actionIt = COBOLUtils.isValidKeywordOrStorageKeyword(languageid, argLower);
                    if (actionIt) {
                        foldstyle = VSCustomIntelliseRules.Default.findCustomIStyle(settings, argLower, foldstyle);
                    }
                    break;
            }

            if (actionIt && foldstyle !== undefined) {
                switch (foldstyle) {
                    case intellisenseStyle.LowerCase:
                        {
                            if (argLower !== arg) {
                                const tmpline = newtext.substr(0, ipos) + argLower + newtext.substr(ipos + arg.length);
                                newtext = tmpline;
                                lastPos += arg.length;
                            }
                        }
                        break;
                    case intellisenseStyle.UpperCase:
                        {
                            const argUpper = arg.toUpperCase();
                            if (argUpper !== arg) {
                                const tmpline = newtext.substr(0, ipos) + argUpper + newtext.substr(ipos + arg.length);
                                newtext = tmpline;
                                lastPos += arg.length;
                            }
                        }
                        break;
                    case intellisenseStyle.CamelCase:
                        {
                            const camelArg = SourceScannerUtils.camelize(arg);
                            if (camelArg !== arg) {
                                const tmpline = newtext.substr(0, ipos) + camelArg + newtext.substr(ipos + arg.length);
                                newtext = tmpline;
                                lastPos += arg.length;
                            }
                        }
                        break;
                }
            }
        }

        // has it changed?
        if (newtext !== text) {
            return newtext;
        }
        return text;
    }

    public static foldToken(
        externalFeatures: IExternalFeatures,
        settings: ICOBOLSettings,
        activeEditor: vscode.TextEditor,
        action: FoldAction,
        languageid: string,
        defaultFoldStyle: intellisenseStyle): void {
        const uri = activeEditor.document.uri;

        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(activeEditor.document, settings);
        if (current === undefined) {
            VSLogger.logMessage(`Unable to fold ${externalFeatures}, as it is has not been parsed`);
            return;
        }
        const file = current.sourceHandler;
        const edits = new vscode.WorkspaceEdit();

        // traverse all the lines
        for (let l = 0; l < file.getLineCount(); l++) {
            const text = file.getLine(l, false);
            if (text === undefined) {
                break;      // eof
            }

            const newtext = COBOLUtils.foldTokenLine(text, current, action, settings.format_constants_to_uppercase, languageid, settings, defaultFoldStyle);

            // one edit per line to avoid the odd overlapping error
            if (newtext !== text) {
                const startPos = new vscode.Position(l, 0);
                const endPos = new vscode.Position(l, newtext.length);
                const range = new vscode.Range(startPos, endPos);
                edits.replace(uri, range, newtext);
            }
        }
        vscode.workspace.applyEdit(edits);
    }

    private static readonly storageAlignItems: string[] = [
        "picture",
        "pic",
        "usage",
        "binary-char",
        "binary-double",
        "binary-long",
        "binary-short",
        "boolean",
        "character",
        "comp-1",
        "comp-2",
        "comp-3",
        "comp-4",
        "comp-5",
        "comp-n",
        "comp-x",
        "comp",
        "computational-1",
        "computational-2",
        "computational-3",
        "computational-4",
        "computational-5",
        "computational-n",
        "computational-x",
        "computational",
        "conditional-value",
        "constant",
        "decimal",
        "external",
        "float-long",
        "float-short",
        "signed-int",
        "signed-long",
        "signed-short",
        "value",
        "as",
        "redefines",
        "renames",
        "pointer"
    ];

    public static getStorageItemPosition(line: string): number {
        for (const storageAlignItem of this.storageAlignItems) {
            const pos = line.toLowerCase().indexOf(" " + storageAlignItem);
            if (pos !== -1) {
                const afterCharPos = 1 + pos + storageAlignItem.length;
                const afterChar = line.charAt(afterCharPos);
                if (afterChar === " " || afterChar === ".") {
                    return pos + 1;
                }
                // VSLogger.logMessage(`afterChar is [${afterChar}]`);
            }
        }
        return -1;
    }

    private static getAlignItemFromSelections(editor: vscode.TextEditor, sels: readonly vscode.Selection[], style: AlignStyle): number {
        let siposa_first = -1;
        let siposa_left = -1;
        let siposa_right = -1;

        for (const sel of sels) {
            for (let startLine = sel.start.line; startLine <= sel.end.line; startLine++) {
                const textSelection = editor.document.lineAt(startLine).text;
                const line = textSelection.trimEnd();
                const sipos = this.getStorageItemPosition(line);

                if (siposa_first === -1) {
                    siposa_first = sipos;
                }

                if (siposa_left === -1) {
                    siposa_left = sipos;
                }

                if (sipos > siposa_right) {
                    siposa_right = sipos;
                }

                if (sipos < siposa_left) {
                    siposa_left = sipos;
                }
            }
        }

        switch (style) {
            case AlignStyle.First: return siposa_first;
            case AlignStyle.Left: return siposa_left;
            case AlignStyle.Right: return siposa_right;
            case AlignStyle.Center: return Math.trunc((siposa_left + siposa_right) / 2);
        }
    }

    public static alignStorage(style: AlignStyle): void {
        const editor = vscode.window.activeTextEditor;
        if (editor) {
            const sels = editor.selections;
            editor.edit(edits => {
                const siposa_from_selection = COBOLUtils.getAlignItemFromSelections(editor, sels, style);

                if (siposa_from_selection !== -1) {
                    for (const sel of sels) {
                        for (let startLine = sel.start.line; startLine <= sel.end.line; startLine++) {
                            const textSelection = editor.document.lineAt(startLine).text;
                            const ran = new vscode.Range(new vscode.Position(startLine, 0),
                                new vscode.Position(startLine, textSelection.length));
                            const line = textSelection.trimEnd();
                            const sipos = this.getStorageItemPosition(line);
                            if (sipos !== -1) {
                                if (sipos !== siposa_from_selection) {
                                    const line_left = line.substring(0, sipos).trimEnd().padEnd(siposa_from_selection - 1) + " ";
                                    const line_right = line.substring(sipos).trimStart();
                                    const newtext = line_left + line_right;
                                    edits.replace(ran, newtext);
                                }
                            }
                        }
                    }
                }
            });
        }
    }

    public static enforceFileExtensions(settings: ICOBOLSettings, activeEditor: vscode.TextEditor, externalFeatures: IExternalFeatures, verbose: boolean, requiredLanguage: string) {
        // const fileConfig = vscode.workspace
        const filesConfig = vscode.workspace.getConfiguration("files");

        const filesAssociationsConfig = filesConfig.get<{ [name: string]: string }>("associations") ?? {} as { [key: string]: string }
        let updateRequired = false;

        const fileAssocMap = new Map<string, string>();
        let fileAssocCount = 0;
        for (const assoc in filesAssociationsConfig) {
            const assocTo = filesAssociationsConfig[assoc];
            if (verbose && fileAssocCount === 0) {
                externalFeatures.logMessage("Active File associations");
            }
            if (verbose) {
                externalFeatures.logMessage(` ${assoc} = ${assocTo}`)
            }

            // grab back
            if (assocTo === "cobol") {
                filesAssociationsConfig[assoc] = requiredLanguage;
                updateRequired = true;
            }
            fileAssocMap.set(assoc, assocTo);
            fileAssocCount++;
        }


        for (const ext of settings.program_extensions) {
            const key = `*.${ext}`;
            const assocTo = fileAssocMap.get(key);
            if (assocTo !== undefined) {
                if (assocTo !== requiredLanguage) {
                    // steal back
                    if (verbose) {
                        externalFeatures.logMessage(` WARNING: ${ext} is associated with ${assocTo}`);
                    }
                    filesAssociationsConfig[key] = requiredLanguage;
                    updateRequired = true;
                }
            } else {
                filesAssociationsConfig[key] = requiredLanguage
                updateRequired = true;
            }
        }

        if (updateRequired) {
            if (VSWorkspaceFolders.get() === undefined) {
                filesConfig.update("associations", filesAssociationsConfig, vscode.ConfigurationTarget.Global);
            } else {
                filesConfig.update("associations", filesAssociationsConfig, vscode.ConfigurationTarget.Workspace);
            }
        }
    }

    public static padTo72() {
        if (vscode.window.activeTextEditor) {
            const editor = vscode.window.activeTextEditor;
            const sel = editor.selection;
            const line = editor.document.lineAt(sel.start.line).text;

            editor.edit(edit => {
                const posStartOfLine = new vscode.Position(sel.start.line, 0);
                const endOfLine = new vscode.Position(sel.start.line, line.length);
                const ran = new vscode.Range(posStartOfLine, endOfLine);
                edit.delete(ran);

                const extraSpaces = line.length < 72 ? 72 - line.length : 0;
                const replaceLine = line + " ".repeat(extraSpaces);
                edit.insert(posStartOfLine, replaceLine);
            });
        }

    }

    public static selectionToHEX(cobolify: boolean) {
        if (vscode.window.activeTextEditor) {
            const editor = vscode.window.activeTextEditor;

            const document = editor.document;
            const selection = editor.selection;

            const ascii = document.getText(selection);
            const hex = COBOLUtils.a2hex(ascii, cobolify);
            editor.edit(editBuilder => {
                editBuilder.replace(selection, hex);
            });
        }
    }

    public static selectionHEXToASCII() {
        if (vscode.window.activeTextEditor) {
            const editor = vscode.window.activeTextEditor;

            const document = editor.document;
            const selection = editor.selection;

            const hex = document.getText(selection);
            const ascii = COBOLUtils.hex2a(hex);
            editor.edit(editBuilder => {
                editBuilder.replace(selection, ascii);
            });
        }
    }

    public static selectionToNXHEX(cobolify: boolean) {
        if (vscode.window.activeTextEditor) {
            const editor = vscode.window.activeTextEditor;

            const document = editor.document;
            const selection = editor.selection;

            const ascii = document.getText(selection);
            const hex = COBOLUtils.a2nx(ascii, cobolify);
            editor.edit(editBuilder => {
                editBuilder.replace(selection, hex);
            });
        }
    }

    // let buffer = Buffer.from(string, 'utf16le')
    // private static isControl(ch: string, index=0) {
    //     var code = ch.charCodeAt(index);
    //     return ((code >= 0x0000 && code <= 0x001f) || (code >= 0x007f && code <= 0x009f));
    // }

    private static isControlCode(code: number) {
        return ((code >= 0x0000 && code <= 0x001f) || (code >= 0x007f && code <= 0x009f));
    }

    public static a2nx(str: string, cobolify: boolean): string {
        const arr = [];
        for (let i = 0, l = str.length; i < l; i++) {

            const chr = str.charAt(i);
            const chrBuffer = Buffer.from(chr, "utf16le").reverse(); // make big endian

            for (let j = 0; j < chrBuffer.length; j++) {
                let hex = Number(chrBuffer[j]).toString(16).toUpperCase();
                if (hex.length === 1) {
                    hex = "0" + hex;
                }
                arr.push(hex);
            }

        }
        const hexString = arr.join("");

        return cobolify ? `NX"${hexString}"` : hexString;
    }

    public static nxhex2a(hexx4: string,): string {
        if (hexx4.length >= 4) {
            if ((hexx4.charAt(0) === "N" || hexx4.charAt(0) === "n") &&
                (hexx4.charAt(1) === "X" || hexx4.charAt(0) === "x")) {
                if (hexx4.charAt(2) === "\"" || hexx4.charAt(2) === "'") {
                    if (hexx4.endsWith("\"") || hexx4.endsWith("'")) {
                        const hexx4len = hexx4.length;
                        hexx4 = hexx4.substring(3, hexx4len - 1);
                    }
                }
            }
        }

        const buf = Buffer.from(hexx4, "hex");
        const len = buf.length / 2;
        const arr = new Array(len);

        for (let i = 0; i < len; i++) {
            arr[i] = buf.readUInt16BE(i * 2);
        }
        return String.fromCharCode(...arr);
    }

    public static a2hex(str: string, cobolify: boolean): string {
        const arr = [];
        for (let i = 0, l = str.length; i < l; i++) {
            let hex = Number(str.charCodeAt(i)).toString(16).toUpperCase();
            if (hex.length === 1) {
                hex = "0" + hex;
            }
            arr.push(hex);
        }
        const hexString = arr.join("");

        return cobolify ? `X"${hexString}"` : hexString;
    }

    public static hex2a(hex: string): string {
        if (hex.length >= 3) {
            if (hex.charAt(0) === "X" || hex.charAt(0) === "x") {
                if (hex.charAt(1) === "\"" || hex.charAt(1) === "'") {
                    if (hex.endsWith("\"") || hex.endsWith("'")) {
                        const hexlen = hex.length;
                        hex = hex.substring(2, hexlen - 1);
                    }
                }
            }
        }

        if (!(hex.length % 2 === 0)) {
            return hex;
        }
        let str = "";

        for (let i = 0; i < hex.length; i += 2) {
            const hexChars = hex.substring(i, i + 2);
            const charCode = parseInt(hexChars, 16);
            if (this.isControlCode(charCode)) {
                str += `[${hexChars}]`
            } else {
                str += String.fromCharCode(charCode);
            }
        }
        return str;
    }

    // public static dumpCallTargets(activeEditor: vscode.TextEditor, externalFeatures: IExternalFeatures) {
    //     const settings = VSCOBOLConfiguration.get();

    //     const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(activeEditor.document, settings);
    //     if (current === undefined) {
    //         externalFeatures.logMessage(`Unable to fold ${externalFeatures}, as it is has not been parsed`);
    //         return;
    //     }

    //     const exampleMap = new Map<string, string>();
    //     const snippetMap = new Map<string, string>();
    //     const paramDeclarationMap = new Map<string, string>();

    //     const gcf = VSCOBOLSourceScanner.getCachedObject(activeEditor.document, settings);
    //     if (gcf !== undefined) {
    //         for (const [target, params] of gcf.callTargets) {
    //             let actualName = params.OriginalToken;
    //             if (actualName.length > 1 && (actualName[0] !== "\"")) {
    //                 const possibleName = gcf.constantsOrVariables.get(target.toLowerCase());
    //                 if (possibleName !== undefined) {
    //                     const firstToken: COBOLToken = possibleName[0];
    //                     const sourceHandler = new FileSourceHandler(firstToken.filename, externalFeatures);
    //                     const lineAtToken = sourceHandler.getLine(firstToken.startLine, true);
    //                     if (lineAtToken !== undefined) {
    //                         const lineAfterToken = lineAtToken.substring(firstToken.startColumn + firstToken.tokenName.length).trimStart();
    //                         const lineAfterTokenLower = lineAfterToken.toLowerCase();
    //                         const indexOfValue = lineAfterTokenLower.indexOf("value");
    //                         if (indexOfValue !== -1) {
    //                             actualName = lineAfterToken.substring(indexOfValue + 5).trim();
    //                         }
    //                     }
    //                 }
    //             }

    //             actualName = COBOLSourceScanner.trimLiteral(actualName);
    //             // const targetCall = KnownAPIs.getCallTarget(actualName);
    //             // externalFeatures.logMessage(` description : "${targetCall?.description}"`);

    //             const sbExample = new StringBuilder();
    //             const sbSnippetBody = new StringBuilder();
    //             const sbParamDecl = new StringBuilder();

    //             if (params.CallParameters.length === 0) {
    //                 sbExample.AppendLine(`call "${actualName}"`);
    //                 sbSnippetBody.AppendLine(`call "${actualName}"`);
    //             } else {
    //                 sbExample.AppendLine(`call "${actualName}" using`);
    //                 sbSnippetBody.AppendLine(`call "${actualName}" using`);
    //                 let paramCounter = 1;
    //                 for (const param of params.CallParameters) {
    //                     //${1:CONDITION}"
    //                     switch (param.using) {
    //                         case UsingState.BY_VALUE:
    //                             sbExample.AppendLine(` by value ${param.name}`);
    //                             sbSnippetBody.AppendLine(` by value \${${paramCounter}:${param.name}}`);
    //                             break;
    //                         case UsingState.BY_REF:
    //                             sbExample.AppendLine(` by reference ${param.name}`);
    //                             sbSnippetBody.AppendLine(` by reference \${${paramCounter}:${param.name}}`);
    //                             break;
    //                         case UsingState.RETURNING:
    //                             sbExample.AppendLine(` returning ${param.name}`);
    //                             sbSnippetBody.AppendLine(` returning \${${paramCounter}:${param.name}}`);
    //                             break;
    //                     }

    //                     const possibleName = gcf.constantsOrVariables.get(param.name.toLowerCase());
    //                     if (possibleName !== undefined) {
    //                         const firstToken: COBOLToken = possibleName[0];
    //                         const sourceHandler = new FileSourceHandler(firstToken.filename, externalFeatures);
    //                         const lineAtToken = sourceHandler.getLine(firstToken.startLine, true);
    //                         if (lineAtToken !== undefined) {
    //                             let lineAfterToken = lineAtToken.substring(firstToken.startColumn + firstToken.tokenName.length).trim();
    //                             if (lineAfterToken.endsWith(".")) {
    //                                 lineAfterToken = lineAfterToken.slice(0, -1);
    //                             }
    //                             if (lineAfterToken.toLowerCase().indexOf("typedef") === -1) {
    //                                 sbParamDecl.AppendLine(`${param.name} => ${lineAfterToken}`);
    //                             }
    //                         }
    //                     }
    //                     paramCounter++;
    //                 }
    //                 sbExample.AppendLine("end-call");
    //                 sbSnippetBody.AppendLine("end-call");
    //                 sbSnippetBody.AppendLine("${0}");

    //             }

    //             exampleMap.set(actualName, sbExample.ToString());
    //             snippetMap.set(actualName, sbSnippetBody.ToString());
    //             paramDeclarationMap.set(actualName, sbParamDecl.ToString());
    //         }
    //         // const exampleArray = Object.fromEntries(exampleMap);
    //         // externalFeatures.logMessage(`${JSON.stringify(exampleArray)}`);
    //         // const snipperArray = Object.fromEntries(snippetMap);
    //         // externalFeatures.logMessage(`${JSON.stringify(snipperArray)}`);
    //         for (const [a, b] of exampleMap) {
    //             const decls = paramDeclarationMap.get(a);
    //             const declString = decls === undefined ? "" : decls + "\r\n" + b;
    //             const varDecls = `[ "${a}", ${JSON.stringify(declString)} ],`;
    //             externalFeatures.logMessage(varDecls);
    //         }
    //     }
    // }


    public static getDebugConfig(workspaceFolder: vscode.WorkspaceFolder, debugFile: string): vscode.DebugConfiguration {

        // "type": "cobol",
        // "request": "launch",
        // "name": "COBOL: Launch",
        // "program": "${workspaceFolder}/HelloWorld",
        // "cwd": "${workspaceFolder}",
        // "stopOnEntry": true

        return {
            name: "Debug COBOL",
            type: "cobol",
            request: "launch",
            cwd: `${workspaceFolder.uri.fsPath}`,
            program: `${debugFile}`,
            stopOnEntry: true
        };
    }

    public static runOrDebug(fsPath: string, debug: boolean): void {
        if (commandTerminal === undefined) {
            commandTerminal = vscode.window.createTerminal(commandTerminalName);
        }

        let prefRunner = "";
        let prefRunnerDebug = "";
        const fsDir = path.dirname(fsPath);

        if (fsPath.endsWith("acu")) {
            if (COBOLFileUtils.isWin32) {
                prefRunner = "wrun32";
                prefRunnerDebug = debug ? "-d " : "";
            } else {
                prefRunner = "runcbl";
                prefRunnerDebug = debug ? "-d " : "";
            }

            commandTerminal.show(true);
            commandTerminal.sendText(`${prefRunner} ${prefRunnerDebug}${fsPath}`);
            return;
        }

        // TODO: need to add .NET dll support!
        if (fsPath.endsWith("int") || fsPath.endsWith("gnt") || fsPath.endsWith("so") || fsPath.endsWith("dll")) {
            if (COBOLUtils.mfExtension === undefined) {
                if (COBOLFileUtils.isWin32) {
                    prefRunner = "run";
                    prefRunnerDebug = debug ? "(+A) " : "";
                } else {
                    prefRunner = debug ? "anim" : "cobrun";
                }

                commandTerminal.show(true);
                commandTerminal.sendText(`${prefRunner} ${prefRunnerDebug}${fsPath}`);
                return;
            } else {
                const workspacePath = VSCOBOLFileUtils.getBestWorkspaceFolder(fsDir);
                if (workspacePath !== undefined) {
                    vscode.debug.startDebugging(workspacePath, COBOLUtils.getDebugConfig(workspacePath, fsPath));
                }
            }
        }
    }
}

