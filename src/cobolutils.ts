import * as vscode from 'vscode';
import path from 'path';
import COBOLSourceScanner, { splitArgument, camelize, COBOLTokenStyle } from './cobolsourcescanner';
import { cobolKeywordDictionary, cobolRegistersDictionary, cobolStorageKeywordDictionary } from './keywords/cobolKeywords';
import { logMessage, isDirectory, logException, COBOLStatUtils } from './extension';
import { VSCodeSourceHandler } from './vscodesourcehandler';
import VSCOBOLSourceScanner from './vscobolscanner';
import { InMemoryGlobalCacheHelper, InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { writeFileSync } from 'fs';
import { COBOLFileUtils } from './opencopybook';
import { VSCOBOLConfiguration } from './configuration';
import { getWorkspaceFolders } from './cobolfolders';
import { ICOBOLSettings } from './iconfiguration';
import { COBOLFileSymbol } from './cobolglobalcache';
import { COBOLWorkspaceSymbolCacheHelper } from './cobolworkspacecache';

export enum FoldStyle {
    LowerCase = 1,
    UpperCase = 2,
    CamelCase = 3
}

export enum FoldAction {
    PerformTargets = 1,
    ConstantsOrVariables = 2,
    Keywords = 3
}

export class COBOLUtils {
    static getCopybookGlobPattern(config: ICOBOLSettings): string {
        let globString = "*.{";
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

    static async populateDefaultCallableSymbols(): Promise<void> {
        const config = VSCOBOLConfiguration.get();
        const globPattern = COBOLUtils.getCopybookGlobPattern(config);

        await vscode.workspace.findFiles(globPattern).then((uris: vscode.Uri[]) => {
            uris.forEach((uri: vscode.Uri) => {
                const fullPath = uri.fsPath;
                const fileName = InMemoryGlobalCacheHelper.getFilenameWithoutPath(fullPath);
                const fileNameNoExt = path.basename(fileName, path.extname(fileName));
                const fileNameNoExtLower = fileNameNoExt.toLowerCase();
                const c = InMemoryGlobalSymbolCache.callableSymbols.get(fileNameNoExtLower);
                if (c === undefined) {
                    COBOLWorkspaceSymbolCacheHelper.addSymbol(fileName, fileNameNoExtLower);
                }
            });
        });
        COBOLUtils.saveGlobalCacheToWorkspace(config);
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

    public static saveGlobalCacheToWorkspace(settings: ICOBOLSettings, update = true): void {
        // only update if we have a workspace
        if (getWorkspaceFolders() === undefined) {
            return;
        }

        // unless we say we want single folder support, never apply an update to it
        if (settings.maintain_metadata_cache_single_folder == false && vscode.workspace.workspaceFile === undefined) {
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
                const fileSymbol = InMemoryGlobalSymbolCache.callableSymbols.get(i);
                if (fileSymbol !== undefined) {
                    fileSymbol.forEach(function (value: COBOLFileSymbol) {
                        symbols.push(`${i},${value.filename}`);
                    });
                }
            }

            for (const [i] of InMemoryGlobalSymbolCache.entryPoints.entries()) {
                const fileSymbol = InMemoryGlobalSymbolCache.entryPoints.get(i);
                if (fileSymbol !== undefined) {
                    fileSymbol.forEach(function (value: COBOLFileSymbol) {
                        entrypoints.push(`${i},${value.filename},${value.lnum}`);
                    });
                }
            }

            for (const [encodedKey,] of InMemoryGlobalSymbolCache.knownCopybooks.entries()) {
                knownCopybooks.push(encodedKey);
            }

            COBOLUtils.typeToArray(types, "T", InMemoryGlobalSymbolCache.types);
            COBOLUtils.typeToArray(types, "I", InMemoryGlobalSymbolCache.interfaces);
            COBOLUtils.typeToArray(types, "E", InMemoryGlobalSymbolCache.enums);

            for (const [fileName] of InMemoryGlobalSymbolCache.sourceFilenameModified.entries()) {
                const cws = InMemoryGlobalSymbolCache.sourceFilenameModified.get(fileName);
                if (cws !== undefined) {
                    files.push(`${cws.lastModifiedTime},${cws.workspaceFilename}`);
                }
            }

            if (update) {
                try {
                    const editorConfig = vscode.workspace.getConfiguration('coboleditor');
                    editorConfig.update('metadata_symbols', symbols, false);
                    editorConfig.update('metadata_entrypoints', entrypoints, false);
                    editorConfig.update('metadata_types', types, false);
                    editorConfig.update('metadata_files', files, false);
                    editorConfig.update('metadata_knowncopybooks', knownCopybooks, false);
                    InMemoryGlobalSymbolCache.isDirty = false;
                } catch (e) {
                    logException("Failed to update metadata", e);
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

    public static migrateCopybooksToWorkspace(): void {
        const fileSearchDirectory = [];
        const settings = VSCOBOLConfiguration.get();
        const extsdir = settings.copybookdirs;

        let updateCopybookdirs = false;
        for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
            const ddir = extsdir[extsdirpos];

            if (COBOLFileUtils.isDirectPath(ddir)) {
                const ws = getWorkspaceFolders();
                if (vscode.workspace !== undefined && ws !== undefined) {
                    if (COBOLStatUtils.isPathInWorkspace(ddir) === false) {
                        if (COBOLFileUtils.isNetworkPath(ddir)) {
                            logMessage(" Adding " + ddir + " to workspace");
                            const uriToFolder = vscode.Uri.file(path.normalize(ddir));
                            vscode.workspace.updateWorkspaceFolders(ws.length, 0, { uri: uriToFolder });
                            updateCopybookdirs = true;
                        }
                    }
                }
            }
        }

        const ws = getWorkspaceFolders();
        if (ws !== undefined) {
            for (const folder of ws) {
                for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
                    try {
                        const extdir = extsdir[extsdirpos];

                        let sdir: string;

                        if (COBOLFileUtils.isDirectPath(extdir) === false) {
                            sdir = path.join(folder.uri.fsPath, extdir);

                            if (isDirectory(sdir)) {
                                if (COBOLFileUtils.isNetworkPath(sdir)) {
                                    if (COBOLStatUtils.isPathInWorkspace(sdir) === false) {
                                        logMessage(" Adding " + sdir + " to workspace");
                                        const uriToFolder = vscode.Uri.file(path.normalize(sdir));
                                        vscode.workspace.updateWorkspaceFolders(ws.length, 0, { uri: uriToFolder });
                                        updateCopybookdirs = true;
                                    } else {
                                        fileSearchDirectory.push(sdir);
                                    }

                                    logMessage(" The directory " + sdir + " for performance should be part of the workspace");
                                } else {
                                    fileSearchDirectory.push(sdir);
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

        // update copybookdirs with optimized version
        if (updateCopybookdirs) {
            const editorConfig = vscode.workspace.getConfiguration('coboleditor');
            editorConfig.update('copybookdirs', fileSearchDirectory);
            logMessage("Copybook settings and workspace has been updated.");
        } else {
            logMessage("No copybook directories have been migrated");
        }

    }

    public static extractSelectionToCopybook(activeTextEditor: vscode.TextEditor): void {
        const sel = activeTextEditor.selection;

        const ran = new vscode.Range(sel.start, sel.end);
        const text = activeTextEditor.document.getText(ran);
        const dir = path.dirname(activeTextEditor.document.fileName);

        vscode.window.showInputBox({
            prompt: 'Copybook name?',
            validateInput: (copybook_filename: string): string | undefined => {
                if (!copybook_filename || copybook_filename.indexOf(' ') !== -1 ||
                    copybook_filename.indexOf(".") !== -1 ||
                    COBOLStatUtils.isFile(path.join(dir, copybook_filename + ".cpy"))) {
                    return 'Invalid copybook';
                } else {
                    return undefined;
                }
            }
        }).then(copybook_filename => {
            // leave if we have no filename
            if (copybook_filename === undefined) {
                return;
            }
            const filename = path.join(dir, copybook_filename + ".cpy");
            writeFileSync(filename, text);

            activeTextEditor.edit(edit => {
                edit.replace(ran, "           copy \"" + copybook_filename + ".cpy\".");
            });
        });

    }

    public static extractSelectionTo(activeTextEditor: vscode.TextEditor, para: boolean): void {
        const sel = activeTextEditor.selection;

        const ran = new vscode.Range(sel.start, sel.end);
        const text = activeTextEditor.document.getText(ran);


        // TODO - Should put something in place to search forward of current line
        //        to find the best position..

        vscode.window.showInputBox({
            prompt: para ? 'New paragraph name?' : 'New section name?',
            validateInput: (text: string): string | undefined => {
                if (!text || text.indexOf(' ') !== -1) {
                    return 'Invalid paragraph or section';
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
        const editorConfig = vscode.workspace.getConfiguration('coboleditor');
        let expEnabled = editorConfig.get<boolean>('mfunit.diagnostic.color');
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

    private static isValidKeywordOrStorageKeyword(keyword: string): boolean {
        const keywordLower = keyword.toLowerCase();
        const isKeyword = cobolKeywordDictionary.has(keywordLower);
        if (isKeyword) {
            return true;
        }

        if (cobolStorageKeywordDictionary.has(keywordLower)) {
            return true;
        }

        return cobolRegistersDictionary.has(keywordLower);
    }

    public static foldTokenLine(text: string, current: COBOLSourceScanner, action: FoldAction, foldstyle: FoldStyle, foldConstantsToUpper: boolean): string {
        let newtext = text;
        const args: string[] = [];
        
        splitArgument(text, true, args);
        const textLower = text.toLowerCase();
        let lastPos = 0;
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
                    actionIt = current.sections.has(argLower);
                    if (actionIt === false) {
                        actionIt = current.paragraphs.has(argLower);
                    }
                    break;

                case FoldAction.ConstantsOrVariables:
                    actionIt = current.constantsOrVariables.has(argLower);
                    if (actionIt && foldConstantsToUpper) {
                        const cvars = current.constantsOrVariables.get(argLower);
                        if (cvars !== undefined) {
                            for (const cvar of cvars) {
                                if (cvar.tokenType === COBOLTokenStyle.Constant) {
                                    foldstyle = FoldStyle.UpperCase;
                                }
                            }
                        }
                    }
                    break;

                case FoldAction.Keywords:
                    actionIt = COBOLUtils.isValidKeywordOrStorageKeyword(argLower);
                    break;
            }

            if (actionIt) {
                switch (foldstyle) {
                    case FoldStyle.LowerCase:
                        {
                            if (argLower !== arg) {
                                const tmpline = newtext.substr(0, ipos) + argLower + newtext.substr(ipos + arg.length);
                                newtext = tmpline;
                                lastPos += arg.length;
                            }
                        }
                        break;
                    case FoldStyle.UpperCase:
                        {
                            const argUpper = arg.toUpperCase();
                            if (argUpper !== arg) {
                                const tmpline = newtext.substr(0, ipos) + argUpper + newtext.substr(ipos + arg.length);
                                newtext = tmpline;
                                lastPos += arg.length;
                            }
                        }
                        break;
                    case FoldStyle.CamelCase:
                        {
                            const camelArg = camelize(arg);
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

    public static foldToken(activeEditor: vscode.TextEditor, action: FoldAction, foldstyle: FoldStyle): void {
        const uri = activeEditor.document.uri;

        const settings = VSCOBOLConfiguration.get();
        const file = new VSCodeSourceHandler(activeEditor.document, false);
        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(activeEditor.document, settings);
        if (current === undefined) {
            logMessage(`Unable to fold ${file.getFilename}, as it is has not been parsed`);
            return;
        }

        const edits = new vscode.WorkspaceEdit();
        // traverse all the lines
        for (let l = 0; l < file.getLineCount(); l++) {
            const lineAt = file.getLine(l, false);
            const text = lineAt;

            if (text === undefined) {
                break;      // eof
            }

            const newtext = COBOLUtils.foldTokenLine(text, current, action, foldstyle, settings.format_constants_to_uppercase);

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

}
