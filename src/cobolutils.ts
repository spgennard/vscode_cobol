import * as vscode from 'vscode';
import { workspace } from 'vscode';
import COBOLSourceScanner, { splitArgument, camelize, COBOLTokenStyle } from './cobolsourcescanner';
import { cobolKeywordDictionary, cobolRegistersDictionary, cobolStorageKeywordDictionary } from './keywords/cobolKeywords';
import { logMessage, isDirectory, logException, COBOLStatUtils } from './extension';
import { VSCodeSourceHandler } from './vscodesourcehandler';
import VSCOBOLSourceScanner from './vscobolscanner';
import { GlobalCachesHelper } from "./globalcachehelper";
import { writeFileSync } from 'fs';
import path from 'path';
import { COBOLFileUtils } from './opencopybook';
import { VSCOBOLConfiguration } from './configuration';
import { getWorkspaceFolders } from './cobolfolders';
import { ICOBOLSettings } from './iconfiguration';
import { COBOLFileSymbol, InMemoryGlobalSymbolCache } from './cobolglobalcache';

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

    static async setupCommand() {
        const config = VSCOBOLConfiguration.get();
        const globPattern = COBOLUtils.getCopybookGlobPattern(config);
        logMessage(`Glob pattern ${globPattern}`);

        await vscode.workspace.findFiles(globPattern).then((uris: vscode.Uri[] ) => {
            uris.forEach((uri: vscode.Uri) => {
                const fullPath = uri.fsPath;
                const dirName = path.dirname(fullPath);
                const fileName = fullPath.substr(dirName.length+1);
                const fileNameNoExt = path.basename(fileName, path.extname(fileName));
                const fileNameNoExtLower = fileNameNoExt.toLowerCase();
                const c: COBOLFileSymbol[] | undefined = InMemoryGlobalSymbolCache.callableSymbols.get(fileNameNoExtLower);
                if (c === undefined) {
                    logMessage(` Missing ${fileNameNoExt} in ${fullPath}`);
                    GlobalCachesHelper.addSymbol(fileName, fileNameNoExtLower, 0);
                }
                // logMessage(`path is ${uri.fsPath}`);
            });
        });

        InMemoryGlobalSymbolCache.isDirty=true;
        COBOLUtils.saveGlobalCacheToWorkspace();
    }

    public static loadGlobalCacheFromArray(symbols:string[]):void {
        for(const symbol of symbols) {
            const symbolValues = symbol.split(",");
            if (symbolValues.length === 3) {
                GlobalCachesHelper.addSymbol(symbolValues[1], symbolValues[0], Number.parseInt(symbolValues[2]));
            }
        }
    }

    public static saveGlobalCacheToWorkspace(): void {
        if (InMemoryGlobalSymbolCache.isDirty) {
            const symbols:string[] = [];

            for (const [i] of InMemoryGlobalSymbolCache.callableSymbols.entries()) {
                const fileSymbol: COBOLFileSymbol[] | undefined = InMemoryGlobalSymbolCache.callableSymbols.get(i);
                if (fileSymbol !== undefined) {
                    fileSymbol.forEach(function (value: COBOLFileSymbol) {
                        const dirName = path.dirname(value.filename);
                        const fileName = value.filename.substr(dirName.length+1);
                        symbols.push(`${i},${fileName},${value.lnum}`);
                    });
                }
            }

            const editorConfig = vscode.workspace.getConfiguration('coboleditor');
            editorConfig.update('metadata_callable_symbols', symbols);

            InMemoryGlobalSymbolCache.isDirty = false;
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
                if (workspace !== undefined && ws !== undefined) {
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
            const editorConfig = workspace.getConfiguration('coboleditor');
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
        const editorConfig = workspace.getConfiguration('coboleditor');
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
        const args: string[] = splitArgument(text, true);
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

        const file = new VSCodeSourceHandler(activeEditor.document, false);
        const current: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(activeEditor.document);
        if (current === undefined) {
            logMessage(`Unable to fold ${file.getFilename}, as it is has not been parsed`);
            return;
        }

        const settings = VSCOBOLConfiguration.get();
        const edits = new vscode.WorkspaceEdit();
        // traverse all the lines
        for (let l = 0; l < file.getLineCount(); l++) {
            const lineAt = file.getLine(l);
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
