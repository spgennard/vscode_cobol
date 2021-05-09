'use strict';

import * as path from 'path';

import * as vscode from 'vscode';
import { Range, TextDocument, Definition, Position, CancellationToken, Uri } from 'vscode';
import { getCombinedCopyBookSearchPath, COBOLStatUtils } from './extension';
import { VSCOBOLConfiguration } from './configuration';
import { COBOLSettings, ICOBOLSettings } from './iconfiguration';

export class COBOLFileUtils {
    static readonly isWin32 = process.platform === "win32";

    public static isValidCopybookExtension(filename: string, settings: COBOLSettings): boolean {
        const lastDot = filename.lastIndexOf(".");
        let extension = filename;
        if (lastDot !== -1) {
            extension = filename.substr(1 + lastDot);
        }

        const exts = settings.copybookexts;
        for (let extpos = 0; extpos < exts.length; extpos++) {
            if (exts[extpos] === extension) {
                return true;
            }
        }
        return false;
    }

    public static isValidProgramExtension(filename: string, settings: COBOLSettings): boolean {
        const lastDot = filename.lastIndexOf(".");
        let extension = "";
        if (lastDot !== -1) {
            extension = filename.substr(1 + lastDot);
        }

        const exts = settings.program_extensions;
        for (let extpos = 0; extpos < exts.length; extpos++) {
            if (exts[extpos] === extension) {
                return true;
            }
        }
        return false;
    }

    public static isDirectPath(dir: string): boolean {
        if (dir === undefined && dir === null) {
            return false;
        }

        if (COBOLFileUtils.isWin32) {
            if (dir.length > 2 && dir[1] === ':') {
                return true;
            }

            if (dir.length > 1 && dir[0] === '\\') {
                return true;
            }

            return false;
        }

        if (dir.length > 1 && dir[0] === '/') {
            return true;
        }

        return false;
    }

    // only handle unc filenames
    public static isNetworkPath(dir: string): boolean {
        if (dir === undefined && dir === null) {
            return false;
        }

        if (COBOLFileUtils.isWin32) {
            if (dir.length > 1 && dir[0] === '\\') {
                return true;
            }
        }

        return false;
    }

    public static findCopyBook(filename: string, config: ICOBOLSettings): string {
        if (!filename) {
            return "";
        }

        const hasDot = filename.indexOf(".");

        for (const copybookdir of getCombinedCopyBookSearchPath()) {

            /* check for the file as is.. */
            const firstPossibleFile = path.join(copybookdir, filename);
            if (COBOLStatUtils.isFile(firstPossibleFile)) {
                return firstPossibleFile;
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile = path.join(copybookdir, filename + "." + ext);

                    if (COBOLStatUtils.isFile(possibleFile)) {
                        return possibleFile;
                    }
                }
            }
        }

        return "";
    }

    public static findCopyBookInDirectory(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        if (!filename) {
            return "";
        }

        const hasDot = filename.indexOf(".");

        for (const baseCopybookdir of getCombinedCopyBookSearchPath()) {
            const copybookdir = path.join(baseCopybookdir, inDirectory);

            /* check for the file as is.. */
            const firstPossibleFile = path.join(copybookdir, filename);
            if (COBOLStatUtils.isFile(firstPossibleFile)) {
                return firstPossibleFile;
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile = path.join(copybookdir, filename + "." + ext);

                    if (COBOLStatUtils.isFile(possibleFile)) {
                        return possibleFile;
                    }
                }
            }

        }

        return "";
    }

}

export class COBOLCopyBookProvider implements vscode.DefinitionProvider {
    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        return this.resolveDefinitions(document, position, token);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async resolveDefinitions(doc: TextDocument, pos: Position, ct: CancellationToken): Promise<Definition> {
        const config = VSCOBOLConfiguration.get();
        const line = doc.lineAt(pos);
        const text = line.text;
        const textLower = text.toLowerCase().replace("\t", " ");
        let filename = this.extractCopyBoolFilename(text);
        let inOrOfPos = 0;

        const activeTextEditor = vscode.window.activeTextEditor;
        if (activeTextEditor && filename === undefined) {
            const sel = activeTextEditor.selection;

            const ran = new vscode.Range(sel.start, sel.end);
            const textSelection = activeTextEditor.document.getText(ran);
            if (textSelection !== undefined) {
                filename = textSelection.trim();
            }
        }

        // leave asap
        if (filename === undefined) {
            return [];
        }

        // exec sql include "has" in in the line.. so take care..
        if (textLower.indexOf("copy") !== -1) {
            inOrOfPos = textLower.indexOf(" in ");
            if (inOrOfPos === -1) {
                inOrOfPos = textLower.indexOf(" of ");
            }
            inOrOfPos !== -1 ? inOrOfPos += 4 : inOrOfPos = 0;
        }

        let inDirectory = inOrOfPos !== 0 ? text.substr(inOrOfPos) : "";

        if (inDirectory.length !== 0) {
            let inDirItems = inDirectory.trim();

            if (inDirItems.endsWith(".")) {
                inDirItems = inDirItems.substr(0, inDirItems.length - 1);
            }

            if (inDirItems.endsWith("\"") && inDirItems.startsWith("\"")) {
                inDirItems = inDirItems.substr(1, inDirItems.length - 2);
            }

            if (inDirItems.endsWith("'") && inDirItems.startsWith("'")) {
                inDirItems = inDirItems.substr(1, inDirItems.length - 2);
            }

            inDirectory = inDirItems;
        }

        if (filename !== null && filename.length !== 0) {
            const fullPath = COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(filename.trim(), inDirectory, config);
            if (fullPath.length !== 0) {
                return new vscode.Location(
                    Uri.file(fullPath),
                    new Range(new Position(0, 0), new Position(0, 0))
                );
            }
        }

        return [];

    }

    public static expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {

        if (inDirectory === null || inDirectory.length === 0) {
            const fullPath = COBOLFileUtils.findCopyBook(filename, config);
            if (fullPath.length !== 0) {
                return path.normalize(fullPath);
            }

            return fullPath;
        }

        const fullPath = COBOLFileUtils.findCopyBookInDirectory(filename, inDirectory, config);
        if (fullPath.length !== 0) {
            return path.normalize(fullPath);
        }

        return fullPath;
    }

    private extractCopyBoolFilename(str: string): string | undefined {
        const copyPos = str.toLowerCase().indexOf("copy");
        if (copyPos !== -1) {
            const noCopyStr = str.substr(4 + copyPos).trimLeft();
            const spacePos = noCopyStr.indexOf(" ");
            let justCopyArg = noCopyStr;
            if (spacePos !== -1) {
                justCopyArg = justCopyArg.substr(0, spacePos).trim();
            }

            // remove trailing .
            if (justCopyArg.endsWith(".")) {
                justCopyArg = justCopyArg.substr(0, justCopyArg.length - 1);
                justCopyArg = justCopyArg.trim();
            }

            // remove double quote
            if (justCopyArg.startsWith('"') && justCopyArg.endsWith('"')) {
                justCopyArg = justCopyArg.substr(1, justCopyArg.length - 2);
                justCopyArg = justCopyArg.trim();
                return justCopyArg;
            }

            // remove single quote
            if (justCopyArg.startsWith('\'') && justCopyArg.endsWith('\'')) {
                justCopyArg = justCopyArg.substr(1, justCopyArg.length - 2);
                justCopyArg = justCopyArg.trim();
            }

            return justCopyArg;
        }

        const strLower = str.toLowerCase();
        if (strLower.indexOf("exec") !== -1) {
            if (strLower.includes("sql", strLower.indexOf("exec"))) {
                let includePos = strLower.indexOf("include");
                if (includePos !== -1) {
                    includePos += 7;
                    const strRight = str.substr(includePos).trimLeft();
                    const strRightLower = strRight.toLowerCase();
                    const endExecPos = strRightLower.indexOf("end-exec");
                    if (endExecPos !== -1) {
                        const filename = strRight.substr(0, endExecPos).trim();

                        return filename;
                    }
                }
            }
        }
        return undefined;
    }

}
