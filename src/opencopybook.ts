'use strict';

import { Range, TextDocument, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as vscode from 'vscode';
import * as path from 'path';
import * as process from 'process';
import { getCombinedCopyBookSearchPath, isFile, logMessage } from './extension';
import { VSCOBOLConfiguration } from './configuration';


export function isValidExtension(filename: string): boolean {
    switch (filename) {
        case "tags":
        case ".tag":
        case ".ctags":
        case ".diff":
        case ".c":
        case ".h":
            return false;
    }
    const exts = VSCOBOLConfiguration.getExtentions();
    for (let extpos = 0; extpos < exts.length; extpos++) {
        let ext = exts[extpos];
        if (ext.length !== 0) {
            if (filename.endsWith(ext)) {
                return true;
            }
        } else {
            // true to parse it, if we have no extension
            if (filename.indexOf(".") === -1) {
                return true;
            }
        }
    }
    return false;
}


function extractCopyBoolFilename(str: string) {
    let copyPos = str.toLowerCase().indexOf("copy");
    if (copyPos !== -1) {
        let noCopyStr = str.substr(4 + copyPos).trimLeft();
        let spacePos = noCopyStr.indexOf(" ");
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

    let strLower = str.toLowerCase();
    if (strLower.indexOf("exec") !== -1) {
        if (strLower.includes("sql", strLower.indexOf("exec"))) {
            let includePos = strLower.indexOf("include");
            if (includePos !== -1) {
                includePos += 7
                let strRight = str.substr(includePos).trimLeft();
                let strRightLower = strRight.toLowerCase();
                let endExecPos = strRightLower.indexOf("end-exec");
                if (endExecPos !== -1) {
                    let filename = strRight.substr(0, endExecPos).trim();

                    return filename;
                }
            }
        }
    }
    return "";
}

// only handle unc filenames
export function isNetworkPath(dir: string) {
    if (dir === undefined && dir === null) {
        return false;
    }

    if (process.platform === "win32") {
        if (dir.length > 1 && dir[0] === '\\') {
            return true;
        }
        path.win32.toNamespacedPath
    }

    return false;

}

export function isDirectPath(dir: string) {
    if (dir === undefined && dir === null) {
        return false;
    }

    if (process.platform === "win32") {
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

function findCopyBook(filename: string): string {
    if (!filename) {
        return "";
    }

    let hasDot = filename.indexOf(".");

    for (let copybookdir of getCombinedCopyBookSearchPath()) {
        /* check for the file as is.. */
        let firstPossibleFile = path.join(copybookdir, filename);
        if (isFile(firstPossibleFile)) {
            return firstPossibleFile;
        }

        /* no extension? */
        if (hasDot === -1) {
            // search through the possible extensions
            for (let ext of VSCOBOLConfiguration.getExtentions()) {
                let possibleFile = path.join(copybookdir, filename + "." + ext);

                if (isFile(possibleFile)) {
                    return possibleFile;
                }
            }
        }

    }

    return "";
}


function findCopyBookInDirectory(filename: string, inDirectory: string): string {
    if (!filename) {
        return "";
    }

    let hasDot = filename.indexOf(".");

    for (let baseCopybookdir of getCombinedCopyBookSearchPath()) {
        let copybookdir = path.join(baseCopybookdir, inDirectory);

        /* check for the file as is.. */
        let firstPossibleFile = path.join(copybookdir, filename);
        if (isFile(firstPossibleFile)) {
            return firstPossibleFile;
        }

        /* no extension? */
        if (hasDot === -1) {
            // search through the possible extensions
            for (let ext of VSCOBOLConfiguration.getExtentions()) {
                let possibleFile = path.join(copybookdir, filename + "." + ext);

                if (isFile(possibleFile)) {
                    return possibleFile;
                }
            }
        }

    }

    return "";
}


export function expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string): string {

    if (inDirectory === null || inDirectory.length === 0) {
        let fullPath = findCopyBook(filename);
        if (fullPath.length !== 0) {
            return path.normalize(fullPath);
        }

        return fullPath;
    }

    let fullPath = findCopyBookInDirectory(filename, inDirectory);
    if (fullPath.length !== 0) {
        return path.normalize(fullPath);
    }

    return fullPath;
}

export function provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {

    const line = doc.lineAt(pos);
    const text = line.text;
    const textLower = text.toLowerCase();
    const filename = extractCopyBoolFilename(text);
    let inPos = -1;

    // exec sql include "has" in in the line.. so becareful
    if (textLower.indexOf("copy") !== -1) {
        inPos = textLower.indexOf("in");
    }

    let inDirectory = inPos !== -1 ? text.substr(2 + inPos) : "";

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
        const fullPath = expandLogicalCopyBookToFilenameOrEmpty(filename.trim(), inDirectory);
        if (fullPath.length !== 0) {
            return new vscode.Location(
                Uri.file(fullPath),
                new Range(new Position(0, 0), new Position(0, 0))
            );
        }
    }

    return;
}
