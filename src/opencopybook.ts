'use strict';

import { Range, TextDocument, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as vscode from 'vscode';
import * as path from 'path';
import * as process from 'process';
import { getCombinedCopyBookSearchPath, isFile } from './extension';
import { VSCOBOLConfiguration } from './configuration';
import { COBOLSettings, ICOBOLSettings } from './iconfiguration';


export function isValidCopybookExtension(filename: string, settings: COBOLSettings): boolean {
    const lastDot = filename.lastIndexOf(".");
    let extension = filename;
    if (lastDot !== -1) {
        extension = filename.substr(1+lastDot);
    }

    switch (extension) {
        case "tag":
        case "ctags":
        case "diff":
        case "c":
        case "h":
            return false;
    }

    const exts = settings.copybookexts;
    for (let extpos = 0; extpos < exts.length; extpos++) {
        if (exts[extpos] === extension) {
            return true;
        }
    }
    return false;
}


function extractCopyBoolFilename(str: string) {
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
    return "";
}

// only handle unc filenames
export function isNetworkPath(dir: string): boolean {
    if (dir === undefined && dir === null) {
        return false;
    }

    if (process.platform === "win32") {
        if (dir.length > 1 && dir[0] === '\\') {
            return true;
        }
    }

    return false;

}

export function isDirectPath(dir: string): boolean {
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

function findCopyBook(filename: string, config: ICOBOLSettings): string {
    if (!filename) {
        return "";
    }

    const hasDot = filename.indexOf(".");

    for (const copybookdir of getCombinedCopyBookSearchPath()) {
        /* check for the file as is.. */
        const firstPossibleFile = path.join(copybookdir, filename);
        if (isFile(firstPossibleFile)) {
            return firstPossibleFile;
        }

        /* no extension? */
        if (hasDot === -1) {
            // search through the possible extensions
            for (const ext of config.copybookexts) {
                const possibleFile = path.join(copybookdir, filename + "." + ext);

                if (isFile(possibleFile)) {
                    return possibleFile;
                }
            }
        }

    }

    return "";
}


function findCopyBookInDirectory(filename: string, inDirectory: string, config: ICOBOLSettings): string {
    if (!filename) {
        return "";
    }

    const hasDot = filename.indexOf(".");

    for (const baseCopybookdir of getCombinedCopyBookSearchPath()) {
        const copybookdir = path.join(baseCopybookdir, inDirectory);

        /* check for the file as is.. */
        const firstPossibleFile = path.join(copybookdir, filename);
        if (isFile(firstPossibleFile)) {
            return firstPossibleFile;
        }

        /* no extension? */
        if (hasDot === -1) {
            // search through the possible extensions
            for (const ext of config.copybookexts) {
                const possibleFile = path.join(copybookdir, filename + "." + ext);

                if (isFile(possibleFile)) {
                    return possibleFile;
                }
            }
        }

    }

    return "";
}


export function expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {

    if (inDirectory === null || inDirectory.length === 0) {
        const fullPath = findCopyBook(filename, config);
        if (fullPath.length !== 0) {
            return path.normalize(fullPath);
        }

        return fullPath;
    }

    const fullPath = findCopyBookInDirectory(filename, inDirectory,config);
    if (fullPath.length !== 0) {
        return path.normalize(fullPath);
    }

    return fullPath;
}

export function provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
    const config=VSCOBOLConfiguration.get();
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
        const fullPath = expandLogicalCopyBookToFilenameOrEmpty(filename.trim(), inDirectory, config);
        if (fullPath.length !== 0) {
            return new vscode.Location(
                Uri.file(fullPath),
                new Range(new Position(0, 0), new Position(0, 0))
            );
        }
    }

    return;
}
