'use strict';

import { Range, TextDocument, workspace, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as vscode from 'vscode';
import * as path from 'path';
import * as process from 'process';
import { getCombinedCopyBookSearchPath, logException, isFile } from './extension';
import { VSCOBOLConfiguration } from './configuration';
import { getWorkspaceFolders } from './cobolfolders';
import { fstat } from 'fs';

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
    let getFirstMatchOrDefault =
        (s: string, pattern: RegExp) => {
            const match = s.match(pattern);
            return match ? match[1] : null;
        };

    let inPos = str.toLowerCase().indexOf(" in ");
    if (inPos !== -1) {
        str = str.substr(0, inPos);
    }
    //const strl = str.toLowerCase();
    let result: string | null;
    if (/copy/i.test(str)) {

        let copyRegs: RegExp[] = [
            new RegExp(".*copy\\s*[\"'](.*)[\"'].*$", "i"),
            new RegExp(".*copy\\s*[\"'](.*)[\"']$", "i"),
            new RegExp(".*copy\\s*[\"'](.*)[\"']\\s+suppress.*$", "i"),
            new RegExp(".*copy\\s*(.*)\\s+suppress.*$", "i"),
            new RegExp(".*copy\\s*[\"'](.*)[\"']\\s+replacing.*$", "i"),
            new RegExp(".*copy\\s*(.*)\\s+replacing.*$", "i"),
            new RegExp(".*copy\\s*(.*)$", "i"),
            new RegExp(".*copy\\s*(.*)\\s.*$", "i"),
            new RegExp(".*copy\\s*(.*)\\.$", "i")
        ];

        for (let regPos = 0; regPos < copyRegs.length; regPos++) {
            try {
                result = getFirstMatchOrDefault(str, copyRegs[regPos]);
                if (result !== null && result.length > 0) {
                    // let a= "Found ["+result+"] test "+regPos+"["+copyRegs+"]";
                    // console.log(a);
                    return result;
                }
            } catch (e) {
                /* continue */
                console.log(e);
                console.log(e.stacktrace);
            }
        }

    }

    //FIXME this could be better
    if (/exec sql include/i.test(str)) {
        try {
            return getFirstMatchOrDefault(str, /exec\\s*sql\\s*include\s(.*)\s*end-exec/);
        } catch (e) {
            /* continue */
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
            for (let ext in VSCOBOLConfiguration.getExtentions()) {
                let possibleFile = path.join(copybookdir, filename + "." + ext);

                if (isFile(possibleFile)) {
                    return possibleFile;
                }
            }
        }

    }

    return "";
}


function findCopyBookInDirectory(filename: string,inDirectory: string): string {
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
            for (let ext in VSCOBOLConfiguration.getExtentions()) {
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
    const filename = extractCopyBoolFilename(text);
    const inPos = text.toLowerCase().indexOf("in");

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
