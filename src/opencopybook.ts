'use strict';

import { Range, TextDocument, workspace, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as process from 'process';
import { getcopybookdirs, logMessage } from './extension';
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
    let getFirstMatchOrDefault =
        (s: string, pattern: RegExp) => {
            const match = s.match(pattern);
            return match ? match[1] : null;
        };

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

export function isDirectPath(dir: string) {
    var isWin = process.platform === "win32";

    if (dir === undefined && dir === null) {
        return false;
    }

    if (isWin) {
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

function findFileInDirectory(filename: string, filenameDir: string): string {
    if (!filename) {
        return "";
    }

    // // does the file exist?
    // if (fs.existsSync(filename)) {
    //     var itemStat = fs.statSync(filename);
    //     if (itemStat.isDirectory() === false) {
    //         return filename;
    //     }
    //     return "";
    // }

    // searching in cwd does not make sense, as it can change
    if (filenameDir === '.') {
        return "";
    }

    var fileExtension = filename.split('.').pop();
    var extsdir = getcopybookdirs();
    extsdir.push(filenameDir);
    for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
        var extdir = extsdir[extsdirpos];

        const basefullPath = isDirectPath(extdir) ?
            path.join(extdir, filename) :
            path.join(filenameDir, extdir + path.sep + filename);

        //No extension?
        if (filename === fileExtension) {
            // search through the possible extensions
            const exts = VSCOBOLConfiguration.getExtentions();
            for (let extpos = 0; extpos < exts.length; extpos++) {
                var ext = exts[extpos];
                var possibleFile = basefullPath + (ext.length !== 0 ? "." + ext : "");

                if (fs.existsSync(possibleFile)) {
                    return possibleFile;
                }
            }
        } else {
            if (fs.existsSync(basefullPath)) {
                return basefullPath;
            }
        }
    }

    return "";
}

function findFileInDirectoryOrWorkspace(filename: string, filenameDir: string): string {
    if (!filename) {
        return "";
    }

    if (filenameDir.length !== 0) {
        var foundFile = findFileInDirectory(filename, filenameDir);
        if (foundFile.length !== 0) {
            return foundFile;
        }
    }

    if (workspace.workspaceFolders) {
        for (var folder of workspace.workspaceFolders) {
            let foundFile = findFileInDirectory(filename, folder.uri.fsPath);
            if (foundFile.length !== 0) {
                return foundFile;
            }
        }
    }
    return "";
}

export function expandLogicalCopyBookToFilenameOrEmpty(filename: string): string {
    let fullPath = findFileInDirectoryOrWorkspace(filename, "");
    if (fullPath.length !== 0) {
        return path.normalize(fullPath);
    }

    let lastDot = filename.lastIndexOf(".");
    if (lastDot !== -1) {
        let filenameNoExtension = filename.substr(0, lastDot);
        fullPath = findFileInDirectoryOrWorkspace(filenameNoExtension, "");
        if (fullPath.length !== 0) {
            return path.normalize(fullPath);
        }
    }

    return "";
}

export function provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {

    const line = doc.lineAt(pos);
    const filename = extractCopyBoolFilename(line.text);

    if (filename !== null && filename.length !== 0) {
        const fullPath = expandLogicalCopyBookToFilenameOrEmpty(filename.trim());
        if (fullPath.length !== 0) {
            return new vscode.Location(
                Uri.file(fullPath),
                new Range(new Position(0, 0), new Position(0, 0))
            );
        }
    }

    return;
}
