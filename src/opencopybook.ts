'use strict';

import { Range, TextDocument, workspace, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as process from 'process';

const DEFAULT_COPYBOOK_EXTS = ["cpy"];
const DEFAULT_COPYBOOK_DIR = ["."];

export function getExtensions(): string[] {
    var editorConfig = workspace.getConfiguration('coboleditor');
    editorConfig = editorConfig;
    var extensions = editorConfig.get<string[]>('copybookexts');
    if (!extensions || (extensions !== null && extensions.length === 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    return extensions;
}

function getcopybookdirs(): string[] {
    let editorConfig = workspace.getConfiguration('coboleditor');
    let dirs = editorConfig.get<string[]>('copybookdirs');
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }

    let extraDirs: string[] = [];

    for (let dirpos = 0; dirpos < dirs.length; dirpos++) {
        let dir = dirs[dirpos];

        if (dir.startsWith("$")) {
            var e = process.env[dir.substr(1)];
            if (e !== undefined && e !== null) {
                e.split(path.delimiter).forEach(function (item) {
                    if (item !== undefined && item !== null && item.length > 0) {
                        extraDirs.push(item);
                    }
                });
            }
        }
    }

    for (let dirpos = 0; dirpos < dirs.length; dirpos++) {
        let dir = dirs[dirpos];
        if (!dir.startsWith("$")) {
            extraDirs.push(dir);
        }
    }

    return extraDirs;
}

function extractText(str: string) {
    let getFirstMatchOrDefault =
        (s: string, pattern: RegExp) => {
            const match = s.match(pattern);
            return match ? match[1] : null;
        };

    const strl = str.toLowerCase();
    let result: string | null;
    if (/copy/.test(strl)) {

        let copyRegs: RegExp[] = [
            new RegExp(".*copy\\s*[\"'](.*)[\"'].*$","i"),
            new RegExp(".*copy\\s*[\"'](.*)[\"']$","i"),
            new RegExp(".*copy\\s*(.*).*$","i"),
            new RegExp(".*copy\\s*(.*)$","i"),
        ];

        for (let regPos = 0; regPos < copyRegs.length; regPos++) {
            try {
                result = getFirstMatchOrDefault(str, copyRegs[regPos]);
                if (result !== null && result.length > 0) {
                    return result;
                }
            } catch (e) {
                /* continue */
                console.log(e);
                console.log(e.stacktrace);
            }
        }

    }

    if (/exec sql include/.test(strl)) {
        try {
            return getFirstMatchOrDefault(strl, /exec sql include\s(.*)\send-exec/);
        } catch (e) {
            /* continue */
        }
    }
    return str;
}

function isDirectPath(dir: string) {
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

function findFileInDirectory(filename: string, filenameDir: string): string | undefined {
    if (!filename) {
        return;
    }

    var fileExtension = filename.split('.').pop();
    var fullPath = filenameDir + path.sep + filename;
    if (fs.existsSync(fullPath)) {
        return fullPath;
    }
    
    var extsdir = getcopybookdirs();
    for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
        var extdir = extsdir[extsdirpos];

        const basefullPath = isDirectPath(extdir) ?
            extdir + path.sep + filename :
            filenameDir + path.sep + extdir + path.sep + filename;

        //No extension?
        if (filename === fileExtension) {
            // search through the possible extensions
            const exts = getExtensions();
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

    if (fs.existsSync(fullPath) === false) {
        return;
    }

    return fullPath;
}

function findFile(filename: string, filenameDir: string): string | undefined {
    if (!filename) {
        return;
    }

    var foundFile = findFileInDirectory(filename, filenameDir);
    if (foundFile) {
        return foundFile;
    }

    if (workspace.workspaceFolders) {
        for (var folder of workspace.workspaceFolders) {
            let foundFile = findFileInDirectory(filename, folder.uri.fsPath);
            if (foundFile !== null) {
                return foundFile;
            }
        }
    }
    return;
}

export function expandLogicalCopyBookToFilenameOrEmpty(filename: string): string {
    const dirOfFilename = path.dirname(filename);

    if (filename) {
        const fullPath = findFile(filename, dirOfFilename);
        if (fullPath) {
            return path.normalize(fullPath);
        }
    }

    return "";
}

export function provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {

    const dirOfFilename = path.dirname(doc.fileName);
    const line = doc.lineAt(pos);
    const filename = extractText(line.text);

    // No way this could be a filename...
    if (filename && filename.includes(" ")) {
        return;
    }
    if (filename) {
        const fullPath = findFile(filename, dirOfFilename);
        if (fullPath) {
            return {
                uri: Uri.file(fullPath),
                range: new Range(new Position(0, 0), new Position(0, 0))
            };
        }
    }

    return;
}
