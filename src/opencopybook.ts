'use strict';

import { Range, TextDocument, workspace, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as process from 'process';
import { logCOBOLChannelLine } from './extension';

const DEFAULT_COPYBOOK_EXTS = ["cpy"];
const DEFAULT_COPYBOOK_DIR = ["."];

export function getExtensions(): string[] {
    var editorConfig = workspace.getConfiguration('coboleditor');
    editorConfig = editorConfig;
    var extensions = editorConfig.get<string[]>('copybookexts');
    if (!extensions || (extensions !== null && extensions.length === 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    extensions.push("");
    return extensions;
}

export function isValidExtension(filename: string): boolean {
    switch (filename) {
        case "tags":
        case ".tag":
        case ".ctags":
            return false;
    }
    const exts = getExtensions();
    for (let extpos = 0; extpos < exts.length; extpos++) {
        let ext = exts[extpos];
        if (ext.length !== 0) {
            if (filename.endsWith(ext)) {
                return true;
            }
        }
    }
    return false;
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

function extractCopyBoolFilename(str: string) {
    let getFirstMatchOrDefault =
        (s: string, pattern: RegExp) => {
            const match = s.match(pattern);
            return match ? match[1] : null;
        };

    const strl = str.toLowerCase();
    let result: string | null;
    if (/copy/.test(strl)) {

        let copyRegs: RegExp[] = [
            new RegExp(".*copy\\s*[\"'](.*)[\"'].*$", "i"),
            new RegExp(".*copy\\s*[\"'](.*)[\"']$", "i"),
            new RegExp(".*copy\\s*(.*).*$", "i"),
            new RegExp(".*copy\\s*(.*)$", "i"),
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
    return "";
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

function findFileInDirectory(filename: string, filenameDir: string): string {
    if (!filename) {
        return "";
    }

    if (filenameDir === '.') {
        filenameDir = "";
    }

    var fileExtension = filename.split('.').pop();
    var fullPath = path.join(filenameDir,filename);
    if (fs.existsSync(fullPath)) {
        return fullPath;
    }

    var extsdir = getcopybookdirs();
    for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
        var extdir = extsdir[extsdirpos];

        const basefullPath = isDirectPath(extdir) ?
            path.join(extdir, filename) :
            path.join(filenameDir, extdir + path.sep + filename);

        //No extension?
        if (filename === fileExtension) {
            // search through the possible extensions
            const exts = getExtensions();
            for (let extpos = 0; extpos < exts.length; extpos++) {
                var ext = exts[extpos];
                var possibleFile = basefullPath + (ext.length !== 0 ? "." + ext : "");

                // logCOBOLChannelLine(" - Search : " + possibleFile);
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
        return "";
    }

    return fullPath;
}

function findFile(filename: string, filenameDir: string): string {
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
    const fullPath = findFile(filename, "");
    if (fullPath.length !== 0) {
        return path.normalize(fullPath);
    }

    return "";
}

export function provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {

    const dirOfFilename = path.dirname(doc.fileName);
    const line = doc.lineAt(pos);
    const filename = extractCopyBoolFilename(line.text);

    if (filename !== null && filename.length !== 0) {
        const fullPath = findFile(filename.trim(), dirOfFilename);
        if (fullPath.length !== 0) {
            return {
                uri: Uri.file(fullPath),
                range: new Range(new Position(0, 0), new Position(0, 0))
            };
        }
    }

    return;
}
