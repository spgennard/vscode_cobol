'use strict';

import { Range, TextDocument, workspace, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

const DEFAULT_COPYBOOK_EXTS = ["cpy"];
const DEFAULT_COPYBOOK_DIR = ["."];

function getExtensions(): string[] {
    var editorConfig = workspace.getConfiguration('coboleditor');
    editorConfig = editorConfig;
    var extensions = editorConfig.get<string[]>('copybookexts');
    if (!extensions || (extensions !== null && extensions.length === 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    return extensions;
}

function getcopybookdirs(): string[] {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var dirs = editorConfig.get<string[]>('copybookdirs');
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }
    return dirs;
}

function extractText(str: string) {
    let getFirstMatchOrDefault =
        (s: string, pattern: RegExp) => {
            const match = s.match(pattern);
            return match ? match[1] : null;
        };

    if (/"/.test(str)) {
        return getFirstMatchOrDefault(str, /"(.*?)"/);
    }

    if (/'/.test(str)) {
        return getFirstMatchOrDefault(str, /'(.*?)'/);
    }

    const strl = str.toLowerCase();
    if (/copy/.test(strl)) {
        try {
            return getFirstMatchOrDefault(strl, /copy\s(.*)\./);
        } catch (e) {
            /* continue */
        }
        try {
            return getFirstMatchOrDefault(strl, /copy\s(.*)/);
        } catch (e) {
            /* continue */
        }
    }
    return str;
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


        const basefullPath = filenameDir + path.sep + extdir + path.sep + filename;

        //No extension?
        if (filename === fileExtension) {
            // search through the possible extensions
            const exts = getExtensions();
            for (let extpos = 0; extpos < exts.length; extpos++) {
                var ext = exts[extpos];
                var possibleFile = basefullPath + "." + ext;

                if (!fs.existsSync(possibleFile) === false) {
                    fullPath = possibleFile;
                    break;
                }
            }
        } else {
            if (fs.existsSync(basefullPath)) {
                fullPath = basefullPath;
                break;
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
