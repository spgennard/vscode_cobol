'use strict';

import { Range, TextDocument, window, workspace, Definition, Position, CancellationToken, ProviderResult, Uri } from 'vscode';
import * as fs from 'fs';

const DEFAULT_COPYBOOK_EXTS = ["cpy"];
const DEFAULT_COPYBOOK_DIR = ["."];

function getExtensions(): string[] {
    var editorConfig =  workspace.getConfiguration('coboleditor');
    editorConfig = editorConfig;
    var extensions = editorConfig.get<string[]>('copybookexts');
    if (!extensions || (extensions != null && extensions.length == 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    return extensions;
}

function getcopybookdirs(): string[] {
    var editorConfig =  workspace.getConfiguration('coboleditor');
    var dirs = editorConfig.get<string[]>('copybookdirs');
    if (!dirs || (dirs != null && dirs.length == 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }
    return dirs;
}

function extractText(str: string) {
    let getFirstMatchOrDefault =
        (s: string, pattern: RegExp) => {
            const match = s.match(pattern)
            return match ? match[1] : null;
        }

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
        } catch(e) {
            /* continue */
        }
        try {
            return getFirstMatchOrDefault(strl, /copy\s(.*)/);
        } catch(e) {
            /* continue */
        }
    }
    return str;
}

function findFile(filename: string): string | undefined {
    if (!filename)
        return;

    var fileExtension = filename.split('.').pop();
    var fullPath = workspace.rootPath + '/' + filename ;

    var extsdir = getcopybookdirs();
    for (let extsdirpos = 0; extsdirpos < extsdir.length; extsdirpos++) {
        var extdir = extsdir[extsdirpos];

        const basefullPath = workspace.rootPath + "/" + extdir + '/' + filename ;

        //No extension?
        if (filename == fileExtension) {
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
            if (!fs.existsSync(basefullPath) === false) {
                fullPath = basefullPath;
            }
        }
    }

    if (fs.existsSync(fullPath) === false) {
        return;
    }

    return fullPath;
}

export function provideDefinition(doc: TextDocument, pos: Position, ct: CancellationToken): ProviderResult<Definition> {
    const line = doc.lineAt(pos);
    const filename = extractText(line.text);
    if (filename) {
        const fullPath = findFile(filename);
        if (fullPath) {
            return {
                uri: Uri.file(fullPath),
                range: new Range(new Position(0, 0), new Position(0, 0))
            }
        } else {
            window.showWarningMessage(`Unable to locate : ${filename}`);
        }
    }
}
