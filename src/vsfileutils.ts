import path from "path";
import fs from "fs";
import { getVSWorkspaceFolders } from "./cobolfolders";
import { COBOLFileUtils } from "./fileutils";
import { Range, TextEditor, Uri, window, workspace } from "vscode";

import { ICOBOLSettings } from "./iconfiguration";
import { VSExtensionUtils } from "./extension";

export class VSCOBOLFileUtils {

    public static isPathInWorkspace(ddir: string): boolean {
        const ws = getVSWorkspaceFolders();
        if (workspace === undefined || ws === undefined) {
            return false;
        }

        const fullPath = path.normalize(ddir);
        for (const folder of ws) {
            if (folder.uri.fsPath === fullPath) {
                return true;
            }
        }

        return false;
    }

    public static getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        const ws = getVSWorkspaceFolders();
        if (workspace === undefined || ws === undefined) {
            return undefined;
        }
        for (const folder of ws) {
            if (folder.uri.scheme === 'file') {
                const folderPath = folder.uri.path;
                const possibleFile = path.join(folderPath, sdir);
                if (COBOLFileUtils.isFile(possibleFile)) {
                    const stat4src = fs.statSync(possibleFile, { bigint: true });
                    if (sdirMs === stat4src.mtimeMs) {
                        return possibleFile;
                    }
                }
            }
        }

        return undefined;
    }

    public static getShortWorkspaceFilename(ddir: string): string | undefined {
        const ws = getVSWorkspaceFolders();
        if (workspace === undefined || ws === undefined) {
            return undefined;
        }

        const fullPath = path.normalize(ddir);
        let bestShortName = "";
        for (const folder of ws) {
            if (folder.uri.scheme === 'file') {
                const folderPath = folder.uri.path;
                if (fullPath.startsWith(folderPath)) {
                    const possibleShortPath = fullPath.substr(1 + folderPath.length);
                    if (bestShortName.length === 0) {
                        bestShortName = possibleShortPath;
                    } else {
                        if (possibleShortPath.length < possibleShortPath.length) {
                            bestShortName = possibleShortPath;
                        }
                    }
                }
            }
        }

        return bestShortName.length === 0 ? undefined : bestShortName;
    }

    public static findCopyBook(filename: string, config: ICOBOLSettings): string {
        if (!filename) {
            return "";
        }

        const hasDot = filename.indexOf(".");

        for (const copybookdir of VSExtensionUtils.getCombinedCopyBookSearchPath()) {

            /* check for the file as is.. */
            const firstPossibleFile = path.join(copybookdir, filename);
            if (COBOLFileUtils.isFile(firstPossibleFile)) {
                return firstPossibleFile;
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile = path.join(copybookdir, filename + "." + ext);

                    if (COBOLFileUtils.isFile(possibleFile)) {
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

        for (const baseCopybookdir of VSExtensionUtils.getCombinedCopyBookSearchPath()) {
            const copybookdir = path.join(baseCopybookdir, inDirectory);

            /* check for the file as is.. */
            const firstPossibleFile = path.join(copybookdir, filename);
            if (COBOLFileUtils.isFile(firstPossibleFile)) {
                return firstPossibleFile;
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile = path.join(copybookdir, filename + "." + ext);

                    if (COBOLFileUtils.isFile(possibleFile)) {
                        return possibleFile;
                    }
                }
            }

        }

        return "";
    }

    public static extractSelectionToCopybook(activeTextEditor: TextEditor): void {
        const sel = activeTextEditor.selection;

        const ran = new Range(sel.start, sel.end);
        const text = activeTextEditor.document.getText(ran);
        const dir = path.dirname(activeTextEditor.document.fileName);

        window.showInputBox({
            prompt: 'Copybook name?',
            validateInput: (copybook_filename: string): string | undefined => {
                if (!copybook_filename || copybook_filename.indexOf(' ') !== -1 ||
                    copybook_filename.indexOf(".") !== -1 ||
                    COBOLFileUtils.isFile(path.join(dir, copybook_filename + ".cpy"))) {
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
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            workspace.fs.writeFile(Uri.file(filename),Buffer.from(text)).then(onFullFilled => {
                activeTextEditor.edit(edit => {
                    edit.replace(ran, "           copy \"" + copybook_filename + ".cpy\".");
                });
            });

        });
    }

}