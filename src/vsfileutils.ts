import path from "path";
import fs from "fs";

import { VSWorkspaceFolders } from "./cobolfolders";
import { Range, TextEditor, Uri, window, workspace, WorkspaceFolder } from "vscode";

import { ICOBOLSettings } from "./iconfiguration";
import { IExternalFeatures } from "./externalfeatures";

export class VSCOBOLFileUtils {

    public static isPathInWorkspace(ddir: string): boolean {
        const ws = VSWorkspaceFolders.get();
        if (workspace === undefined || ws === undefined) {
            return false;
        }

        const fullPath = Uri.file(ddir).fsPath;
        for (const folder of ws) {
            if (folder.uri.fsPath === fullPath) {
                return true;
            }
        }

        return false;
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    public static getFullWorkspaceFilename(features: IExternalFeatures, sdir: string, sdirMs: BigInt): string | undefined {
        const ws = VSWorkspaceFolders.get();
        if (workspace === undefined || ws === undefined) {
            return undefined;
        }
        for (const folder of ws) {
            if (folder.uri.scheme === "file") {
                const folderPath = folder.uri.path;

                const possibleFile = path.join(folderPath, sdir);
                if (features.isFile(possibleFile)) {
                    const stat4srcMs = features.getFileModTimeStamp(possibleFile);
                    if (sdirMs === stat4srcMs) {
                        return possibleFile;
                    }
                }
            }
        }

        return undefined;
    }

    public static getShortWorkspaceFilename(schema: string, ddir: string): string | undefined {
        // if schema is untitle, we treat it as a non-workspace file, so it is never cached
        if (schema === "untitled") {
            return undefined;
        }
        const ws = VSWorkspaceFolders.get();
        if (workspace === undefined || ws === undefined) {
            return undefined;
        }

        const fullPath = Uri.file(ddir).fsPath;
        let bestShortName = "";
        for (const folder of ws) {
            if (folder.uri.scheme === schema) {
                const folderPath = folder.uri.path;
                if (fullPath.startsWith(folderPath)) {
                    const possibleShortPath = fullPath.substring(1 + folderPath.length);
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

    public static findCopyBook(filename: string, config: ICOBOLSettings, features: IExternalFeatures): string {
        if (!filename) {
            return "";
        }

        const hasDot = filename.indexOf(".");

        for (const copybookdir of config.file_search_directory) {

            /* check for the file as is.. */
            const firstPossibleFile = path.join(copybookdir, filename);
            if (features.isFile(firstPossibleFile)) {
                return firstPossibleFile;
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile = path.join(copybookdir, filename + "." + ext);

                    if (features.isFile(possibleFile)) {
                        return possibleFile;
                    }
                }
            }
        }

        return "";
    }

    public static findCopyBookInDirectory(filename: string, inDirectory: string, config: ICOBOLSettings, features: IExternalFeatures): string {
        if (!filename) {
            return "";
        }

        const hasDot = filename.indexOf(".");

        for (const baseCopybookdir of config.file_search_directory) {
            const copybookdir = path.join(baseCopybookdir, inDirectory);

            /* check for the file as is.. */
            const firstPossibleFile = path.join(copybookdir, filename);
            if (features.isFile(firstPossibleFile)) {
                return firstPossibleFile;
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile = path.join(copybookdir, filename + "." + ext);

                    if (features.isFile(possibleFile)) {
                        return possibleFile;
                    }
                }
            }

        }

        return "";
    }

    public static async findCopyBookViaURL(filename: string, config: ICOBOLSettings, features: IExternalFeatures): Promise<string> {
        if (!filename) {
            return "";
        }

        const hasDot = filename.indexOf(".");

        for (const copybookdir of features.getURLCopyBookSearchPath()) {

            /* check for the file as is.. */
            const firstPossibleFile = Uri.parse(copybookdir+"/"+filename);
            if (await features.isFileASync(firstPossibleFile.toString())) {
                return firstPossibleFile.toString();
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile = Uri.parse(copybookdir+"/"+filename + "." + ext);

                    if (await features.isFileASync(possibleFile.toString())) {
                        return possibleFile.toString();
                    }
                }
            }
        }

        return "";
    }

    public static async findCopyBookInDirectoryViaURL(filename: string, inDirectory: string, config: ICOBOLSettings, features: IExternalFeatures): Promise<string> {
        if (!filename) {
            return "";
        }

        const hasDot = filename.indexOf(".");

        for (const baseCopybookdir of features.getURLCopyBookSearchPath()) {
            const copybookdir = baseCopybookdir+"/"+inDirectory;

            /* check for the file as is.. */
            const firstPossibleFile =  Uri.parse(copybookdir+"/"+filename);
            if (await features.isFileASync(firstPossibleFile.toString())) {
                return firstPossibleFile.toString();
            }

            /* no extension? */
            if (hasDot === -1) {
                // search through the possible extensions
                for (const ext of config.copybookexts) {
                    const possibleFile =  Uri.parse(copybookdir+"."+filename + "." + ext);

                    if (await features.isFileASync(possibleFile.toString())) {
                        return possibleFile.toString();
                    }
                }
            }
        }

        return "";
    }

    public static extractSelectionToCopybook(activeTextEditor: TextEditor, features: IExternalFeatures): void {
        const sel = activeTextEditor.selection;

        const ran = new Range(sel.start, sel.end);
        const text = activeTextEditor.document.getText(ran);
        const dir = path.dirname(activeTextEditor.document.fileName);

        window.showInputBox({
            prompt: "Copybook name?",
            validateInput: (copybook_filename: string): string | undefined => {
                if (!copybook_filename || copybook_filename.indexOf(" ") !== -1 ||
                    copybook_filename.indexOf(".") !== -1 ||
                    features.isFile(path.join(dir, copybook_filename + ".cpy"))) {
                    return "Invalid copybook";
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
            fs.writeFileSync(filename, text);
            activeTextEditor.edit(edit => {
                edit.replace(ran, "           copy \"" + copybook_filename + ".cpy\".");
            });

        });
    }

    public static getBestWorkspaceFolder(workspaceDirectory: string): WorkspaceFolder | undefined {
        const workspaces = workspace.workspaceFolders;
        if (!workspaces) {
            return undefined;
        }

        let bestWorkspace: WorkspaceFolder | undefined = undefined;
        for (const workspace of workspaces) {
            if (workspace.uri.fsPath.startsWith(workspaceDirectory)) {
                bestWorkspace = workspace;
            }
        }
        return bestWorkspace;
    }
}

