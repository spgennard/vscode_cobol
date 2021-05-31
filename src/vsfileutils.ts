import path from "path";
import fs from "fs";
import { getWorkspaceFolders } from "./cobolfolders";
import { COBOLFileUtils } from "./fileutils";
import { workspace } from "vscode";
import { getCombinedCopyBookSearchPath } from "./extension";
import { ICOBOLSettings } from "./iconfiguration";

export class VSCOBOLFileUtils {

    public static isPathInWorkspace(ddir: string): boolean {
        const ws = getWorkspaceFolders();
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
        const ws = getWorkspaceFolders();
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
        const ws = getWorkspaceFolders();
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

        for (const copybookdir of getCombinedCopyBookSearchPath()) {

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

        for (const baseCopybookdir of getCombinedCopyBookSearchPath()) {
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
}