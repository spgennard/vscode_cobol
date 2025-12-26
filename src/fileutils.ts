import * as fs from "fs";
import { ICOBOLSettings } from "./iconfiguration";
import path from "path";
import { IExternalFeatures } from "./externalfeatures";

export class COBOLFileUtils {
    static readonly isWin32 = process.platform === "win32";

    public static isFile(sdir: string): boolean {
        try {
            if (fs.existsSync(sdir)) {
                // not on windows, do extra check for +x perms (protects exe & dirs)
                // if (!COBOLFileUtils.isWin32) {
                //     try {
                //         fs.accessSync(sdir, fs.constants.F_OK | fs.constants.X_OK);
                //         return false;
                //     }
                //     catch {
                //         return true;
                //     }
                // }

                return true;
            }
        }
        catch {
            return false;
        }
        return false;
    }

    public static isValidCopybookExtension(filename: string, settings: ICOBOLSettings): boolean {
        const lastDot = filename.lastIndexOf(".");
        let extension = filename;
        if (lastDot !== -1) {
            extension = filename.substring(1 + lastDot);
        }

        const exts = settings.copybookexts;
        for (let extpos = 0; extpos < exts.length; extpos++) {
            if (exts[extpos] === extension) {
                return true;
            }
        }
        return false;
    }

    public static isValidProgramExtension(filename: string, settings: ICOBOLSettings): boolean {
        const lastDot = filename.lastIndexOf(".");
        let extension = "";
        if (lastDot !== -1) {
            extension = filename.substring(1 + lastDot);
        }

        const exts = settings.program_extensions;
        for (let extpos = 0; extpos < exts.length; extpos++) {
            if (exts[extpos] === extension) {
                return true;
            }
        }
        return false;
    }

    public static isDirectPath(dir: string): boolean {
        if (dir === undefined && dir === null) {
            return false;
        }

        if (COBOLFileUtils.isWin32) {
            if (dir.length > 2 && dir[1] === ":") {
                return true;
            }

            if (dir.length > 1 && dir[0] === "\\") {
                return true;
            }

            return false;
        }

        if (dir.length > 1 && dir[0] === "/") {
            return true;
        }

        return false;
    }

    // only handle unc filenames
    public static isNetworkPath(dir: string, settings: ICOBOLSettings): boolean {
        if (dir === undefined && dir === null) {
            return false;
        }

        if (COBOLFileUtils.isWin32) {
            if (dir.length > 1 && dir[0] === "\\") {
                return true;
            }
        }

        for (const prefix of settings.network_directory_prefixes) {
            if (dir.startsWith(prefix)) {
                return true;
            }
        }   

        return false;
    }

    public static isDirectory(sdir: string): boolean {
        try {
            const f = fs.statSync(sdir, { bigint: true });
            if (f && f.isDirectory()) {
                return true;
            }
        }
        catch {
            return false;
        }
        return false;
    }

    public static cleanupFilename(filename: string): string {
        let trimmedFilename = filename.trim();

        if (trimmedFilename.startsWith("\"") && trimmedFilename.endsWith("\"")) {
            return trimmedFilename.substring(1, trimmedFilename.length-1);
        }

        if (trimmedFilename.startsWith("\'") && trimmedFilename.endsWith("\'")) {
            return trimmedFilename.substring(1, trimmedFilename.length-1);
        }
        
        return trimmedFilename;
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

        // check for directory aliases
        if (config.copybook_directory_aliases !== undefined && config.copybook_directory_aliases !== null && inDirectory.length > 0) {
            const aliasTarget = config.copybook_directory_aliases[inDirectory];
            if (aliasTarget !== undefined) {
                const newInDirectory = aliasTarget;
                const possibleResult = this._findCopyBookInDirectory(filename, newInDirectory, config, features);
                if (possibleResult.length !== 0) {
                    return possibleResult;
                }
            }
        }

        return this._findCopyBookInDirectory(filename, inDirectory, config, features);
    }
    
    private static _findCopyBookInDirectory(filename: string, inDirectory: string, config: ICOBOLSettings, features: IExternalFeatures): string {
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
    
    public static expandLogicalCopyBookOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings, sourceFilename: string, features: IExternalFeatures): string {

        if (config.perfile_copybookdirs.length !== 0) {
            // fileDirname
            var fileDirname = path.dirname(sourceFilename);
            for (var _perCopydir of config.perfile_copybookdirs) {
                var perFileDir = _perCopydir.replace("${fileDirname}", fileDirname);
                /* check for the file as is.. */
                const firstPossibleFile = path.join(perFileDir, filename);
                if (features.isFile(firstPossibleFile)) {
                    return firstPossibleFile;
                }

                const hasDot = filename.indexOf(".");
                /* no extension? */
                if (hasDot === -1) {
                    // search through the possible extensions
                    for (const ext of config.copybookexts) {
                        const possibleFile = path.join(perFileDir, filename + "." + ext);

                        if (features.isFile(possibleFile)) {
                            return possibleFile;
                        }
                    }
                }
            }
        }

        if (inDirectory === null || inDirectory.length === 0) {
            const fullPath = COBOLFileUtils.findCopyBook(filename, config, features);
            if (fullPath.length !== 0) {
                return path.normalize(fullPath);
            }

            return fullPath;
        }

        const fullPath = COBOLFileUtils.findCopyBookInDirectory(filename, inDirectory, config, features);
        if (fullPath.length !== 0) {
            return path.normalize(fullPath);
        }

        return fullPath;
    }
}
