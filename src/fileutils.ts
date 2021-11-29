import * as fs from "fs";
import { ICOBOLSettings } from "./iconfiguration";

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
            extension = filename.substr(1 + lastDot);
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
            extension = filename.substr(1 + lastDot);
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
    public static isNetworkPath(dir: string): boolean {
        if (dir === undefined && dir === null) {
            return false;
        }

        if (COBOLFileUtils.isWin32) {
            if (dir.length > 1 && dir[0] === "\\") {
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
}
