/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { VSLogger } from "./vslogger";
import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import { ICOBOLSettings } from "./iconfiguration";
import { ISourceHandler } from "./isourcehandler";
import { COBOLCopyBookProvider } from "./opencopybook";
import { getVSCOBOLSourceFormat } from "./sourceformat";
import { VSCOBOLFileUtils } from "./vsfileutils";

import fs from "fs";
import { COBOLFileUtils } from "./fileutils";
import { getVSWorkspaceFolders } from "./cobolfolders";

class VSExternalFeaturesImpl implements IExternalFeatures {
    public logMessage(message: string): void {
        VSLogger.logMessage(message);
    }

    public logException(message: string, ex: Error): void {
        VSLogger.logException(message, ex);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        return VSLogger.logTimedMessage(timeTaken, message, parameters);
    }

    public performance_now(): number {
        return Date.now();
    }

    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        return COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(filename, inDirectory, config, this);
    }

    public getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat {
        return getVSCOBOLSourceFormat(doc, config);
    }

    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        return VSCOBOLFileUtils.getFullWorkspaceFilename(this, sdir, sdirMs);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public setWorkspaceFolders(_folders: string[]) {
        //
    }

    public getWorkspaceFolders(): string[] {
        const folders = getVSWorkspaceFolders();
        if (folders === undefined) {
            return [];
        }
        const foldersArray: string[] = [];
        for (const folder of folders) {
            foldersArray.push(folder.uri.fsPath);
        }

        return foldersArray;
    }

    public isDirectory(possibleDirectory: string): boolean {
        return COBOLFileUtils.isDirectory(possibleDirectory);
    }

    public isFile(possibleFilename: string): boolean {
        try {
            if (fs.existsSync(possibleFilename)) {
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


    private fileSearchDirectory: string[] = [];

    public getFileModTimeStamp(filename: string): BigInt {
        try {
            const f = fs.statSync(filename, { bigint: true });
            return (BigInt)(f.mtimeMs);
        } catch (e) {
            //
        }

        return (BigInt)(0);
    }

    public getCombinedCopyBookSearchPath(): string[] {
        return this.fileSearchDirectory;
    }

    public setCombinedCopyBookSearchPath(fileSearchDirectory: string[]): void {
        this.fileSearchDirectory = fileSearchDirectory;
    }
}

export const VSExternalFeatures = new VSExternalFeaturesImpl();
