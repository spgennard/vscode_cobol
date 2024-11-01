/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { VSLogger } from "./vslogger";
import { IExternalFeatures } from "./externalfeatures";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLCopyBookProvider } from "./opencopybook";
import { VSCOBOLFileUtils } from "./vsfileutils";

import fs from "fs";
import { COBOLFileUtils } from "./fileutils";
import { VSWorkspaceFolders } from "./cobolfolders";
import { FileType, Uri, workspace } from "vscode";
import { IVSCOBOLSettings } from "./vsconfiguration";

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

    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: IVSCOBOLSettings): string {
        return COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(filename, inDirectory, config, this);
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        return VSCOBOLFileUtils.getFullWorkspaceFilename(this, sdir, sdirMs);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public setWorkspaceFolders(_folders: string[]) {
        //
    }

    public getWorkspaceFolders(): string[] {
        const folders = VSWorkspaceFolders.get();
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


    // eslint-disable-next-line @typescript-eslint/ban-types
    public getFileModTimeStamp(filename: string): BigInt {
        try {
            const f = fs.statSync(filename, { bigint: true });
            return (BigInt)(f.mtimeMs);
        } catch (e) {
            //
        }

        return (BigInt)(0);
    }

    public getSourceTimeout(config: ICOBOLSettings): number {
        return config.scan_time_limit;
    }

    private URLSearchDirectory: string[] = [];

    public getURLCopyBookSearchPath(): string[] {
        return this.URLSearchDirectory;
    }

    public setURLCopyBookSearchPath(fileSearchDirectory: string[]): void {
        this.URLSearchDirectory = fileSearchDirectory;
    }

    public async isFileASync(possibleFilename: string): Promise<boolean> {
        try {
            const u = Uri.parse(possibleFilename);
            const sf = await workspace.fs.stat(u);
            switch (sf.type) {
                case FileType.File: return true;
                case FileType.File | FileType.SymbolicLink: return true;
            }

            return false;
        } catch (e) {
            return false;
        }
    }

    public isDebuggerActive(): boolean {
        // if (debug.activeDebugSession === undefined) {
        //     return false;
        // }

        return true;
    }

}

export const VSExternalFeatures = new VSExternalFeaturesImpl();
