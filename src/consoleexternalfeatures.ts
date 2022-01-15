import util from "util";
import path from "path";
import fs from "fs";

/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { IExternalFeatures } from "./externalfeatures";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLFileUtils } from "./fileutils";

export class ConsoleExternalFeatures implements IExternalFeatures {
    public static readonly Default = new ConsoleExternalFeatures();

    public workspaceFolders: string[] = [];


    public logMessage(message: string): void {
        if (process.send) {
            process.send(message);
        } else {
            // eslint-disable-next-line no-console
            console.log(message);
        }

        return;
    }

    public logException(message: string, ex: Error): void {
        this.logMessage(ex.name + ": " + message);
        if (ex !== undefined && ex.stack !== undefined) {
            this.logMessage(ex.stack);
        } 
        return;
    }

    readonly logTimeThreshold = 500;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        const fixedTimeTaken = " (" + timeTaken.toFixed(2) + "ms)";

        if (timeTaken < this.logTimeThreshold) {
            return false;
        }

        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            const m: string = util.format(message, parameters);
            this.logMessage(m.padEnd(60) + fixedTimeTaken);
        } else {
            this.logMessage(message.padEnd(60) + fixedTimeTaken);
        }

        return true;

    }

    public performance_now(): number {
        return Date.now();
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        return "";
    }

    public setWorkspaceFolders(folders: string[]) {
        this.workspaceFolders = folders;
    }

    public getWorkspaceFolders(): string[] {
        return this.workspaceFolders;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        if (this.workspaceFolders.length === 0) {
            this.logMessage("getFullWorkspaceFilename: workspaceFolders.length === 0");
        }
        for (const folder of this.workspaceFolders) {
            const possibleFile = path.join(folder, sdir);
            if (this.isFile(possibleFile)) {
                const stat4src = fs.statSync(possibleFile, { bigint: true });
                if (sdirMs === stat4src.mtimeMs) {
                    return possibleFile;
                } 
                this.logMessage(`getFullWorkspaceFilename: found ${possibleFile} ${sdirMs} !== ${stat4src.mtimeMs}`);
            }
        }

        return undefined;
    }

    public isDirectory(possibleDirectory: string) : boolean {
        return COBOLFileUtils.isDirectory(possibleDirectory);
    }

    public isFile(possibleFilename:string): boolean {
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

    public getFileModTimeStamp(filename:string):BigInt {
        const f = fs.statSync(filename, {bigint:true});
        return (BigInt)(f.mtimeMs);
    }

    private fileSearchDirectory: string[] = [];
    
    public getCombinedCopyBookSearchPath(): string[] {
        return this.fileSearchDirectory;
    }

    public setCombinedCopyBookSearchPath(fileSearchDirectory: string[]):void {
        this.fileSearchDirectory = fileSearchDirectory;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getSourceTimeout(config: ICOBOLSettings): number {
        return Number.MAX_VALUE;
    }
}
