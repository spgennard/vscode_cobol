/* eslint-disable @typescript-eslint/ban-types */
import { parentPort, workerData } from "worker_threads";
import { Scanner, workerThreadData } from "./cobscanner";
import { ScanStats } from "./cobscannerdata";
import { IExternalFeatures } from "./externalfeatures";

import * as fs from "fs";
import * as path from "path";
import util from "util";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLFileUtils } from "./fileutils";
import { ICOBOLSourceScannerEventer } from "./icobolsourcescanner";
import { ISourceHandler } from "./isourcehandler";

export class ThreadConsoleExternalFeatures implements IExternalFeatures {
    public static readonly Default = new ThreadConsoleExternalFeatures();

    public workspaceFolders: string[] = [];

    public logMessage(message: string): void {
        if (parentPort !== null) {
            parentPort.postMessage(message);
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
    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, source: ISourceHandler, config: ICOBOLSettings): string {
        return COBOLFileUtils.expandLogicalCopyBookOrEmpty(filename, inDirectory, config, source.getFilename(), this);
    }

    public setWorkspaceFolders(folders: string[]): void {
        this.workspaceFolders = folders;
    }

    public getWorkspaceFolders(settings: ICOBOLSettings): string[] {
        return this.workspaceFolders;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        for (const folder of this.workspaceFolders) {
            const possibleFile = path.join(folder, sdir);
            if (this.isFile(possibleFile)) {
                const stat4src = fs.statSync(possibleFile, { bigint: true });
                if (sdirMs === stat4src.mtimeMs) {
                    return possibleFile;
                }
            }
        }

        return undefined;
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

    public isDirectory(possibleDirectory: string): boolean {
        return COBOLFileUtils.isDirectory(possibleDirectory);
    }

    public getFileModTimeStamp(filename: string): BigInt|undefined {
        try {
            const possibleFileMod = fs.statSync(filename, { bigint: true, throwIfNoEntry: false });
            if (possibleFileMod === undefined) {
                return undefined;
            }
            return (BigInt)(possibleFileMod.mtimeMs);
        } catch {
            return undefined;
        }
    }

    private fileSearchDirectory: string[] = [];

    public getCombinedCopyBookSearchPath(): string[] {
        return this.fileSearchDirectory;
    }

    public setCombinedCopyBookSearchPath(fileSearchDirectory: string[]): void {
        this.fileSearchDirectory = fileSearchDirectory;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getSourceTimeout(config: ICOBOLSettings): number {
        return Number.MAX_VALUE;
    }

    private URLSearchDirectory: string[] = [];

    public getURLCopyBookSearchPath(): string[] {
        return this.URLSearchDirectory;
    }

    public setURLCopyBookSearchPath(fileSearchDirectory: string[]): void {
        this.URLSearchDirectory = fileSearchDirectory;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async isFileASync(possibleFilename: string): Promise<boolean> {
        return false;
    }

    public isDebuggerActive(): boolean {
        return false;
    }

}

class threadSender implements ICOBOLSourceScannerEventer {
    public static readonly Default = new threadSender();

    sendMessage(message: string): void {
        if (parentPort !== null) {
            parentPort.postMessage(message);
        }
    }
}

if (parentPort !== null) {
    try {
        const features = ThreadConsoleExternalFeatures.Default;
        const wd: workerThreadData = workerData as workerThreadData;
        const scanData = wd.scanData;
        scanData.showStats = false;
        const sd = new ScanStats();
        Scanner.transferScanDataToGlobals(scanData, features);
        Scanner.processFiles(scanData, features, threadSender.Default, sd);
        parentPort.postMessage(`++${JSON.stringify(sd)}`);
    } catch (e) {
        threadSender.Default.sendMessage((e as Error).message);
    }
}