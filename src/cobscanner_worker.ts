import { parentPort, workerData } from 'worker_threads';
import { Scanner, workerThreadData } from './cobscanner';
import { ScanData, ScanDataHelper, ScanStats } from './cobscannerdata';
import { ESourceFormat, IExternalFeatures } from './externalfeatures';

import * as fs from 'fs';
import * as path from 'path';
import util from 'util';
import ISourceHandler from './isourcehandler';
import { ICOBOLSettings } from './iconfiguration';
import { getCOBOLSourceFormat } from './consoleexternalfeatures';
import { ICOBOLSourceScannerEventer } from './cobolsourcescanner';

// class WorkerUtils {
//     public static msleep(n: number) {
//         Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, n);
//     }

//     public static sleep(n: number) {
//         WorkerUtils.msleep(n * 1000);
//     }
// }

export class ThreadConsoleExternalFeatures implements IExternalFeatures {
    public static readonly Default = new ThreadConsoleExternalFeatures();

    public workspaceFolders: string[] = [];

    private static isFile(sdir: string): boolean {
        try {
            if (fs.existsSync(sdir)) {
                return true;
            }
        }
        catch {
            return false;
        }
        return false;
    }

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
    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        return "";
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat {
        return getCOBOLSourceFormat(doc,config,false);
    }

    public setWorkspaceFolders(folders: string[]):void {
        this.workspaceFolders = folders;
    }

    public getWorkspaceFolders(): string[] {
        return this.workspaceFolders;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        for (const folder of this.workspaceFolders) {
            const possibleFile = path.join(folder, sdir);
            if (ThreadConsoleExternalFeatures.isFile(possibleFile)) {
                const stat4src = fs.statSync(possibleFile, { bigint: true });
                if (sdirMs === stat4src.mtimeMs) {
                    return possibleFile;
                }
            }
        }

        return undefined;
    }
}

class threadSender implements ICOBOLSourceScannerEventer {
    public static Default = new threadSender();

    sendMessage(message: string): void {
        if (parentPort !== null) {
            parentPort.postMessage(message);
        }
    }
}

if (parentPort !== null) {
    try {
        const wd: workerThreadData = workerData as workerThreadData;
        const scanData: ScanData = ScanDataHelper.parseScanData(wd.scanDataString);
        const sd = new ScanStats();
        scanData.showStats = false;
        // scanData.cache_metadata_show_progress_messages = true;
        Scanner.processFiles(scanData,ThreadConsoleExternalFeatures.Default, threadSender.Default, sd);
        parentPort.postMessage(`++${JSON.stringify(sd)}`);
    } catch (e) {
        threadSender.Default.sendMessage(e.message);
    }
}