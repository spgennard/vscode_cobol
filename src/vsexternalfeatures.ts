/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { VSLogger } from "./vslogger";
import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import { ICOBOLSettings } from "./iconfiguration";
import ISourceHandler from "./isourcehandler";
import { COBOLCopyBookProvider } from "./opencopybook";
import { getVSCOBOLSourceFormat } from "./sourceformat";
import { VSCOBOLFileUtils } from "./vsfileutils";

import fs from 'fs';
import { COBOLFileUtils } from "./fileutils";

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
        return COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(filename, inDirectory, config, this);
    }

    public getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat {
        return getVSCOBOLSourceFormat(doc, config);
    }

    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        return VSCOBOLFileUtils.getFullWorkspaceFilename(sdir, sdirMs);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public setWorkspaceFolders(_folders: string[]) {
        //
    }

    public getWorkspaceFolders(): string[] {
        return [];
    }

    public isDirectory(possibleDirectory: string): boolean {
        return COBOLFileUtils.isDirectory(possibleDirectory);
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
