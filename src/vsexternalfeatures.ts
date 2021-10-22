/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { FileType, Uri, workspace } from "vscode";
import { VSLogger } from "./vslogger";
import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import { ICOBOLSettings } from "./iconfiguration";
import ISourceHandler from "./isourcehandler";
import { COBOLCopyBookProvider } from "./opencopybook";
import { getVSCOBOLSourceFormat } from "./sourceformat";
import { VSExtensionUtils } from "./extension";
import { VSCOBOLFileUtils } from "./vsfileutils";

export class VSExternalFeatures implements IExternalFeatures {
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
        return VSExtensionUtils.performance_now();
    }

    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        return COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(filename, inDirectory, config);
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
        try {
            async () => {
                const stat = await workspace.fs.stat(Uri.file(possibleDirectory));
                return stat.type === FileType.Directory;
            }
        } catch {
            return false;
        }

        return false;
    }
}