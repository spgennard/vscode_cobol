/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { COBOLStatUtils, logException, logMessage, logTimedMessage, performance_now } from "./extension";
import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import { ICOBOLSettings } from "./iconfiguration";
import ISourceHandler from "./isourcehandler";
import { getCOBOLSourceFormat } from "./margindecorations";
import { COBOLCopyBookProvider } from "./opencopybook";

export class VSExternalFeatures implements IExternalFeatures{
    public logMessage(message: string): void {
        logMessage(message);
    }

    public logException(message: string, ex: Error): void {
        logException(message, ex);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        return logTimedMessage(timeTaken, message, parameters);
    }

    public performance_now(): number {
        return performance_now();
    }

    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        return COBOLCopyBookProvider.expandLogicalCopyBookToFilenameOrEmpty(filename, inDirectory, config);
    }

    public getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat {
        return getCOBOLSourceFormat(doc,config);
    }

    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        return COBOLStatUtils.getFullWorkspaceFilename(sdir, sdirMs);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public setWorkspaceFolders(_folders: string[]) {
        //
    }
}