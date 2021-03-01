/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { extensions } from "vscode";
import { COBOLPreprocessor } from "./cobapi";
import { CobApiHandle } from "./cobapiimpl";
import { logException, logMessage, logTimedMessage, performance_now } from "./extension";
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

    public getCOBOLPreprocessor(packageJSON:any): COBOLPreprocessor|undefined {
        const handle = new CobApiHandle(packageJSON);

        const preprocexp = extensions.getExtension(handle.id);
        if (preprocexp?.exports === undefined) {
            const messageForInfo = `Interface version requested is out of date, please contact ${handle.bugReportEmail} @ ${handle.bugReportUrl}\n` +
                          `for an updated of extension ${handle.id}\n` +
                          "No export found";
            throw new Error(messageForInfo);
        }

        return preprocexp.exports as COBOLPreprocessor;
    }
}