import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import ISourceHandler from "./isourcehandler";

import util from 'util';
import { ICOBOLSettings } from "./iconfiguration";

export class ConsoleExternalFeatures implements IExternalFeatures {
    public static readonly Default = new ConsoleExternalFeatures();


    public logMessage(message: string): void {
        if (process.send) {
            process.send(message);
        }
        console.log(message);

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
        return ESourceFormat.unknown;
    }
}
