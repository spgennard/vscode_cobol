/* eslint-disable @typescript-eslint/no-explicit-any */
import { COBOLApi, COBOLPreprocessor, COBOLPreprocessorHandle, COBOLPreprocessorOutput } from "./cobapi";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { IExternalFeatures } from "./externalfeatures";
import { clearCOBOLCache } from "./vscobolscanner";

export class CobApiHandle implements COBOLPreprocessorHandle {
    id: string;
    callback: COBOLPreprocessor;

    constructor(id: string, callback:COBOLPreprocessor) {
        this.id = id;
        this.callback = callback;
    }
}

export class CobApiOutput implements COBOLPreprocessorOutput {
    public lines: string[] = [];
    public externalFiles = new Map<string,string>();

    addLine(line: string): void {
        this.lines.push(line);
    }

    addLines(lines: string[]): void {
        for (const line of lines) {
            this.lines.push(line);
        }
    }

    addFileSymbol(symbol:string, copybookName: string):void {
        this.externalFiles.set(symbol,copybookName);
    }
}

export class CobApi implements COBOLApi {
    private externalFeatures: IExternalFeatures;

    constructor(externalFeatures: IExternalFeatures) {
        this.externalFeatures = externalFeatures;
    }

    registerPreprocessor(ownerid:string, callback: COBOLPreprocessor): COBOLPreprocessorHandle {
        COBOLPreprocessorHelper.ownerId.push(ownerid);
        COBOLPreprocessorHelper.sourceScanner.push(callback);
        return new CobApiHandle(ownerid,callback);
    }

    logWarningMessage(handle: COBOLPreprocessorHandle, message: string): void {
        this.externalFeatures.logMessage(`[${handle.id}]: ${message}`);
    }
}
