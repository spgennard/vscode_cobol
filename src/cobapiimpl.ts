/* eslint-disable @typescript-eslint/no-explicit-any */
import { COBOLApi, COBOLPreprocessor, COBOLPreprocessorHandle } from "./cobapi";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { logMessage } from "./extension";

export class CobApiHandle implements COBOLPreprocessorHandle {
    id: string;
    callback: COBOLPreprocessor;

    constructor(id: string, callback:COBOLPreprocessor) {
        this.id = id;
        this.callback = callback;
    }
}

export class CobApi implements COBOLApi {
    registerPreprocessor(ownerid:string, callback: COBOLPreprocessor): COBOLPreprocessorHandle {
        COBOLPreprocessorHelper.ownerId.push(ownerid);
        COBOLPreprocessorHelper.sourceScanner.push(callback);
        const handle = new CobApiHandle(ownerid,callback);
        this.logWarningMessage(handle, "is active");
        return handle;
    }

    logWarningMessage(handle: COBOLPreprocessorHandle, message: string): void {
        logMessage(`[${handle.id}]: ${message}`);
    }
}
