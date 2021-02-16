/* eslint-disable @typescript-eslint/no-explicit-any */
import { COBOLApi, COBOLPreprocessor } from "./cobapi";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { logMessage } from "./extension";

export class CobApi implements COBOLApi {
    registerPreprocessor(callback: COBOLPreprocessor): boolean {
        COBOLPreprocessorHelper.sourceScanner.push(callback);
        return false;
    }

    logWarningMessage(message: string): void {
        logMessage(`[Extension]: ${message}]`);
    }
}
