/* eslint-disable @typescript-eslint/no-explicit-any */
import { COBOLApi } from "./cobapi";
import { logMessage } from "./extension";

export class CobApi implements COBOLApi {
    logWarningMessage(message: string, ...parameters: any[]): void {
        logMessage("[Extension]: "+message,parameters);
    }
}
