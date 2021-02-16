/* eslint-disable @typescript-eslint/no-explicit-any */
import { COBOLApi } from "./api";
import { logMessage } from "./extension";

export class Api implements COBOLApi {
    logWarningMessage(message: string, ...parameters: any[]): void {
        logMessage("[Extension]: "+message,parameters);
    }
}
