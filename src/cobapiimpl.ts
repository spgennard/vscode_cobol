/* eslint-disable @typescript-eslint/no-explicit-any */
import { COBOLApi, COBOLPreprocessor, COBOLPreprocessorHandle, COBOLPreprocessorOutput } from "./cobapi";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { IExternalFeatures } from "./externalfeatures";

export class CobApiHandle implements COBOLPreprocessorHandle {
    id: string;
    description: string;
    packageJson: any;
    bugReportUrl: string;
    bugReportEmail: string;
    callback: COBOLPreprocessor;
    info: string;

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    constructor(packageJson: any, callback:COBOLPreprocessor) {
        this.packageJson = packageJson;
        this.callback = callback;
        if (packageJson.name !== undefined && packageJson.publisher !== undefined) {
            this.id = `${packageJson.publisher}.${packageJson.name}`;
        } else {
            this.id = "";
        }

        if (packageJson.description !== undefined) {
            this.description = `${packageJson.description}`;
        } else {
            this.description = "";
        }

        if (packageJson.bugs !== undefined && packageJson.bugs.url !== undefined) {
            this.bugReportUrl = `${packageJson.bugs.url}`;
        } else {
            this.bugReportUrl = "";
        }

        if (packageJson.bugs !== undefined && packageJson.bugs.email !== undefined) {
            this.bugReportEmail = `${packageJson.bugs.email}`;
        } else {
            this.bugReportEmail = "";
        }

        this.info = `${this.id}`;
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

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    registerPreprocessor(packageJSON:any, callback: COBOLPreprocessor): COBOLPreprocessorHandle {
        if (packageJSON === undefined || callback === undefined) {
            throw new Error("Invalid argument (registerPreprocessor)");
        }
        const handle = new CobApiHandle(packageJSON,callback);
        if (handle.id.length === 0) {
            throw new Error("Invalid packageJSON, no id present (registerPreprocessor)");
        }

        if (handle.bugReportEmail.length === 0) {
            throw new Error("Invalid packageJSON, no bug email address present (registerPreprocessor)");
        }

        if (handle.bugReportUrl.length === 0) {
            throw new Error("Invalid packageJSON, no bug url present to report issue (registerPreprocessor)");
        }

        COBOLPreprocessorHelper.preprocessors.set(handle, callback);

        return handle;
    }

    logWarningMessage(handle: COBOLPreprocessorHandle, message: string): void {
        this.externalFeatures.logMessage(`[${handle.info}]: ${message}`);
    }
}
