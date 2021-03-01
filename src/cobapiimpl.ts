/* eslint-disable @typescript-eslint/no-explicit-any */
// import { extensions } from "vscode";
import { COBAPIConstants, COBOLApi, COBOLPreprocessor, COBOLPreprocessorOutput } from "./cobapi";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { IExternalFeatures } from "./externalfeatures";

/* eslint-disable @typescript-eslint/no-explicit-any */
export interface COBOLPreprocessorHandle {
    packageJson: any;
    info: string;
}

export class CobApiHandle implements COBOLPreprocessorHandle {
    id: string;
    description: string;
    packageJson: any;
    bugReportUrl: string;
    bugReportEmail: string;
    callback: COBOLPreprocessor|undefined = undefined;
    info: string;

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    constructor(packageJson: any) {
        this.packageJson = packageJson;
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
    private handle: CobApiHandle|undefined;

    constructor(externalFeatures: IExternalFeatures) {
        this.externalFeatures = externalFeatures;
    }

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    registerPreprocessor(interface_version:number, packageJSON:any):void {

        if (packageJSON === undefined) {
            throw new Error("Invalid argument (registerPreprocessor)");
        }

        this.handle = new CobApiHandle(packageJSON);
        if (this.handle.id.length === 0) {
            throw new Error("Invalid packageJSON, no id present (registerPreprocessor)");
        }

        if (this.handle.bugReportEmail.length === 0) {
            throw new Error("Invalid packageJSON, no bug email address present (registerPreprocessor)");
        }

        if (this.handle.bugReportUrl.length === 0) {
            throw new Error("Invalid packageJSON, no bug url present to report issue (registerPreprocessor)");
        }

        if (interface_version !== COBAPIConstants.COB_API_INTERFACE_VERSION) {
            const messageForInfo = `Interface version requested is out of date, please contact ${this.handle.bugReportEmail} @ ${this.handle.bugReportUrl}\n` +
                          `for an updated of extension ${this.handle.id}`;
            throw new Error(messageForInfo);
        }

        const pp = this.externalFeatures.getCOBOLPreprocessor(packageJSON) as COBOLPreprocessor
        COBOLPreprocessorHelper.preprocessors.set(this.handle, pp);
    }

    logWarningMessage(message: string): void {
        if (this.handle !== undefined) {
            this.externalFeatures.logMessage(`[${this.handle.info}]: ${message}`);
        }
    }
}
