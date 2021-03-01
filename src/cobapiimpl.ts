/* eslint-disable @typescript-eslint/no-explicit-any */
// import { extensions } from "vscode";
import { COBOLPreprocessor, COBOLPreprocessorHandle, COBOLPreprocessorOutput } from "./cobapi";
import { IExternalFeatures } from "./externalfeatures";

export class CobApiHandle implements COBOLPreprocessorHandle {
    id: string;
    description: string;
    packageJson: any;
    bugReportUrl: string;
    bugReportEmail: string;
    callback: COBOLPreprocessor|undefined = undefined;
    info: string;

    externalFeatures: IExternalFeatures;

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    constructor(packageJson: any, features: IExternalFeatures) {
        this.packageJson = packageJson;
        this.externalFeatures = features;

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

        // validate
        if (this.id.length === 0) {
            throw new Error("Invalid packageJSON, no id present (registerPreprocessor)");
        }

        if (this.bugReportEmail.length === 0) {
            throw new Error("Invalid packageJSON, no bug email address present (registerPreprocessor)");
        }

        if (this.bugReportUrl.length === 0) {
            throw new Error("Invalid packageJSON, no bug url present to report issue (registerPreprocessor)");
        }

        this.info = `${this.id}`;
    }

    logWarningMessage(message: string): void {
        this.externalFeatures.logMessage(`[${this.info}]: ${message}`);
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
