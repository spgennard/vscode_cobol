// eslint-disable-next-line @typescript-eslint/no-unused-vars

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export class COBAPIConstants {
    public static COB_API_INTERFACE_VERSION = 1;
}

/* eslint-disable @typescript-eslint/no-explicit-any */
export interface COBOLPreprocessorHandle {
    packageJson: any;
    info: string;
}

export interface COBOLPreprocessorCallbacks {
    getCurrentDivision(): string;
    getCurrentSection(): string;
    getCopyFilename(copybook: string, inInfo: string):string;
}

export interface COBOLPreprocessorOutput {
    addLine(line: string):void;
    addLines(lines: string[]):void;
    addFileSymbol(symbol:string, copybookName: string):void;
}

export interface COBOLPreprocessor {
    start(source:string):void;
    process(source:string, line:string, output:COBOLPreprocessorOutput, callbacks:COBOLPreprocessorCallbacks): boolean;
    end(source:string):void;
}

export interface COBOLApi {
    logWarningMessage(handle:COBOLPreprocessorHandle, message: string):void;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    registerPreprocessor(interface_version:number, packageJson: any, callback:COBOLPreprocessor): COBOLPreprocessorHandle;
}
