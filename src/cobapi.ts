/* eslint-disable @typescript-eslint/no-explicit-any */
// eslint-disable-next-line @typescript-eslint/no-unused-vars

export class COBAPIConstants {
    public static COB_API_INTERFACE_VERSION = 2;
}
/* eslint-disable @typescript-eslint/no-explicit-any */
export interface COBOLPreprocessorHandle {
    packageJson: any;
    info: string;

    logWarningMessage(message: string):void;
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
    start(source:string, handle: COBOLPreprocessorHandle, callbacks:COBOLPreprocessorCallbacks):void;
    startDivision(source:string, divisionName: string):void;
    startSection(source:string, sectionName: string):void;
    process(source:string, line:string, output:COBOLPreprocessorOutput): boolean;
    end(source:string):void;

    getPackageJson(): any;
    getImplementedVersion(): number;

}
