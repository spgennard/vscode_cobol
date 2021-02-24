export interface COBOLPreprocessorHandle {
    id: string;
}

export interface COBOLPreprocessorOutput {
    addLine(line: string):void;
    addLines(lines: string[]):void;
    addFileSymbol(symbol:string, copybookName: string):void;
}

export interface COBOLPreprocessor {
    start(source:string):void;
    process(source:string, line:string, output:COBOLPreprocessorOutput): boolean;
    end(source:string):void;
}

export interface COBOLApi {
    logWarningMessage(handle:COBOLPreprocessorHandle, message: string):void;
    registerPreprocessor(owner:string, callback:COBOLPreprocessor): COBOLPreprocessorHandle;
}
