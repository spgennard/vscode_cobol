export interface COBOLPreprocessorHandle {
    id: string;
}

export interface COBOLPreprocessor {
    start(source:string):void;
    process(source:string, line:string): string[];
    end(source:string):void;
}

export interface COBOLApi {
    logWarningMessage(handle:COBOLPreprocessorHandle, message: string):void;
    registerPreprocessor(owner:string, callback:COBOLPreprocessor): COBOLPreprocessorHandle;
}
