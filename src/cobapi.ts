
export interface COBOLPreprocessor {
    start(source:string):void;
    process(line:string): string[];
    end(source:string):void;
}

export interface COBOLApi {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    logWarningMessage(message: string):void;
    registerPreprocessor(callback:COBOLPreprocessor): boolean;
}
