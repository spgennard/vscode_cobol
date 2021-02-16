
export interface IPreProcessorCallback {
    processLine(actualLine:string ) : string[];
}

export interface IPreProcessor {
    registerPreProcInterest(callback: IPreProcessorCallback):void;
}