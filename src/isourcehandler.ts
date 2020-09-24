import { Uri } from "vscode";

export interface ICommentCallback {
    processComment(commentLine: string, sourceFilename: string, sourceLineNumber:number) : void;
}

export default interface ISourceHandler {
    getUri(): Uri;
    getLineCount(): number;
    getCommentCount(): number;
    resetCommentCount():void;
    getLine(lineNumber: number): string|undefined;
    setDumpAreaA(flag: boolean): void;
    setDumpAreaBOnwards(flag: boolean): void;
    getFilename(): string;
    setCommentCallback(commentCallback: ICommentCallback):void;
}