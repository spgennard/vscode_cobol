import { Uri } from "vscode";

export interface ICommentCallback {
    processComment(commentLine: string) : void;
}

export default interface ISourceHandler {
    getUri(): Uri;
    getLineCount(): number;
    getCommentCount(): number;
    getLine(lineNumber: number): string;
    getRawLine(lineNumber: number): string;
    setDumpAreaA(flag: boolean): void;
    setDumpAreaBOnwards(flag: boolean): void;
    isCacheBelow(): boolean;
    getFilename(): string;
}