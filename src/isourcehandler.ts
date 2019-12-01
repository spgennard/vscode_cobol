import { Uri } from "vscode";

export default interface ISourceHandler {
    getUri(): Uri;
    getLineCount(): number;
    getCommentCount(): number;
    getLine(lineNumber: number): string;
    getRawLine(lineNumber: number): string;
    setDumpAreaA(flag: boolean): void;
    setDumpAreaBOnwards(flag: boolean): void;
}