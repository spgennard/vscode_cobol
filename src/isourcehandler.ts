
export default interface ISourceHandler {
    getLineCount(): number;
    getLine(lineNumber: number): string;
    setDumpAreaA(flag: boolean): void;
    setDumpAreaBOnwards(flag: boolean): void;
}