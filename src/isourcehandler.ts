
export default interface ISourceHandler {
    getLineCount(): number;
    getLine(lineNumber: number): string;
}