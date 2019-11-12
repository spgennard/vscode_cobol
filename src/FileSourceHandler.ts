import ISourceHandler from './isourcehandler';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';
import { logCOBOLChannelLine, logCOBOLChannelLineException } from './extension';

// var detab = require('detab');
const lineByLine = require('n-readlines');

export class FileSourceHandler implements ISourceHandler {
    document: string;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    lines: string[];

    public constructor(document: string, dumpNumbersInAreaA: boolean) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.dumpAreaBOnwards = false;
        this.lines = [];

        let line: string;
        //const editor = vscode.window.activeTextEditor;
        // let tabSize: number = 4;
        // if (editor) {
        //     if (typeof (editor.options.tabSize) === "number") {
        //         tabSize = editor.options.tabSize as number;
        //     }
        //     if (typeof (editor.options.tabSize) === "string") {
        //         tabSize = Number.parseInt(editor.options.tabSize as string);
        //     }
        // }

        try {
            const liner = new lineByLine(document, {readChunk:256*1024} );
            while (line = liner.next()) {
                // if (line.indexOf('\t') !== -1) {
                //     line = detab(line.toString(), tabSize);
                // }
                this.lines.push(line.toString());
            }
        }
        catch (e) {
            logCOBOLChannelLineException("File failed! ("+document+")",e);
        }
    }

    getLineCount(): number {
        return this.lines.length;
    }

    getRawLine(lineNumber: number): string {
        try {
            let line = this.lines[lineNumber];
            if (line === null || line === undefined) {
                return "";
            }
            return line;
        }
        catch
        {
            return "";
        }
    }

    private static readonly paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ]/g;

    getLine(lineNumber: number): string {
        let line = this.getRawLine(lineNumber);

        let startComment = line.indexOf("*>");
        if (startComment !== -1) {
            line = line.substring(0, startComment);
        }
        // drop fixed format line
        if (line.length > 1 && line[0] === '*') {
            return "";
        }

        // drop fixed format line
        if (line.length > 7 && line[6] === '*') {
            return "";
        }

        // todo - this is a bit messy and should be revised
        if (this.dumpNumbersInAreaA) {
            if (line.match(FileSourceHandler.paraPrefixRegex1)) {
                line = "       " + line.substr(6);
            } else {
                if (line.length > 7 && line[6] === ' ') {
                    let possibleKeyword = line.substr(0, 6).trim();
                    if (this.isValidKeyword(possibleKeyword) === false) {
                        line = "       " + line.substr(6);
                    }
                }
            }
        }
        if (this.dumpAreaBOnwards && line.length >= 73) {
            line = line.substr(0, 72);
        }

        return line;
    }

    setDumpAreaA(flag: boolean): void {
        this.dumpNumbersInAreaA = flag;
    }

    setDumpAreaBOnwards(flag: boolean): void {
        this.dumpAreaBOnwards = flag;
    }

    isValidKeyword(keyword: string): boolean {
        return cobolKeywordDictionary.containsKey(keyword);
    }
}
