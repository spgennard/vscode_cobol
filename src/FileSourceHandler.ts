import ISourceHandler from './isourcehandler';
import * as vscode from 'vscode';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';
import * as fs from 'fs';

export class FileSourceHandler implements ISourceHandler {
    document: string;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    lines: string[];

    public constructor(document: string, dumpNumbersInAreaA: boolean) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.dumpAreaBOnwards = false;
        this.lines = fs.readFileSync(document).toString().split('\n');
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
            let paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ]/g;
            if (line.match(paraPrefixRegex1)) {
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
