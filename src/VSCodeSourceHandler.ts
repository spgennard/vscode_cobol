import ISourceHandler from './isourcehandler';
import * as vscode from 'vscode';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';

export class VSCodeSourceHandler implements ISourceHandler {
    document: vscode.TextDocument;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    public constructor(document: vscode.TextDocument, dumpNumbersInAreaA: boolean) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.dumpAreaBOnwards = false;
    }
    
    getLineCount(): number {
        return this.document.lineCount;
    }

    getRawLine(lineNumber: number): string {
        try {
            let line = this.document.lineAt(lineNumber);
            return line.text;
        }
        catch
        {
            return "";
        }
    }

    private static paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ]/g;

    getLine(lineNumber: number): string {
        let line = this.document.lineAt(lineNumber).text;

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
            if (line.match(VSCodeSourceHandler.paraPrefixRegex1)) {
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
