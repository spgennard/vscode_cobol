import ISourceHandler, { ICommentCallback } from './isourcehandler';
import * as vscode from 'vscode';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';

export class VSCodeSourceHandler implements ISourceHandler {
    commentCount: number;
    document: vscode.TextDocument;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    commentCallback?: ICommentCallback;

    public constructor(document: vscode.TextDocument, dumpNumbersInAreaA: boolean, commentCallback?: ICommentCallback) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.dumpAreaBOnwards = false;
        this.commentCount = 0;
        this.commentCallback = commentCallback;
    }

    getLineCount(): number {
        return this.document.lineCount;
    }

    getCommentCount(): number {
        return this.commentCount;
    }

    private static paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ]/g;

    private sendCommentCallback(line: string, lineNumber: number) {
        if (this.commentCallback !== undefined) {
            this.commentCallback.processComment(line, this.getFilename(), lineNumber);
        }
    }

    getLine(lineNumber: number): string|undefined {

        if (lineNumber >= this.document.lineCount) {
            return undefined;
        }

        const lineText = this.document.lineAt(lineNumber);

        let line = lineText.text;

        const startComment = line.indexOf("*>");
        if (startComment !== -1) {
            this.sendCommentCallback(line, lineNumber);
            line = line.substring(0, startComment);
            this.commentCount++;
        }
        // drop fixed format line
        if (line.length > 1 && line[0] === '*') {
            this.commentCount++;
            this.sendCommentCallback(line, lineNumber);
            return "";
        }
        // drop fixed format line
        if (line.length > 7 && line[6] === '*') {
            this.commentCount++;
            this.sendCommentCallback(line, lineNumber);
            return "";
        }

        // todo - this is a bit messy and should be revised
        if (this.dumpNumbersInAreaA) {
            if (line.match(VSCodeSourceHandler.paraPrefixRegex1)) {
                line = "      " + line.substr(6);
            } else {
                if (line.length > 7 && line[6] === ' ') {
                    const possibleKeyword = line.substr(0, 6).trim();
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
        return cobolKeywordDictionary.has(keyword);
    }

    getUri() : vscode.Uri {
        return this.document.uri;
    }

    getFilename(): string {
        return this.document.fileName;
    }

    setCommentCallback(commentCallback: ICommentCallback):void {
        this.commentCallback = commentCallback;
    }

    resetCommentCount():void {
        this.commentCount = 0;
    }
}
