import ISourceHandler, { ICommentCallback } from './isourcehandler';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';
import { logException, logTimedMessage } from './extension';
import { performance } from 'perf_hooks';

// var detab = require('detab');
const lineByLine = require('n-readlines');
import fs from 'fs';
import { Uri } from 'vscode';

export class FileSourceHandler implements ISourceHandler {
    document: string;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    lines: string[];
    commentCount: number;
    commentCallback?: ICommentCallback;

    public constructor(document: string, dumpNumbersInAreaA: boolean,   commentCallback?: ICommentCallback) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.commentCallback = commentCallback;
        this.dumpAreaBOnwards = false;
        this.lines = [];
        this.commentCount = 0;

        let docstat = fs.statSync(document);
        let docChunkSize = docstat.size < 4096 ? 4096 : 96 * 1024;
        let line: string;
        var startTime = performance.now();
        try {
            const liner = new lineByLine(document, { readChunk: docChunkSize });
            while (line = liner.next()) {
                this.lines.push(line.toString());
            }
            logTimedMessage(performance.now() - startTime, ' - Loading File ' + document);
        }
        catch (e) {
            logException("File failed! (" + document + ")", e);
        }

    }

    private sendCommentCallback(line: string) {
        if (this.commentCallback !== undefined) {
            this.commentCallback.processComment(line);
        }
    }

    getLineCount(): number {
        return this.lines.length;
    }

    getCommentCount(): number {
        return this.commentCount;
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
            this.sendCommentCallback(line);
            line = line.substring(0, startComment);
            this.commentCount++;
        }
        // drop fixed format line
        if (line.length > 1 && line[0] === '*') {
            this.commentCount++;
            this.sendCommentCallback(line);
            return "";
        }

        // drop fixed format line
        if (line.length > 7 && line[6] === '*') {
            this.commentCount++;
            this.sendCommentCallback(line);
            return "";
        }

        // todo - this is a bit messy and should be revised
        if (this.dumpNumbersInAreaA) {
            if (line.match(FileSourceHandler.paraPrefixRegex1)) {
                line = "      " + line.substr(6);
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

    getUri() : Uri {
        return Uri.file(this.document);
    }
}
