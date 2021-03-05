import ISourceHandler, { ICommentCallback } from './isourcehandler';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';

// let detab = require('detab');
// eslint-disable-next-line @typescript-eslint/no-var-requires
const lineByLine = require('n-readlines');
import fs from 'fs';

import { EmptyExternalFeature, IExternalFeatures } from './externalfeatures';
import { pathToFileURL } from 'url';

export class FileSourceHandler implements ISourceHandler {
    document: string;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    lines: string[];
    commentCount: number;
    commentCallback?: ICommentCallback;
    documentVersionId: BigInt;

    public constructor(document: string, dumpNumbersInAreaA: boolean, commentCallback?: ICommentCallback, features?: IExternalFeatures) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.commentCallback = commentCallback;
        this.dumpAreaBOnwards = false;
        this.lines = [];
        this.commentCount = 0;

        if (features === undefined) {
            features = EmptyExternalFeature.Default;
        }
        const docstat = fs.statSync(document, {bigint:true});
        const docChunkSize = docstat.size < 4096 ? 4096 : 96 * 1024;
        let line: string;
        this.documentVersionId = docstat.mtimeMs;
        const startTime = features.performance_now();
        try {
            const liner = new lineByLine(document, { readChunk: docChunkSize });
            while ((line = liner.next())) {
                this.lines.push(line.toString());
            }
            features.logTimedMessage(features.performance_now() - startTime, ' - Loading File ' + document);
        }
        catch (e) {
            features.logException("File failed! (" + document + ")", e);
        }

    }
    getDocumentVersionId(): BigInt {
        return this.documentVersionId;
    }

    private sendCommentCallback(line: string, lineNumber:number) {
        if (this.commentCallback !== undefined) {
            this.commentCallback.processComment(line, this.getFilename(), lineNumber);
        }
    }

    getUriAsString(): string {
        return pathToFileURL(this.getFilename()).href;
    }

    getLineCount(): number {
        return this.lines.length;
    }

    getCommentCount(): number {
        return this.commentCount;
    }

    private static readonly paraPrefixRegex1 = /^[0-9 ][0-9 ][0-9 ][0-9 ][0-9 ][0-9 ]/g;

    getLine(lineNumber: number): string | undefined {
        let line:string|undefined=undefined;

        try {
            if (lineNumber >= this.lines.length) {
                return undefined;
            }

            line = this.lines[lineNumber];

            const startComment = line.indexOf("*>");
            if (startComment !== -1) {
                this.sendCommentCallback(line,lineNumber);
                line = line.substring(0, startComment);
                this.commentCount++;
            }
            // drop fixed format line
            if (line.length > 1 && line[0] === '*') {
                this.commentCount++;
                this.sendCommentCallback(line,lineNumber);
                return "";
            }

            // drop fixed format line
            if (line.length > 7 && line[6] === '*') {
                this.commentCount++;
                this.sendCommentCallback(line,lineNumber);
                return "";
            }

            // todo - this is a bit messy and should be revised
            if (this.dumpNumbersInAreaA) {
                if (line.match(FileSourceHandler.paraPrefixRegex1)) {
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
        }
        catch {
            return undefined;
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

    getFilename(): string {
        return this.document;
    }

    setCommentCallback(commentCallback: ICommentCallback): void {
        this.commentCallback = commentCallback;
    }

    resetCommentCount(): void {
        this.commentCount = 0;
    }
}
