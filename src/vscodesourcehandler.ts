import { StringBuilder } from 'typescript-string-operations';
import * as vscode from 'vscode';
import { workspace } from 'vscode';
import ISourceHandler, { ICommentCallback } from './isourcehandler';
import { getCOBOLKeywordDictionary } from './keywords/cobolKeywords';
import { VSCOBOLFileUtils } from './vsfileutils';

export class VSCodeSourceHandler implements ISourceHandler {
    commentCount: number;
    document: vscode.TextDocument|undefined;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    commentCallback?: ICommentCallback;
    lineCount: number;
    documentVersionId: BigInt;
    isSourceInWorkSpace: boolean;
    shortWorkspaceFilename: string;
    updatedSource: Map<number, string>;
    languageId: string;

    public constructor(document: vscode.TextDocument, dumpNumbersInAreaA: boolean, commentCallback?: ICommentCallback) {
        this.document = document;
        this.dumpNumbersInAreaA = dumpNumbersInAreaA;
        this.dumpAreaBOnwards = false;
        this.commentCount = 0;
        this.commentCallback = commentCallback;
        this.lineCount = this.document.lineCount;
        this.documentVersionId = BigInt(this.document.version);
        this.languageId = document.languageId;

        const workspaceFilename = VSCOBOLFileUtils.getShortWorkspaceFilename(document.fileName);
        this.shortWorkspaceFilename = workspaceFilename === undefined ? "" : workspaceFilename;
        this.isSourceInWorkSpace = this.shortWorkspaceFilename.length !== 0;
        this.updatedSource = new Map<number, string>();

        // if we cannot be trusted and the file is outside the workspace, dont read it
        if (vscode.workspace.isTrusted === false && !this.isSourceInWorkSpace) {
            this.commentCallback = undefined;
            this.document = undefined;
        }
    }

    getDocumentVersionId(): BigInt {
        return this.documentVersionId;
    }

    getUriAsString(): string {
        return this.document === undefined ? "" : this.document.uri.toString();
    }

    getLineCount(): number {
        return this.lineCount;
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

    getLine(lineNumber: number, raw: boolean): string | undefined {
        if (this.document === undefined || lineNumber >= this.lineCount) {
            return undefined;
        }

        const lineText = this.document.lineAt(lineNumber);

        let line = lineText.text;

        if (raw) {
            return line;
        }

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
        if (line.length >= 7 && (line[6] === '*' || line[6] === '/') ) {
            this.commentCount++;
            if (line[6] === '/') {
                this.sendCommentCallback(line, lineNumber);
            }
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

    getLineTabExpanded(lineNumber: number):string|undefined {
        const unexpandedLine = this.getLine(lineNumber, true);
        if (unexpandedLine === undefined) {
            return undefined;
        }

        // do we have a tab?
        if (unexpandedLine.indexOf('\t') === -1) {
            return unexpandedLine;
        }

        const editorConfig = workspace.getConfiguration('editor');
        const tabSize = editorConfig === undefined ? 4 : editorConfig.get<number>('tabSize', 4);

        let col = 0;
        const buf = new StringBuilder();
        for (const c of unexpandedLine) {
            if (c === '\t') {
                do {
                    buf.Append(' ');
                } while (++col % tabSize !== 0);
            } else {
                buf.Append(c);
            }
            col++;
        }
        return buf.ToString();
    }

    setDumpAreaA(flag: boolean): void {
        this.dumpNumbersInAreaA = flag;
    }

    setDumpAreaBOnwards(flag: boolean): void {
        this.dumpAreaBOnwards = flag;
    }

    isValidKeyword(keyword: string): boolean {
        return getCOBOLKeywordDictionary(this.languageId).has(keyword);
    }

    getFilename(): string {
        return this.document !== undefined ? this.document.fileName : "";
    }

    setCommentCallback(commentCallback: ICommentCallback): void {
        this.commentCallback = commentCallback;
    }

    resetCommentCount(): void {
        this.commentCount = 0;
    }

    getIsSourceInWorkSpace(): boolean {
        return this.isSourceInWorkSpace;
    }

    getShortWorkspaceFilename(): string {
        return this.shortWorkspaceFilename;
    }

    setUpdatedLine(lineNumber: number, line: string): void {
        this.updatedSource.set(lineNumber, line);
    }

    getUpdatedLine(linenumber: number): string | undefined {
        if (this.updatedSource.has(linenumber)) {
            return this.updatedSource.get(linenumber);
        }

        return this.getLine(linenumber, false);
    }

    getLanguageId(): string {
        return this.languageId;
    }
}
