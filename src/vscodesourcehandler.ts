
import * as vscode from "vscode";
import { workspace } from "vscode";
// import { colourCommentHandler } from "./extension";
import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import { ISourceHandler, ICommentCallback, ISourceHandlerLite, commentRange } from "./isourcehandler";
import { getCOBOLKeywordDictionary } from "./keywords/cobolKeywords";
import { SimpleStringBuilder } from "./stringutils";
import { colourCommentHandler } from "./vscolourcomments";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { VSCOBOLFileUtils } from "./vsfileutils";

export class VSCodeSourceHandlerLite implements ISourceHandlerLite {
    document: vscode.TextDocument | undefined;
    lineCount: number;
    languageId: string;
    notedCommentRanges: commentRange[];

    public constructor(document: vscode.TextDocument) {
        this.document = document;
        this.lineCount = this.document.lineCount;
        this.languageId = document.languageId;
        this.notedCommentRanges = [];
    }

    public getLineCount(): number {
        return this.lineCount;
    }

    getLanguageId(): string {
        return this.languageId;
    }

    getFilename(): string {
        return this.document !== undefined ? this.document.fileName : "";
    }

    getRawLine(lineNumber: number): string | undefined {
        if (this.document === undefined || lineNumber >= this.lineCount) {
            return undefined;
        }

        const lineText = this.document.lineAt(lineNumber);

        return lineText === undefined ? undefined : lineText.text;
    }

    getLineTabExpanded(lineNumber: number): string | undefined {
        const unexpandedLine = this.getRawLine(lineNumber);
        if (unexpandedLine === undefined) {
            return undefined;
        }

        // do we have a tab?
        if (unexpandedLine.indexOf("\t") === -1) {
            return unexpandedLine;
        }

        const editorConfig = workspace.getConfiguration("editor");
        const tabSize = editorConfig === undefined ? 4 : editorConfig.get<number>("tabSize", 4);

        let col = 0;
        const buf = new SimpleStringBuilder();
        for (const c of unexpandedLine) {
            if (c === "\t") {
                do {
                    buf.Append(" ");
                } while (++col % tabSize !== 0);
            } else {
                buf.Append(c);
            }
            col++;
        }
        return buf.ToString();
    }


    getNotedComments(): commentRange[] {
        return this.notedCommentRanges;
    }

    getCommentAtLine(lineNumber: number): string {
        return "";
    }
}

export class VSCodeSourceHandler implements ISourceHandler, ISourceHandlerLite {
    commentCount: number;
    document: vscode.TextDocument | undefined;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    commentCallbacks: ICommentCallback[] = [];
    lineCount: number;
    // eslint-disable-next-line @typescript-eslint/ban-types
    documentVersionId: BigInt;
    isSourceInWorkSpace: boolean;
    shortWorkspaceFilename: string;
    updatedSource: Map<number, string>;
    languageId: string;
    format: ESourceFormat;
    externalFeatures: IExternalFeatures
    notedCommentRanges: commentRange[];

    commentsIndex: Map<number, string>;
    commentsIndexInline: Map<number, boolean>;

    public constructor(document: vscode.TextDocument) {
        this.document = document;
        this.dumpNumbersInAreaA = false;
        this.dumpAreaBOnwards = false;
        this.commentCount = 0;
        this.lineCount = this.document.lineCount;
        this.documentVersionId = BigInt(this.document.version);
        this.languageId = document.languageId;
        this.format = ESourceFormat.unknown;
        this.externalFeatures = VSExternalFeatures;
        this.notedCommentRanges = [];
        this.commentsIndex = new Map<number, string>();
        this.commentsIndexInline = new Map<number, boolean>();

        const workspaceFilename = VSCOBOLFileUtils.getShortWorkspaceFilename(document.uri.scheme, document.fileName);
        this.shortWorkspaceFilename = workspaceFilename === undefined ? "" : workspaceFilename;
        this.isSourceInWorkSpace = this.shortWorkspaceFilename.length !== 0;
        this.updatedSource = new Map<number, string>();

        // if we cannot be trusted and the file is outside the workspace, dont read it
        if (vscode.workspace.isTrusted === false && !this.isSourceInWorkSpace) {
            this.clear();
        }

        if (this.isFileExcluded()) {
            this.clear();
        }

        this.addCommentCallback(colourCommentHandler);
    }

    private clear(): void {
        this.commentCallbacks = [];
        this.document = undefined;
        this.lineCount = 0;
    }

    private isFileExcluded(): boolean {
        const config = VSCOBOLConfiguration.get();

        if (this.document !== undefined) {
            if (this.document.lineCount > config.scan_line_limit) {
                this.externalFeatures.logMessage(`Aborted scanning ${this.shortWorkspaceFilename} after line limit of ${config.scan_line_limit} has been exceeded`);
                return true;
            }

            for (const fileEx of config.files_exclude) {
                const documentFilter: vscode.DocumentFilter = {
                    pattern: fileEx
                };

                if (vscode.languages.match(documentFilter, this.document) !== 0) {
                    this.externalFeatures.logMessage(`Aborted scanning ${this.shortWorkspaceFilename} (files_exclude)`);
                    return true;
                }
            }
        }

        return false;
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
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

    private sendCommentCallback(line: string, lineNumber: number, startPos: number, format: ESourceFormat) {

        if (this.commentsIndex.has(lineNumber) === false) {
            let isInline = false;
            let l = line.substring(startPos).trimStart();
            if (l.startsWith("*>")) {
                isInline = true;
                l = l.substring(2).trim();
            } else {
                if (format === ESourceFormat.fixed) {
                    if (line.length >= 7 && (line[6] === "*" || line[6] === "/")) {
                        l = line.substring(7).trimStart();
                        isInline = false;
                    }
                }
            }

            if (l.length !== 0) {
                this.commentsIndex.set(lineNumber, l);
                this.commentsIndexInline.set(lineNumber, isInline);
            }
        }

        if (this.commentCallbacks !== undefined) {
            for (const commentCallback of this.commentCallbacks) {
                commentCallback.processComment(this, line, this.getFilename(), lineNumber, startPos, format);
            }
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
            this.commentCount++;
            this.sendCommentCallback(line, lineNumber, startComment, ESourceFormat.variable);
            line = line.substring(0, startComment);
        }

        // drop variable format line
        if (line.length > 1 && line[0] === "*") {
            this.commentCount++;
            this.sendCommentCallback(line, lineNumber, 0, ESourceFormat.free);
            return "";
        }

        // drop fixed format line
        if (line.length >= 7 && (line[6] === "*" || line[6] === "/")) {
            this.commentCount++;
            this.sendCommentCallback(line, lineNumber, 6, ESourceFormat.fixed);
            return "";
        }

        // handle debug and terminal
        if (this.format === ESourceFormat.terminal) {
            if (line.startsWith("\\D") || line.startsWith("|")) {
                this.commentCount++;
                this.sendCommentCallback(line, lineNumber, 0, ESourceFormat.terminal);
                return "";
            }
        }

        // todo - this is a bit messy and should be revised
        if (this.dumpNumbersInAreaA) {
            if (line.match(VSCodeSourceHandler.paraPrefixRegex1)) {
                line = "      " + line.substring(6);
            } else {
                if (line.length > 7 && line[6] === " ") {
                    const possibleKeyword = line.substring(0, 6).trim();
                    if (this.isValidKeyword(possibleKeyword) === false) {
                        line = "       " + line.substring(6);
                    }
                }
            }
        }

        if (this.dumpAreaBOnwards && line.length >= 73) {
            line = line.substring(0, 72);
        }

        return line;
    }

    getLineTabExpanded(lineNumber: number): string | undefined {
        const unexpandedLine = this.getLine(lineNumber, true);
        if (unexpandedLine === undefined) {
            return undefined;
        }

        // do we have a tab?
        if (unexpandedLine.indexOf("\t") === -1) {
            return unexpandedLine;
        }

        const editorConfig = workspace.getConfiguration("editor");
        const tabSize = editorConfig === undefined ? 4 : editorConfig.get<number>("tabSize", 4);

        let col = 0;
        const buf = new SimpleStringBuilder();
        for (const c of unexpandedLine) {
            if (c === "\t") {
                do {
                    buf.Append(" ");
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

    addCommentCallback(commentCallback: ICommentCallback): void {
        this.commentCallbacks.push(commentCallback);
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

    setSourceFormat(format: ESourceFormat): void {
        this.format = format;
    }

    getNotedComments(): commentRange[] {
        return this.notedCommentRanges;
    }

    getCommentAtLine(lineNumber: number): string {

        if (this.commentsIndex.has(lineNumber)) {
            return "" + this.commentsIndex.get(lineNumber);
        }

        if (lineNumber > 1) {
            if (this.commentsIndex.has(lineNumber - 1)) {
                // only interested in non-inline comments for previous line
                // todo... prehaps go back further?
                if (this.commentsIndexInline.get(lineNumber - 1) === false) {
                    let maxIncludedLines = 5;
                    let backwardsCount = 2;
                    let lines = "" + this.commentsIndex.get(lineNumber - 1)+"\n";

                    while(maxIncludedLines > 0)
                    {
                        if (this.commentsIndex.has(lineNumber - backwardsCount) === false)
                        {
                            break;
                        }

                        if (this.commentsIndexInline.get(lineNumber - backwardsCount) === true)
                        {
                            break;
                        }

                        let prevLines = lines;
                        lines = ""+this.commentsIndex.get(lineNumber - backwardsCount) + "\n"+prevLines;

                        backwardsCount++;
                        maxIncludedLines--;
                    }

                    return lines;
                }
            }
        }
        return "";
    }
}
