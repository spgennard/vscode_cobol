import fs from "fs";

import { ISourceHandler, ICommentCallback, ISourceHandlerLite, commentRange } from "./isourcehandler";
import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import { pathToFileURL } from "url";
import path from "path";
import { getCOBOLKeywordDictionary } from "./keywords/cobolKeywords";
import { ExtensionDefaults } from "./extensionDefaults";
import { SimpleStringBuilder } from "./stringutils";
import { ICOBOLSettings } from "./iconfiguration";

export class FileSourceHandler implements ISourceHandler, ISourceHandlerLite {
    document: string;
    dumpNumbersInAreaA: boolean;
    dumpAreaBOnwards: boolean;
    lines: string[];
    commentCount: number;
    commentCallbacks?: ICommentCallback[];
    // eslint-disable-next-line @typescript-eslint/ban-types
    documentVersionId: BigInt;
    isSourceInWorkspace: boolean;
    updatedSource: Map<number, string>;
    shortFilename: string;
    languageId: string;
    format: ESourceFormat;
    notedCommentRanges: commentRange[];

    commentsIndex: Map<number, string>;
    commentsIndexInline: Map<number, boolean>;
    private readonly lineRegExFilter: RegExp | undefined;
    settings: ICOBOLSettings;

    public constructor(settings: ICOBOLSettings, regEx: RegExp | undefined, document: string, features: IExternalFeatures) {
        this.lineRegExFilter = regEx;
        this.document = document;
        this.dumpNumbersInAreaA = false;
        this.commentCallbacks = [];
        this.dumpAreaBOnwards = false;
        this.lines = [];
        this.commentCount = 0;
        this.isSourceInWorkspace = false;
        this.updatedSource = new Map<number, string>();
        this.languageId = ExtensionDefaults.defaultCOBOLLanguage;
        this.format = ESourceFormat.unknown;
        this.notedCommentRanges = [];
        this.commentsIndex = new Map<number, string>();
        this.commentsIndexInline = new Map<number, boolean>();
        this.settings = settings;
        this.shortFilename = this.findShortWorkspaceFilename(document, features, settings);
        const docstat: fs.BigIntStats = fs.statSync(document, { bigint: true });

        this.documentVersionId = docstat.mtimeMs;
        const startTime = features.performance_now();
        try {
            let line: string;

            const linesRead = fs.readFileSync(document).toString().split(/\r?\n/);
            if (this.lineRegExFilter !== undefined) {
                for(line of linesRead) {
                    const l = line.toString();
                    if (this.lineRegExFilter.test(l)) {
                        this.lines.push(l);
                    } else {
                        // keep line position
                        this.lines.push("");
                    }
                }
            } else {
                this.lines = linesRead;
            }
            features.logTimedMessage(features.performance_now() - startTime, " - Loading File " + document);
        }
        catch (e) {
            features.logException("File load failed! (" + document + ")", e as Error);
        }
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    getDocumentVersionId(): BigInt {
        return this.documentVersionId;
    }

    private sendCommentCallback(line: string, lineNumber: number, startPos: number, format: ESourceFormat) {
        if (this.commentsIndex.has(lineNumber) === false) {
            let isInline = false;
            let l = line.substring(startPos).trimStart();

            if (l.startsWith("*>") && (format !== ESourceFormat.fixed)) {
                isInline = true;
                l = l.substring(2).trim();
            } else {
                const lastPos = line.length < 72 ? line.length : 72;

                if (line.length >= 7 && (line[6] === "*" || line[6] === "/")) {
                    if (line[6] === '*' && line[7] === '>')
                        l = line.substring(8,lastPos).trim();
                    else
                        l = line.substring(7,lastPos).trim();
                    isInline = false;
                }
            }

            if (l.length !== 0) {
                this.commentsIndex.set(lineNumber, l);
                this.commentsIndexInline.set(lineNumber, isInline);
            }
        }

        if (this.commentCallbacks !== undefined) {
            for (const commentCallback of this.commentCallbacks) {
                commentCallback.processComment(this.settings, this, line, this.getFilename(), lineNumber, startPos, format);
            }
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

    getLine(lineNumber: number, raw: boolean): string | undefined {
        let line: string | undefined = undefined;

        try {
            if (lineNumber >= this.lines.length) {
                return undefined;
            }

            line = this.lines[lineNumber];

            if (raw) {
                return line;
            }

            const startComment = line.indexOf("*>");
            if (startComment !== -1 && startComment !== 6) {
                this.commentCount++;
                this.sendCommentCallback(line, lineNumber, startComment, ESourceFormat.variable);
                line = line.substring(0, startComment);
            }

            // drop fixed format line
            if (line.length > 1 && line[0] === "*") {
                this.commentCount++;
                this.sendCommentCallback(line, lineNumber, 0, ESourceFormat.variable);
                return "";
            }

            // drop fixed format line
            if (line.length >= 7 && (line[6] === "*" || line[6] === "/")) {
                this.commentCount++;
                this.sendCommentCallback(line, lineNumber, 6, ESourceFormat.fixed);
                return "";
            }

            // only handle debug
            if (this.format === ESourceFormat.terminal) {
                if (line.startsWith("\\D") || line.startsWith("|")) {
                    this.commentCount++;
                    this.sendCommentCallback(line, lineNumber, 0, ESourceFormat.terminal);
                    return "";
                }
            }

            // todo - this is a bit messy and should be revised
            if (this.dumpNumbersInAreaA) {
                if (line.match(FileSourceHandler.paraPrefixRegex1)) {
                    line = "      " + line.substring(6);
                } else {
                    if (line.length > 7 && line[6] === " ") {
                        const possibleKeyword = line.substring(0, 6).trim();
                        if (this.isValidKeyword(possibleKeyword) === false) {
                            line = "      " + line.substring(6);
                        }
                    }
                }
            }

            if (this.dumpAreaBOnwards && line.length >= 73) {
                line = line.substring(0, 72);
            }
        }
        catch {
            return undefined;
        }

        return line;
    }

    getLineTabExpanded(lineNumber: number): string | undefined {
        const unexpandedLine = this.getLine(lineNumber, true);
        if (unexpandedLine === undefined) {
            return undefined;
        }

        // do we have a tab?
        if (unexpandedLine.indexOf("\t") !== -1) {
            return unexpandedLine;
        }

        const tabSize = 4;

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
        return this.document;
    }

    addCommentCallback(commentCallback: ICommentCallback): void {
        this.commentCallbacks?.push(commentCallback);
    }

    resetCommentCount(): void {
        this.commentCount = 0;
    }

    getIsSourceInWorkSpace(): boolean {
        return this.isSourceInWorkspace;
    }

    getShortWorkspaceFilename(): string {
        return this.shortFilename;
    }

    getUpdatedLine(linenumber: number): string | undefined {
        if (this.updatedSource.has(linenumber)) {
            return this.updatedSource.get(linenumber);
        }

        return this.getLine(linenumber, false);
    }

    setUpdatedLine(lineNumber: number, line: string): void {
        this.updatedSource.set(lineNumber, line);
    }

    private findShortWorkspaceFilename(ddir: string, features: IExternalFeatures, config: ICOBOLSettings): string {
        const ws = features.getWorkspaceFolders(config);
        if (ws === undefined || ws.length === 0) {
            return "";
        }

        const fullPath = path.normalize(ddir);
        let bestShortName = "";
        for (const folderPath of ws) {
            if (fullPath.startsWith(folderPath)) {
                const possibleShortPath = fullPath.substring(1 + folderPath.length);
                if (bestShortName.length === 0) {
                    bestShortName = possibleShortPath;
                } else {
                    if (possibleShortPath.length < possibleShortPath.length) {
                        bestShortName = possibleShortPath;
                    }
                }
            }
        }

        return bestShortName;
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
                    let lines = "" + this.commentsIndex.get(lineNumber - 1) + "\n";

                    while (maxIncludedLines > 0) {
                        if (this.commentsIndex.has(lineNumber - backwardsCount) === false) {
                            break;
                        }

                        if (this.commentsIndexInline.get(lineNumber - backwardsCount) === true) {
                            break;
                        }

                        let prevLines = lines;
                        lines = "" + this.commentsIndex.get(lineNumber - backwardsCount) + "\n" + prevLines;

                        backwardsCount++;
                        maxIncludedLines--;
                    }

                    return lines;
                }
            }
        }

        return "";
    }

    getText(startLine: number, startColumn: number, endLine: number, endColumn: number): string {
        let fl_r = this.getLine(startLine, true);
        if (!fl_r) {
            return "";
        }

        if (startLine === endLine) {
            return fl_r.substring(startColumn, endColumn - startColumn);
        }

        let fl = fl_r.substring(startColumn);
        let lines = (endLine - startLine) - 1;
        let lc = startLine + 1;
        while (lines-- >= 0) {
            fl_r = this.getLine(lc, true);
            if (fl_r === undefined) {
                return fl;
            }

            if (lc === endLine) {
                fl += "\n" + fl_r.substring(0, endColumn);
            } else {
                fl += "\n" + fl_r;
            }

            lc++;
        }

        return fl;
    }
}

