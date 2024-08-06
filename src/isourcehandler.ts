/* eslint-disable @typescript-eslint/ban-types */
import { ESourceFormat } from "./externalfeatures";

export interface ICommentCallback {
    processComment(sourceHandler: ISourceHandlerLite, commentLine: string, sourceFilename: string, sourceLineNumber:number, startPos: number, format: ESourceFormat) : void;
}

export class commentRange {
    public startLine: number;
    public startColumn: number;
    public length: number;
    public commentStyle: string;

    constructor(startLine: number, startColumn: number, length: number, commentStyle: string) {
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.length = length;
        this.commentStyle = commentStyle;
    }
}

export interface ISourceHandlerLite {
    getLineCount(): number;
    getLanguageId():string;
    getFilename(): string;
    getLineTabExpanded(lineNumber: number):string|undefined;
    getNotedComments(): commentRange[];
    getCommentAtLine(lineNumber: number):string;
}

export interface ISourceHandler {
    getUriAsString(): string;
    getLineCount(): number;
    getCommentCount(): number;
    resetCommentCount():void;
    getLine(lineNumber: number, raw: boolean): string|undefined;
    getLineTabExpanded(lineNumber: number):string|undefined;
    setUpdatedLine(lineNumber: number, line:string) : void;
    getUpdatedLine(linenumber: number) : string|undefined;
    setDumpAreaA(flag: boolean): void;
    setDumpAreaBOnwards(flag: boolean): void;
    getFilename(): string;
    addCommentCallback(commentCallback: ICommentCallback):void;
    getDocumentVersionId(): BigInt;
    getIsSourceInWorkSpace(): boolean;
    getShortWorkspaceFilename(): string;
    getLanguageId():string;
    setSourceFormat(format: ESourceFormat):void;
    getNotedComments(): commentRange[];
    getCommentAtLine(lineNumber: number):string;
}