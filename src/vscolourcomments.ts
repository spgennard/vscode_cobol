import { DecorationOptions, DecorationRenderOptions, Position, Range, TextDocument, TextEditor, TextEditorDecorationType, window, workspace } from "vscode";
import { ESourceFormat } from "./externalfeatures";
import { commentRange, ICommentCallback, ISourceHandlerLite } from "./isourcehandler";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { TextLanguage, VSExtensionUtils } from "./vsextutis";
import { VSLogger } from "./vslogger";

class CommentColourHandlerImpl implements ICommentCallback {

    static readonly commentDec = window.createTextEditorDecorationType({

    });

    private tags = new Map<string, TextEditorDecorationType>();
    private currentLanguage: string = "";

    constructor() {
        this.setupTags();
    }

    private setupTags(): void {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const items = workspace.getConfiguration("coboleditor").get("comments_tags") as any;
        if (items === undefined) {
            return;
        }

        for (const item of items) {
            try {
                const options: DecorationRenderOptions = { color: item.color, backgroundColor: item.backgroundColor };

                if (item.strikethrough) {
                    options.textDecoration += "line-through";
                }

                if (item.underline) {
                    options.textDecoration += " underline";
                }

                if (item.bold) {
                    options.fontWeight = "bold";
                }

                if (item.italic) {
                    options.fontStyle = "italic";
                }

                const decl = window.createTextEditorDecorationType(options);
                const tag = item.tag as string;
                this.tags.set(tag.toUpperCase(), decl);
            } catch (e) {
                VSLogger.logException("Invalid comments_tags entry", e as Error);
            }
        }
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    processComment(sourceHandler: ISourceHandlerLite, commentLine: string, sourceFilename: string, sourceLineNumber: number, startPos: number, format: ESourceFormat): void {

        this.currentLanguage = sourceHandler.getLanguageId();
        const ranges = sourceHandler.getNotedComments();
        const commentLineUpper = commentLine.toUpperCase();
        const skipPos = format === ESourceFormat.variable ? 2 : 1;
        // let added = false;
        for (const [tag,] of this.tags) {
            const pos = commentLineUpper.indexOf(tag, skipPos + startPos);

            if (pos !== -1) {
                // ranges.push(new commentRange(sourceLineNumber, pos, tag.length, tag));+
                ranges.push(new commentRange(sourceLineNumber, skipPos + startPos, commentLineUpper.length - (startPos + skipPos - 1), tag));
                // added=true;
            }
        }
    }

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    public async updateDecorations(activeTextEditor: TextEditor | undefined) {
        if (!activeTextEditor) {
            return;
        }
        const doc: TextDocument = activeTextEditor.document;
        const textLanguage: TextLanguage = VSExtensionUtils.isSupportedLanguage(doc);

        if (textLanguage !== TextLanguage.COBOL) {
            return;
        }

        const currentLanguage = doc.languageId;
        if (this.currentLanguage !== currentLanguage) {
            const decorationOptions: DecorationOptions[] = [];
            for (const [, dec] of this.tags) {
                activeTextEditor.setDecorations(dec, decorationOptions);
            }
        }

        const configHandler = VSCOBOLConfiguration.get();
        const gcp = VSCOBOLSourceScanner.getCachedObject(doc, configHandler);
        if (gcp !== undefined) {
            const ranges = gcp.sourceHandler.getNotedComments();
            const mapOfDecorations = new Map<string, DecorationOptions[]>();
            for (const range of ranges) {
                const startPos = new Position(range.startLine, range.startColumn);
                const endPos = new Position(range.startLine, range.startColumn + range.length);
                const decoration = { range: new Range(startPos, endPos) };

                if (!mapOfDecorations.has(range.commentStyle)) {
                    const decorationOptions: DecorationOptions[] = [];
                    mapOfDecorations.set(range.commentStyle, decorationOptions);
                }

                const decorationOptions = mapOfDecorations.get(range.commentStyle);
                if (decorationOptions !== undefined) {
                    decorationOptions.push(decoration);
                }
            }

            for (const [decTagName,] of this.tags) {
                const decorationOption = mapOfDecorations.get(decTagName);
                if (decorationOption === undefined) {
                    const empty: DecorationOptions[] = [];
                    activeTextEditor.setDecorations(CommentColourHandlerImpl.commentDec, empty);
                } else {
                    const dec = this.tags.get(decTagName);
                    if (dec !== undefined) {
                        activeTextEditor.setDecorations(dec, decorationOption)
                    } 
                }
            }
        } else {
            const decorationOptions: DecorationOptions[] = [];
            for (const [, dec] of this.tags) {
                activeTextEditor.setDecorations(dec, decorationOptions);
            }
        }
    }
}

export const colourCommentHandler = new CommentColourHandlerImpl();

