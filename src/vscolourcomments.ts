import { DecorationOptions, DecorationRenderOptions, Position, Range, TextDocument, TextEditor, TextEditorDecorationType, window, workspace } from "vscode";
import { ESourceFormat } from "./externalfeatures";
import { commentRange, ICommentCallback, ISourceHandlerLite } from "./isourcehandler";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { TextLanguage, VSExtensionUtils } from "./vsextutis";
import { VSLogger } from "./vslogger";

class CommentColourHandlerImpl implements ICommentCallback {
    static readonly commentDec = window.createTextEditorDecorationType({
        //
    });

    private tags = new Map<string, TextEditorDecorationType>();
    // private currentLanguage = "";

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
        const configHandler = VSCOBOLConfiguration.get();
        if (!configHandler.enable_comment_tags) {
            return;
        }
        
        const ranges = sourceHandler.getNotedComments();
        const commentLineUpper = commentLine.toUpperCase();
        // const skipPos = format === ESourceFormat.variable ? 2 : 1;
        const comment_tag_word = configHandler.comment_tag_word;

        for (const [tag,] of this.tags) {
            const pos = commentLineUpper.indexOf(tag, 1+startPos);

            if (pos !== -1) {
                if (comment_tag_word) {
                    ranges.push(new commentRange(sourceLineNumber, pos, tag.length, tag));
                } else {
                    ranges.push(new commentRange(sourceLineNumber, pos, commentLineUpper.length - startPos, tag));
                }
            }
        }
    }

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    public async updateDecorations(activeTextEditor: TextEditor | undefined) {
        if (!activeTextEditor) {
            return;
        }

        // get rid of all decorations
        const decorationOptions: DecorationOptions[] = [];
        for (const [, dec] of this.tags) {
            activeTextEditor.setDecorations(dec, decorationOptions);
        }
        
        const configHandler = VSCOBOLConfiguration.get();
        if (!configHandler.enable_comment_tags) {
            return;
        }

        const doc: TextDocument = activeTextEditor.document;
        const textLanguage: TextLanguage = VSExtensionUtils.isSupportedLanguage(doc);

        if (textLanguage !== TextLanguage.COBOL) {
            return;
        }

            
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
        }
    }
}

export const colourCommentHandler = new CommentColourHandlerImpl();

