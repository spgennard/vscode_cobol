import { DecorationOptions, DecorationRenderOptions, Position, Range, TextDocument, TextEditor, TextEditorDecorationType, ThemeColor, window, workspace } from "vscode";
import { ExtensionDefaults } from "./extensionDefaults";
import { ESourceFormat } from "./externalfeatures";
import { commentRange, ICommentCallback, ISourceHandlerLite } from "./isourcehandler";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { TextLanguage, VSExtensionUtils } from "./vsextutis";
import { VSLogger } from "./vslogger";
import { VSExternalFeatures } from "./vsexternalfeatures";

export class ColourTagHandler {

    public setupTags(configElement: string,tags: Map<string, TextEditorDecorationType>): void {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const items = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get(configElement) as any;
        if (items === undefined) {
            return;
        }

        tags.clear();

        for (const item of items) {
            try {
                const options: DecorationRenderOptions = {};

                if (item.color) {
                    options.color = item.color;
                }

                if (item.backgroundColor) {
                    options.backgroundColor = item.backgroundColor;
                }

                if (item.strikethrough) {
                    options.textDecoration = "line-through solid";
                }

                if (item.underline) {
                    options.textDecoration += " underline solid";
                }

                if (item.undercurl) {
                    options.textDecoration += " underline wavy";
                }

                if (item.bold) {
                    options.fontWeight = "bold";
                }

                if (item.italic) {
                    options.fontStyle = "italic";
                }

                if (item.reverse) {
                    options.backgroundColor = new ThemeColor("editor.foreground");
                    options.color = new ThemeColor("editor.background");
                }

                const decl = window.createTextEditorDecorationType(options);
                const tag = item.tag as string;
                tags.set(tag.toUpperCase(), decl);
            } catch (e) {
                VSLogger.logException("Invalid comments_tags entry", e as Error);
            }
        }
    }
}

class CommentColourHandlerImpl extends ColourTagHandler implements ICommentCallback {
    static readonly emptyCommentDecoration = window.createTextEditorDecorationType({
        //
    });

    private tags = new Map<string, TextEditorDecorationType>();

    constructor() {
        super();
        this.setupTags();
    }

    public setupTags() {
        super.setupTags("comments_tags",this.tags);
    }
 
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    processComment(sourceHandler: ISourceHandlerLite, commentLine: string, sourceFilename: string, sourceLineNumber: number, startPos: number, format: ESourceFormat): void {
        const configHandler = VSCOBOLConfiguration.get();
        if (!configHandler.enable_comment_tags) {
            return;
        }

        const ranges = sourceHandler.getNotedComments();
        const commentLineUpper = commentLine.toUpperCase();
        const comment_tag_word = configHandler.comment_tag_word;

        let lowestTag: commentRange | undefined = undefined;
        let lowestPos = commentLine.length;
        for (const [tag,] of this.tags) {
            const pos = commentLineUpper.indexOf(tag, 1 + startPos);

            if (pos !== -1) {
                if (pos < lowestPos) {
                    lowestPos = pos;
                    if (comment_tag_word) {
                        lowestTag = new commentRange(sourceLineNumber, pos, tag.length, tag);
                    } else {
                        lowestTag = new commentRange(sourceLineNumber, pos, commentLineUpper.length - startPos, tag);
                    }
                }
            }
        }

        // default to left most tag
        if (lowestTag !== undefined) {
            ranges.push(lowestTag);
        }
    }

    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
    public async updateDecorations(activeTextEditor: TextEditor | undefined) {
        if (!activeTextEditor) {
            return;
        }

        const configHandler = VSCOBOLConfiguration.get_using_textdocument(activeTextEditor.document, VSExternalFeatures);
        if (!configHandler.enable_comment_tags) {
            return;
        }

        // get rid of all decorations
        const decorationOptions: DecorationOptions[] = [];
        for (const [, dec] of this.tags) {
            activeTextEditor.setDecorations(dec, decorationOptions);
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
                    activeTextEditor.setDecorations(CommentColourHandlerImpl.emptyCommentDecoration, empty);
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

