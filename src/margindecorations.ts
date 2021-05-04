'use strict';

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEditorDecorationType, ExtensionContext } from 'vscode';
import { getCurrentContext } from './extension';
import minimatch from 'minimatch';
import { ICOBOLSettings } from './iconfiguration';
import { VSCOBOLConfiguration } from './configuration';
import ISourceHandler from './isourcehandler';
import { ESourceFormat } from './externalfeatures';
import VSCOBOLSourceScanner from './vscobolscanner';

const trailingSpacesDecoration: TextEditorDecorationType = window.createTextEditorDecorationType({
    light: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)",
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
        textDecoration: 'solid'
    },
    dark: {
        // backgroundColor: "rgba(255,0,0,1)",
        // color: "rgba(0,0,0,1)"
        color: new ThemeColor("editorLineNumber.foreground"),
        backgroundColor: new ThemeColor("editor.background"),
        textDecoration: 'solid'
    }

});

function isMarginEnabled(configString: string): boolean {
    const currentContext: ExtensionContext = getCurrentContext();
    const enabledViaContext: boolean | undefined = currentContext.workspaceState.get<boolean>(configString);

    // if this is enabled via the context, then return it early
    if (enabledViaContext !== undefined && enabledViaContext !== null && enabledViaContext === true) {
        return true;
    }

    const editorConfig = workspace.getConfiguration(configString);
    const marginOn = editorConfig.get<boolean>('margin');
    if (marginOn !== undefined) {
        return marginOn;
    }
    return true;
}

export function enableMarginCobolMargin(enabled: boolean): void {
    const configString = "coboleditor";
    const currentContext: ExtensionContext = getCurrentContext();

    currentContext.workspaceState.update(configString, enabled);
}

export function isEnabledViaWorkspace4cobol(): boolean {
    return isMarginEnabled('coboleditor');
}

function isEnabledViaWorkspace4jcl(): boolean {
    return isMarginEnabled('jcleditor');
}


export const sourceformatMessages: string[] = ['unknown', 'fixed', 'free', 'variable'];

interface IEditorMarginFiles {
    pattern: string;
    sourceformat: ESourceFormat;
}

function isNumber(value: string | number): boolean {
    if (value.toString().length === 0) {
        return false;
    }
    return !isNaN(Number(value.toString()));
}

function getFixedFilenameConfiguration(): IEditorMarginFiles[] {
    const editorConfig = workspace.getConfiguration('coboleditor');
    const files: IEditorMarginFiles[] | undefined = editorConfig.get<IEditorMarginFiles[]>("fileformat");
    if (files === undefined || files === null) {
        return [];
    }

    return files;
}

const inline_sourceformat: string[] = ['sourceformat', '>>source format'];

function isValidFixedLine(line: string): boolean {
    if (line.length > 7) {
        switch (line[6]) {
            case '*': return true;
            case 'D': return true;
            case '/': return true;
            case ' ': return true;
            case '-': return true;
        }
    }

    return false;
}

export function getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat {

    if (config.fileformat_strategy === "always_fixed") {
        return ESourceFormat.fixed;
    }

    let linesWithJustNumbers = 0;
    let linesWithIdenticalAreaB = 0;
    const maxLines = doc.getLineCount() > 10 ? 10 : doc.getLineCount();
    let defFormat = ESourceFormat.unknown;

    let langid = "";
    if (window.activeTextEditor !== undefined) {
        langid = window.activeTextEditor.document.languageId.toLowerCase();
    }

    const checkForTerminalFormat: boolean = langid === 'acucobol' ? true : false;
    let prevRightMargin = "";
    let validFixedLines = 0;
    for (let i = 0; i < maxLines; i++) {

        const lineText = doc.getLine(i, true);
        if (lineText === undefined) {
            break;
        }

        const line = lineText.toLowerCase();
        const validFixedLine = isValidFixedLine(line);
        if (validFixedLine) {
            validFixedLines++;
        }
        // acu
        if (defFormat === ESourceFormat.unknown && checkForTerminalFormat) {
            if (line.startsWith("*") || line.startsWith("|") || line.startsWith("\\D")) {
                defFormat = ESourceFormat.terminal;
            }
        }

        // non-acu
        if (defFormat === ESourceFormat.unknown && !checkForTerminalFormat) {
            const newcommentPos = line.indexOf("*>");
            if (newcommentPos !== -1 && defFormat === ESourceFormat.unknown) {
                defFormat = ESourceFormat.variable;
            }
        }

        let pos4sourceformat_after = 0;
        for (let isf = 0; isf < inline_sourceformat.length; isf++) {
            const pos4sourceformat = line.indexOf(inline_sourceformat[isf]);
            if (pos4sourceformat !== -1) {
                pos4sourceformat_after = pos4sourceformat + inline_sourceformat[isf].length + 1;
                break;
            }
        }

        // does it contain a inline comments? no
        if (pos4sourceformat_after === 0) {
            if (line.length > 80) {
                defFormat = ESourceFormat.variable;
                continue;
            } else {
                if (isValidFixedLine(line)) {
                    if (line.length > 72) {
                        const rightMargin = line.substr(72).trim();

                        if (prevRightMargin === rightMargin) {
                            linesWithIdenticalAreaB++;
                        } else {
                            if (isNumber(rightMargin)) {
                                linesWithJustNumbers++;
                            }
                        }

                        prevRightMargin = rightMargin;
                    }
                } else {
                    // if we cannot be sure, then let the default be variable
                    defFormat = ESourceFormat.variable;
                }
            }
            continue;
        } else {
            // got a inline comment,yes
            const line2right = line.substr(pos4sourceformat_after);

            if (line2right.indexOf("fixed") !== -1) {
                return ESourceFormat.fixed;
            }
            if (line2right.indexOf("variable") !== -1) {
                return ESourceFormat.variable;
            }
            if (line2right.indexOf("free") !== -1) {
                return ESourceFormat.free;
            }
        }
    }

    if (validFixedLines == maxLines) {
        return ESourceFormat.fixed;
    }

    //it might well be...
    if (linesWithJustNumbers > 7 || linesWithIdenticalAreaB > 7) {
        return ESourceFormat.fixed;
    }

    const filesFilter = getFixedFilenameConfiguration();
    if (filesFilter.length >= 1) {
        const docFilename: string = doc.getFilename();
        for (let i = 0; i < filesFilter.length; i++) {
            const filter: IEditorMarginFiles = filesFilter[i];

            if (minimatch(docFilename, filter.pattern, { nocase: true })) {
                return ESourceFormat[filter.sourceformat];
            }
        }
    }

    return defFormat;
}


export enum TextLanguage {
    Unknown = 0,
    COBOL = 1,
    JCL = 2
}

export function isSupportedLanguage(document: TextDocument): TextLanguage {

    switch (document.languageId.toLowerCase()) {
        case "cobolit":
        case "cobol":
        case "acucobol":
            return TextLanguage.COBOL;
        case "jcl":
            return TextLanguage.JCL;
    }

    /* not a supported language? */
    return TextLanguage.Unknown;
}

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export default async function updateDecorations(activeTextEditor: TextEditor | undefined) {
    if (!activeTextEditor) {
        return;
    }

    const doc: TextDocument = activeTextEditor.document;
    const decorationOptions: DecorationOptions[] = [];

    const textLanguage: TextLanguage = isSupportedLanguage(doc);

    if (textLanguage === TextLanguage.Unknown) {
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    /* is it enabled? (COBOL) */
    if (textLanguage === TextLanguage.COBOL && !isEnabledViaWorkspace4cobol) {
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    /* is it enabled? (COBOL) */
    if (textLanguage === TextLanguage.JCL && !isEnabledViaWorkspace4jcl) {
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    if (textLanguage === TextLanguage.COBOL) {
        const configHandler = VSCOBOLConfiguration.get();
        const gcp = VSCOBOLSourceScanner.getCachedObject(doc, configHandler);
        if (gcp === undefined) {
            activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
            return;
        }

        /* does it include sourceformat"free"? */
        const sourceformatStyle: ESourceFormat = gcp.sourceFormat;
        switch (sourceformatStyle) {
            case ESourceFormat.free:
            case ESourceFormat.variable:
            case ESourceFormat.unknown:
                activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
                return;
        }

        const lineLimit = configHandler.editor_maxTokenizationLineLength;
        const maxLinesInFile = doc.lineCount;
        let maxLines = maxLinesInFile;
        if (maxLines > lineLimit) {
            maxLines = lineLimit;
        }

        for (let i = 0; i < maxLines; i++) {
            const lineText = doc.lineAt(i);
            const line = lineText.text;

            if (sourceformatStyle === ESourceFormat.fixed) {
                if (line.length > 6) {
                    const startPos = new Position(i, 0);
                    const endPos = new Position(i, 6);
                    const decoration = { range: new Range(startPos, endPos) };
                    decorationOptions.push(decoration);
                }
            }

            if (line.length > 72) {
                const startPos = new Position(i, 72);
                const endPos = new Position(i, line.length);
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }
        }
    }

    if (textLanguage === TextLanguage.JCL) {
        for (let i = 0; i < doc.lineCount; i++) {
            const lineText = doc.lineAt(i);
            const line = lineText.text;

            if (line.length > 72) {
                const startPos = new Position(i, 72);
                const endPos = new Position(i, line.length);
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }
        }
    }
    activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
}
