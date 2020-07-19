'use strict';

import { DecorationOptions, Range, TextEditor, Position, window, ThemeColor, TextDocument, workspace, TextEditorDecorationType, ExtensionContext } from 'vscode';
import { getCurrentContext, enableMarginStatusBar, hideMarginStatusBar } from './extension';
import minimatch from 'minimatch';
import { ICOBOLSettings } from './iconfiguration';
import { VSCOBOLConfiguration } from './configuration';
import ISourceHandler from './isourcehandler';

var trailingSpacesDecoration: TextEditorDecorationType = window.createTextEditorDecorationType({
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
    let currentContext: ExtensionContext = getCurrentContext();
    let enabledViaContext: boolean | undefined = currentContext.workspaceState.get<boolean>(configString);

    // if this is enabled via the context, then return it early
    if (enabledViaContext !== undefined && enabledViaContext !== null && enabledViaContext === true) {
        return true;
    }

    let editorConfig = workspace.getConfiguration(configString);
    let marginOn = editorConfig.get<boolean>('margin');
    if (marginOn !== undefined) {
        return marginOn;
    }
    return true;
}

export function enableMarginCobolMargin(enabled: boolean) {
    let configString: string = "coboleditor";
    let currentContext: ExtensionContext = getCurrentContext();

    currentContext.workspaceState.update(configString, enabled);
}

export function isEnabledViaWorkspace4cobol(): boolean {
    return isMarginEnabled('coboleditor');
}

function isEnabledViaWorkspace4jcl(): boolean {
    return isMarginEnabled('jcleditor');
}

export enum ESourceFormat {
    unknown = 'unknown',
    fixed = 'fixed',
    free = 'free',
    terminal = 'terminal',
    variable = 'variable',
    jcl = 'jcl'
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
    let editorConfig = workspace.getConfiguration('coboleditor');
    let files: IEditorMarginFiles[] | undefined = editorConfig.get<IEditorMarginFiles[]>("fileformat");
    if (files === undefined || files === null) {
        return [];
    }

    return files;
}

const inline_sourceformat: string[] = ['sourceformat', '>>source format'];

export function getCOBOLSourceFormat(doc: ISourceHandler, config:ICOBOLSettings) : ESourceFormat {
    let langid = "";
    if (window.activeTextEditor !== undefined) {
        langid = window.activeTextEditor.document.languageId.toLowerCase();
    }

    if (config.fileformat_strategy === "always_fixed") {
        return ESourceFormat.fixed;
    }

    let linesWithJustNumbers = 0;
    let maxLines = doc.getLineCount() > 10 ? 10 : doc.getLineCount();
    let defFormat = ESourceFormat.unknown;

    let checkForTerminalFormat: boolean = langid === 'acucobol' ? true : false;

    for (let i = 0; i < maxLines; i++) {
        let lineText = doc.getLine(i);
        let line = lineText.toLocaleLowerCase();

        // acu
        if (defFormat === ESourceFormat.unknown && checkForTerminalFormat) {
            if (line.startsWith("*") || line.startsWith("|") || line.startsWith("\\D")) {
                defFormat = ESourceFormat.terminal;
            }
        }

        // non-acu
        if (defFormat === ESourceFormat.unknown && !checkForTerminalFormat) {
            let newcommentPos = line.indexOf("*>");
            if (newcommentPos !== -1 && defFormat === ESourceFormat.unknown) {
                defFormat = ESourceFormat.variable;
            }
        }

        let pos4sourceformat_after = 0;
        for (let isf = 0; isf < inline_sourceformat.length; isf++) {
            let pos4sourceformat = line.indexOf(inline_sourceformat[isf]);
            if (pos4sourceformat !== -1) {
                pos4sourceformat_after = pos4sourceformat + inline_sourceformat[isf].length + 1;
                break;
            }
        }

        // does it contain a inline comments? no
        if (pos4sourceformat_after === 0) {
            if (line.length > 72) {
                let rightMargin = line.substr(72).trim();
                if (isNumber(rightMargin)) {
                    linesWithJustNumbers++;
                }
            }
            continue;
        } else {
            // got a inline comment,yes
            let line2right = line.substr(pos4sourceformat_after);

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

    //it might well be...
    if (linesWithJustNumbers > 7) {
        return ESourceFormat.fixed;
    }

    let filesFilter = getFixedFilenameConfiguration();
    if (filesFilter.length >= 1) {
        let docFilename: string = doc.getFilename();
        for (let i = 0; i < filesFilter.length; i++) {
            let filter: IEditorMarginFiles = filesFilter[i];

            if (minimatch(docFilename, filter.pattern, { nocase: true })) {
                return ESourceFormat[filter.sourceformat];
            }
        }
    }

    return defFormat;

}
export function getSourceFormat(doc: TextDocument, config: ICOBOLSettings): ESourceFormat {
    let langid = doc.languageId.toLowerCase();

    /* just use the extension for jcl */
    switch (langid) {
        case "jcl":
        case "job":
        case "cntl":
        case "prc":
        case "proc":
            return ESourceFormat.jcl;
    }

    if (config.fileformat_strategy === "always_fixed") {
        return ESourceFormat.fixed;
    }

    let linesWithJustNumbers = 0;
    let maxLines = doc.lineCount > 10 ? 10 : doc.lineCount;
    let defFormat = ESourceFormat.unknown;
    let checkForTerminalFormat: boolean = langid === 'acucobol' ? true : false;

    for (let i = 0; i < maxLines; i++) {
        let lineText = doc.lineAt(i);
        let line = lineText.text.toLocaleLowerCase();

        // acu
        if (defFormat === ESourceFormat.unknown && checkForTerminalFormat) {
            if (line.startsWith("*") || line.startsWith("|") || line.startsWith("\\D")) {
                defFormat = ESourceFormat.terminal;
            }
        }

        // non-acu
        if (defFormat === ESourceFormat.unknown && !checkForTerminalFormat) {
            let newcommentPos = line.indexOf("*>");
            if (newcommentPos !== -1 && defFormat === ESourceFormat.unknown) {
                defFormat = ESourceFormat.variable;
            }
        }

        let pos4sourceformat_after = 0;
        for (let isf = 0; isf < inline_sourceformat.length; isf++) {
            let pos4sourceformat = line.indexOf(inline_sourceformat[isf]);
            if (pos4sourceformat !== -1) {
                pos4sourceformat_after = pos4sourceformat + inline_sourceformat[isf].length + 1;
                break;
            }
        }

        // does it contain a inline comments? no
        if (pos4sourceformat_after === 0) {
            if (line.length > 72) {
                let rightMargin = line.substr(72).trim();
                if (isNumber(rightMargin)) {
                    linesWithJustNumbers++;
                }
            }
            continue;
        } else {
            // got a inline comment,yes
            let line2right = line.substr(pos4sourceformat_after);

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

    //it might well be...
    if (linesWithJustNumbers > 7) {
        return ESourceFormat.fixed;
    }

    let filesFilter = getFixedFilenameConfiguration();
    if (filesFilter.length >= 1) {
        let docFilename: string = doc.fileName;
        for (let i = 0; i < filesFilter.length; i++) {
            let filter: IEditorMarginFiles = filesFilter[i];

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
        case "entcobol":
        case "cobol":
        case "opencobol":
        case "gnucobol":
        case "acucobol":
            return TextLanguage.COBOL;
        case "jcl":
            return TextLanguage.JCL;
    }

    /* not a supported language? */
    return TextLanguage.Unknown;
}

export default function updateDecorations(activeTextEditor: TextEditor | undefined) {
    if (!activeTextEditor) {
        hideMarginStatusBar();
        return;
    }

    const doc: TextDocument = activeTextEditor.document;
    const decorationOptions: DecorationOptions[] = [];

    const textLanguage: TextLanguage = isSupportedLanguage(doc);

    if (textLanguage === TextLanguage.Unknown) {
        hideMarginStatusBar();
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    let enabledViaWorkspace4cobol: boolean = isEnabledViaWorkspace4cobol();

    /* is it enabled? (COBOL) */
    if (textLanguage === TextLanguage.COBOL && !enabledViaWorkspace4cobol) {
        hideMarginStatusBar();
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    let enabledViaWorkspace4jcl: boolean = isEnabledViaWorkspace4jcl();

    /* is it enabled? (COBOL) */
    if (textLanguage === TextLanguage.JCL && !enabledViaWorkspace4jcl) {
        hideMarginStatusBar();
        activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
        return;
    }

    if (textLanguage === TextLanguage.COBOL) {
        /* does it include sourceformat"free"? */
        let sourceformatStyle: ESourceFormat = getSourceFormat(doc, VSCOBOLConfiguration.get());
        enableMarginStatusBar(sourceformatStyle);

        if (enabledViaWorkspace4cobol) {
            sourceformatStyle = ESourceFormat.fixed;
        } else {
            switch (sourceformatStyle) {
                case ESourceFormat.free:
                case ESourceFormat.variable:
                case ESourceFormat.unknown:
                    activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
                    return;
            }
        }

        for (let i = 0; i < doc.lineCount; i++) {
            let lineText = doc.lineAt(i);
            let line = lineText.text;

            if (sourceformatStyle === ESourceFormat.fixed) {
                if (line.length > 6) {
                    let startPos = new Position(i, 0);
                    let endPos = new Position(i, 6);
                    const decoration = { range: new Range(startPos, endPos) };
                    decorationOptions.push(decoration);
                }
            }

            if (line.length > 72) {
                let startPos = new Position(i, 72);
                let endPos = new Position(i, line.length);
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }
        }
    }

    if (textLanguage === TextLanguage.JCL) {
        for (let i = 0; i < doc.lineCount; i++) {
            let lineText = doc.lineAt(i);
            let line = lineText.text;

            if (line.length > 72) {
                let startPos = new Position(i, 72);
                let endPos = new Position(i, line.length);
                const decoration = { range: new Range(startPos, endPos) };
                decorationOptions.push(decoration);
            }
        }
    }
    activeTextEditor.setDecorations(trailingSpacesDecoration, decorationOptions);
}
