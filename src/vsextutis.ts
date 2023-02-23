import { ICOBOLSettings } from "./iconfiguration";
import * as vscode from "vscode";

export enum TextLanguage {
    Unknown = 0,
    COBOL = 1,
    JCL = 2
}

export class VSExtensionUtils {

    public static isSupportedLanguage(document: vscode.TextDocument): TextLanguage {

        switch (document.languageId.toLowerCase()) {
            case "cobolit":
            case "bitlang-cobol":
            case "cobol":
            case "acucobol":
                return TextLanguage.COBOL;
            case "jcl":
                return TextLanguage.JCL;
        }

        /* not a supported language? */
        return TextLanguage.Unknown;
    }

    private static readonly knownSchemes: string[] = [
        "file",
        "ftp",
        "git",
        "member",
        "sftp",
        "ssh",
        "streamfile",
        "untitled",
        "vscode-vfs",
        "zip"
    ];

    public static isKnownScheme(scheme: string): boolean {
        for (const kscheme of VSExtensionUtils.knownSchemes) {
            if (scheme === kscheme) {
                return true;
            }
        }
        return false;
    }

    public static getAllCobolSelectors(config: ICOBOLSettings): vscode.DocumentSelector {
        const ret = [];

        for (const langid of config.valid_cobol_language_ids) {
            for (const kscheme of VSExtensionUtils.knownSchemes) {
                ret.push(
                    { scheme: kscheme, language: langid },
                )
            }
        }

        return ret;
    }


    public static getAllCobolSelector(langid: string): vscode.DocumentSelector {
        const ret = [];

        for (const kscheme of VSExtensionUtils.knownSchemes) {
            ret.push(
                { scheme: kscheme, language: langid },
            )
        }

        return ret;
    }
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public static getAllJCLSelectors(config: ICOBOLSettings): vscode.DocumentSelector {
        const ret = [];

        for (const kscheme of VSExtensionUtils.knownSchemes) {
            ret.push(
                { scheme: kscheme, language: "JCL" },
            )
        }

        return ret;
    }

    public static isKnownCOBOLLanguageId(config: ICOBOLSettings, possibleLangid: string): boolean {
        for (const langid of config.valid_cobol_language_ids) {
            if (possibleLangid === langid) {
                return true;
            }
        }

        return false;
    }

    private static readonly bmsColumnNumber = /^([0-9][0-9][0-9][0-9][0-9][0-9]).*$/g;

    public static flip_plaintext(doc: vscode.TextDocument): void {
        if (doc === undefined) {
            return;
        }

        // flip to ACUCOBOL if the source file looks like it a Acubench generated file
        if (doc.languageId === "COBOL") {
            const lineCount = doc.lineCount;
            if (lineCount >= 3) {
                const firstLine = doc.lineAt((0)).text;
                const secondLine = doc.lineAt(1).text;

                if ((firstLine.indexOf("*{Bench}") !== -1) || ((secondLine.indexOf("*{Bench}") !== -1))) {
                    vscode.languages.setTextDocumentLanguage(doc, "ACUCOBOL");
                    return;
                }
            }
        }

        if (doc.languageId === "plaintext" || doc.languageId === "tsql") {  // one tsql ext grabs .lst!
            const lineCount = doc.lineCount;
            if (lineCount >= 3) {
                const firstLine = doc.lineAt((0)).text;
                const secondLine = doc.lineAt(1).text;

                if ((firstLine.length >= 1 && firstLine.charCodeAt(0) === 12) && secondLine.startsWith("* Micro Focus COBOL ")) {
                    vscode.languages.setTextDocumentLanguage(doc, "COBOL_MF_LISTFILE");
                    return;
                }

                //NOTE: If we have more.. refactor..
                if (firstLine.startsWith("Pro*COBOL: Release")) {
                    vscode.languages.setTextDocumentLanguage(doc, "COBOL_PCOB_LISTFILE");
                    return;
                }

                if ((firstLine.indexOf("ACUCOBOL-GT ") !== -1) && (firstLine.indexOf("Page:") !== -1)) {
                    vscode.languages.setTextDocumentLanguage(doc, "COBOL_ACU_LISTFILE");
                    return;
                }
            }
        }

        if (doc.uri.fsPath.endsWith(".map") && !doc.languageId.startsWith("bms")) {
            const maxLines = doc.lineCount < 10 ? doc.lineCount : 10;
            for (let lcount = 1; lcount <= maxLines; lcount++) {
                const qline = doc.lineAt(lcount).text;
                if (qline.indexOf("DFHMSD") !== -1) {
                    if (qline.match(VSExtensionUtils.bmsColumnNumber)) {
                        vscode.languages.setTextDocumentLanguage(doc, "bmsmap");
                        return;
                    }
                    vscode.languages.setTextDocumentLanguage(doc, "bms");
                    return;
                }
            }
        }
    }


}
