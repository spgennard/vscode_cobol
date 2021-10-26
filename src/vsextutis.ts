import { ICOBOLSettings } from "./iconfiguration";
import * as vscode from 'vscode';

export class VSExtensionUtils {

    public static getAllCobolSelectors(config: ICOBOLSettings): vscode.DocumentSelector {
        const ret = [];

        for (const langid of config.valid_cobol_language_ids) {
            ret.push(
                { scheme: 'file', language: langid },
            );
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

    public static flip_plaintext(doc: vscode.TextDocument): void {
        if (doc === undefined) {
            return;
        }

        if (doc.languageId === 'plaintext' || doc.languageId === 'tsql') {  // one tsql ext grabs .lst!
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
    }
}
