import { ProviderResult } from "vscode";
import vscode from "vscode";
import { ICOBOLSettings, hoverApi } from "./iconfiguration";
import { CallTarget, KnownAPIs } from "./keywords/cobolCallTargets";
import { COBOLUtils } from "./cobolutils";
import { COBOLSourceScanner, COBOLVariable } from "./cobolsourcescanner";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { ESourceFormat } from "./externalfeatures";

const nhexRegEx = new RegExp("[nN][xX][\"'][0-9A-Fa-f]*[\"']");
const hexRegEx = new RegExp("[xX][\"'][0-9A-Fa-f]*[\"']");
const wordRegEx = new RegExp("[#0-9a-zA-Z][a-zA-Z0-9-_]*");

export class VSHoverProvider {

    public static provideHover(settings: ICOBOLSettings, document: vscode.TextDocument, position: vscode.Position): ProviderResult<vscode.Hover> {

        if (settings.hover_show_known_api !== hoverApi.Off) {
            const txt = document.getText(document.getWordRangeAtPosition(position, wordRegEx));
            const txtTarget: CallTarget | undefined = KnownAPIs.getCallTarget(txt);
            if (txtTarget !== undefined) {
                let example = txtTarget.example.length > 0 ? `\n\n---\n\n~~~\n${txtTarget.example.join("\r\n")}\n~~~\n` : "";
                if (settings.hover_show_known_api === hoverApi.Short) {
                    example = "";
                }
                return new vscode.Hover(`**${txtTarget.api}** - ${txtTarget.description}\n\n[\u2192 ${txtTarget.apiGroup}](${txtTarget.url})${example}`);
            }
        }

        if (settings.hover_show_encoded_literals) {
            const nxtxt = document.getText(document.getWordRangeAtPosition(position, nhexRegEx));
            if (nxtxt.toLowerCase().startsWith("nx\"") || nxtxt.toLowerCase().startsWith("nx'")) {
                const ascii = COBOLUtils.nxhex2a(nxtxt);
                if (ascii.length !== 0) {
                    return new vscode.Hover(`UTF16=${ascii}`);
                }
                return undefined;
            }
            const txt = document.getText(document.getWordRangeAtPosition(position, hexRegEx));
            if (txt.toLowerCase().startsWith("x\"") || txt.toLowerCase().startsWith("x'")) {
                const ascii = COBOLUtils.hex2a(txt);
                if (ascii.length !== 0) {
                    return new vscode.Hover(`ASCII=${ascii}`);
                }
            }
        }

        if (settings.hover_show_variable_definition) {
            const variableRegEx = new RegExp("[$#0-9a-zA-Z_][a-zA-Z0-9-_]*");
            const wordRange = document.getWordRangeAtPosition(position, variableRegEx);
            const word = wordRange ? document.getText(wordRange) : "";
            if (word === "") {
                return undefined;
            }
            const sf: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
            if (sf === undefined) {
                return undefined;
            }

            // only show when in the procedure division
            if (sf.sourceReferences.state.procedureDivision?.startLine !== undefined && position.line < sf.sourceReferences.state.procedureDivision?.startLine) {
                return undefined;
            }

            const tokenLower: string = word.toLowerCase();
            const variables: COBOLVariable[] | undefined = sf.constantsOrVariables.get(tokenLower);
            if (variables === undefined || variables.length === 0) {
                return undefined;
            }

            var hoverMessage = "";
            for (const variable of variables) {
                if (variable.token !== undefined) {
                    const token = variable.token;
                    var line = document.lineAt(token.startLine).text;
                    // not interested in margins
                    if (sf.sourceFormat === ESourceFormat.fixed) {
                        if (line.length > 72) {
                            line = line.substring(7, 72);
                        } else if (line.length > 7) {
                            line = line.substring(7,line.length);
                        }
                    }
                    hoverMessage += line.replace(/\s+/g, ' ').trim() + "\n\n";
                }
            }

            return new vscode.Hover(hoverMessage);
        }

        return undefined;
    }

}