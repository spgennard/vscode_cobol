import { ProviderResult } from "vscode";
import vscode from "vscode";
import { ICOBOLSettings, hoverApi } from "./iconfiguration";
import { CallTarget, KnownAPIs } from "./keywords/cobolCallTargets";
import { VSCOBOLUtils } from "./vscobolutils";
import { COBOLToken, COBOLVariable, SQLDeclare } from "./cobolsourcescanner";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { ICOBOLSourceScanner } from "./icobolsourcescanner";
// import { ESourceFormat } from "./externalfeatures";

const nhexRegEx = new RegExp("[nN][xX][\"'][0-9A-Fa-f]*[\"']");
const hexRegEx = new RegExp("[xX][\"'][0-9A-Fa-f]*[\"']");
const wordRegEx = new RegExp("[#0-9a-zA-Z][a-zA-Z0-9-_$]*");
const variableRegEx = new RegExp("[$#0-9a-zA-Z_][a-zA-Z0-9-_]*");

export class VSHoverProvider {

    private static wrapCommentAndCode(comment: string, code: string): string {
        let cleanCode = code;
        if (code.endsWith("\n")) {
            cleanCode = code.slice(0, -1)
        }

        if (comment.trim().length === 0) {
            // return just the code
            return `\`\`\`COBOL
${cleanCode}
\`\`\`
`;
        }

        // return both the comment and the code
        return `\`\`\`text
${comment}
\`\`\`

\`\`\`COBOL
${cleanCode}
\`\`\`

`;
    }

    public static provideHover(settings: ICOBOLSettings, document: vscode.TextDocument, position: vscode.Position): ProviderResult<vscode.Hover> {
        if (settings.hover_show_known_api !== hoverApi.Off) {
            const txt = document.getText(document.getWordRangeAtPosition(position, wordRegEx));
            const txtTarget: CallTarget | undefined = KnownAPIs.getCallTarget(document.languageId, txt);
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
                const ascii = VSCOBOLUtils.nxhex2a(nxtxt);
                if (ascii.length !== 0) {
                    return new vscode.Hover(`UTF16=${ascii}`);
                }
                return undefined;
            }
            const txt = document.getText(document.getWordRangeAtPosition(position, hexRegEx));
            if (txt.toLowerCase().startsWith("x\"") || txt.toLowerCase().startsWith("x'")) {
                const ascii = VSCOBOLUtils.hex2a(txt);
                if (ascii.length !== 0) {
                    return new vscode.Hover(`ASCII=${ascii}`);
                }
            }
        }

        const wordRange = document.getWordRangeAtPosition(position, variableRegEx);
        const word = document.getText(wordRange);
        if (word === undefined || word.length === 0) {
            return undefined;
        }
        const sf: ICOBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (sf === undefined) {
            return undefined;
        }


        let inProcedureDivision = false;
        if (sf.sourceReferences.state.procedureDivision?.startLine !== undefined && position.line > sf.sourceReferences.state.procedureDivision?.startLine) {
            inProcedureDivision = true;
        }

        if (settings.hover_show_variable_definition) {
            // only show when in the procedure division
            if (sf.sourceReferences.state.procedureDivision?.startLine !== undefined && position.line < sf.sourceReferences.state.procedureDivision?.startLine) {
                return undefined;
            }

            const tokenLower: string = word.toLowerCase();
            const variables: COBOLVariable[] | undefined = sf.constantsOrVariables.get(tokenLower);
            if (variables !== undefined && variables.length !== 0) {
                let hoverMessage = "";
                for (const variable of variables) {
                    if (variable.token !== undefined) {
                        const token = variable.token;
                        let line = token.sourceHandler.getLine(token.startLine, false);
                        if (line === undefined) {
                            continue;
                        }

                        let newHoverMessage = line.trimEnd();
                        if (variables.length === 1) {
                            newHoverMessage = newHoverMessage.trimStart();
                        }
                        let commentLine = token.sourceHandler.getCommentAtLine(token.startLine);
                        if (hoverMessage.length !== 0) {
                            hoverMessage += "\n\n----\n\n";
                        }
                        hoverMessage += VSHoverProvider.wrapCommentAndCode(commentLine, newHoverMessage);
                    }
                }

                if (hoverMessage.length !== 0) {
                    let md = new vscode.MarkdownString(hoverMessage);
                    return new vscode.Hover(md);
                }
            }
        }

        const showSection = true;
        if (showSection && inProcedureDivision) {
            const tokenLower: string = word.toLowerCase();
            const token: COBOLToken | undefined = sf.sections.get(tokenLower);
            if (token !== undefined && token.startLine !== position.line) {
                let line = token.sourceHandler.getLine(token.startLine, false);
                if (line !== undefined) {
                    let newHoverMessage = line.trimEnd();
                    let sectionsCommentLine = token.sourceHandler.getCommentAtLine(token.startLine);
                    let sectionsHoverMessage = VSHoverProvider.wrapCommentAndCode(sectionsCommentLine, newHoverMessage);

                    if (sectionsHoverMessage.length !== 0) {
                        let md = new vscode.MarkdownString(sectionsHoverMessage);
                        return new vscode.Hover(md);
                    }
                }
            }
        }

        if (showSection && inProcedureDivision) {
            const tokenLower: string = word.toLowerCase();
            const token: COBOLToken | undefined = sf.paragraphs.get(tokenLower);
            if (token !== undefined && token.startLine !== position.line) {
                let line = token.sourceHandler.getLine(token.startLine, false);
                if (line !== undefined) {
                    let newHoverMessage = line.trimEnd();
                    let paragraphCommentLine = token.sourceHandler.getCommentAtLine(token.startLine);
                    let paragraphHoverMessage = VSHoverProvider.wrapCommentAndCode(paragraphCommentLine, newHoverMessage);

                    if (paragraphHoverMessage.length !== 0) {
                        let md = new vscode.MarkdownString(paragraphHoverMessage);
                        return new vscode.Hover(md);
                    }
                }
            }
        }

        if (settings.enable_exec_sql_cursors) {
            const tokenLower: string = word.toLowerCase();
            const sqlToken: SQLDeclare | undefined = sf.execSQLDeclare.get(tokenLower);
            if (sqlToken !== undefined) {
                const token = sqlToken.token;
                if (token !== undefined && token.startLine !== position.line) {
                    let sc = token.rangeStartLine === token.rangeEndLine ? token.rangeStartColumn : 0;
                    let lines = token.sourceHandler.getText(token.rangeStartLine, sc, token.rangeEndLine, token.rangeEndColumn);
                    if (lines !== undefined) {
                        let newHoverMessage = lines.trimEnd();
                        let sqldeclareComment = token.sourceHandler.getCommentAtLine(token.startLine);
                        let sqldeclareHoverMarkdown = VSHoverProvider.wrapCommentAndCode(sqldeclareComment, newHoverMessage);

                        if (sqldeclareHoverMarkdown.length !== 0) {
                            let md = new vscode.MarkdownString(sqldeclareHoverMarkdown);
                            return new vscode.Hover(md);
                        }
                    }
                }
            }
        }
        return undefined;
    }


}