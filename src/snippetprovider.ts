import { CancellationToken, CompletionContext, CompletionItem, CompletionItemKind, CompletionItemProvider, CompletionList, Position, ProviderResult, Range, SnippetString, TextDocument } from "vscode";
import { COBOLSourceScanner } from "./cobolsourcescanner";
import { KnownAPIs } from "./keywords/cobolCallTargets";

// const snippetMap = new Map<string, SnippetString>(
//     [
//         ["CBL_CLOSE_FILE", new SnippetString("call \"CBL_CLOSE_FILE\" using ${1:HANDLE}\n  returning ${2:STATUS-CODE}\nend-call\n$0")]
//     ]);




export class SnippetCompletionItemProvider implements CompletionItemProvider {

    private allCallTargets = new Map<string, CompletionItem>();

    constructor() {
        const callMap = KnownAPIs.getCallTargetMap();
        for (const [api,] of callMap) {
            const ci = this.getCompletionItemForAPI(api);
            if (ci !== undefined) {
                this.allCallTargets.set(api, ci);
            }
        }
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private getCompletionItemForAPI(api: string): CompletionItem | undefined {
        const ki = KnownAPIs.getCallTarget(api);
        const snippet = ki === undefined ? undefined : ki.snippet;
        if (snippet === undefined) {
            return snippet;
        }
        const preselect = `call "${api}"`;
        const ci = new CompletionItem(preselect);
        ci.keepWhitespace = false;
        ci.kind = CompletionItemKind.Snippet;
        ci.insertText = new SnippetString(snippet);
        ci.preselect = true;
        ci.filterText = preselect;
        ci.commitCharacters = [`call "${api}"`];

        if (ki !== undefined) {
            ci.documentation = ki.description;
        } else {
            ci.documentation = "";
        }

        if (ki !== undefined && ki.example.length !== 0) {
            ci.documentation += `\r\n\r\nExample:\r\n${ki.example}`;
        }
        return ci;
    }


    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {

        const wordRange = document.getWordRangeAtPosition(new Position(position.line, position.character - 2)); // 1 space -1
        if (!wordRange) return [];

        const position_plus1 = new Position(position.line, position.character + 1);
        const position_plus1_char = document.getText(new Range(position, position_plus1));
        const snippets: CompletionItem[] = [];
        const preWord = document.getText(wordRange);
        if (preWord !== undefined) {
            if (preWord.toUpperCase() === "CALL") {
                 for (const [, ci] of this.allCallTargets) {
                    if (ci.insertText !== undefined) {
                        const line = document.lineAt(position.line);
                        const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf("call");
                        if (position_plus1_char !== undefined && position_plus1_char === "\"") {
                            ci.range = new Range(new Position(position.line, charPosForCall), position_plus1);
                        } else {
                            ci.range = new Range(new Position(position.line, charPosForCall), position);
                        }
                        snippets.push(ci);
                    }
                }
                return snippets;
            }

            const prevWordChar = position.character - preWord.length - 3; // 2 spaces -1
            if (prevWordChar >= 0) {
                const line = document.lineAt(position.line);
                const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf("call");
                const wordRangeForCall = document.getWordRangeAtPosition(new Position(position.line, prevWordChar));
                const pre2Word = document.getText(wordRangeForCall);
                if (pre2Word.toUpperCase() !== "CALL") {
                    return snippets;
                }
                const preTrimmedWork = COBOLSourceScanner.trimLiteral(preWord);
                const preTrimmedWorkUpper = preTrimmedWork.toUpperCase();
                const ci = this.allCallTargets.get(preTrimmedWorkUpper);
                if (ci !== undefined) {
                    if (ci.insertText !== undefined) {
                        if (position_plus1_char !== undefined && position_plus1_char === "\"") {
                            ci.range = new Range(new Position(position.line, charPosForCall), position_plus1);
                        } else {
                            ci.range = new Range(new Position(position.line, charPosForCall), position);
                        }
                    }
                    snippets.push(ci);
                } else {
                    for (const [api, ci] of this.allCallTargets) {
                        if (ci.insertText !== undefined) {
                            if (api.startsWith(preTrimmedWorkUpper)) {
                                if (position_plus1_char !== undefined && position_plus1_char === "\"") {
                                    ci.range = new Range(new Position(position.line, charPosForCall), position_plus1);
                                } else {
                                    ci.range = new Range(new Position(position.line, charPosForCall), position);
                                }
                                snippets.push(ci);
                            }
                        }
                    }    
                }

            }
        }

        return snippets;
    }

}