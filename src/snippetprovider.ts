import { CancellationToken, CompletionContext, CompletionItem, CompletionItemKind, CompletionItemProvider, CompletionList, MarkdownString, Position, ProviderResult, Range, SnippetString, TextDocument } from "vscode";
import { COBOLSourceScanner, SourceScannerUtils } from "./cobolsourcescanner";
import { COBOLUtils, FoldAction, FoldStyle } from "./cobolutils";
import { ExtensionDefaults } from "./extensionDefaults";
import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";
import { KnownAPIs } from "./keywords/cobolCallTargets";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";

const jsonCRLF = "\r\n";

interface ISimpleSnippet {
    prefix: string;
    body: string[];
    description: string;
    scope: string;
}

const simpleSnippets: ISimpleSnippet[] = [
    {
        "prefix": "add",
        "body": [
            "add ${1:a} to ${2:b} giving ${3:c}"
        ],
        "description": "Add a to b giving c",
        "scope": "cobol"
    },
    {
        "prefix": "accept",
        "body": [
            "accept ${1:variable}",
            "$0"
        ],
        "scope": "cobol",
        "description": "Accept variable"
    },
    {
        "prefix": "accept",
        "body": [
            "accept ${1:variable} ${2|from date,from day,from day-of-week,time|}",
            "$0"
        ],
        "description": "accept from date/day/week/time",
        "scope": "cobol"
    }
];

export class SnippetCompletionItemProvider implements CompletionItemProvider {
    public static Default: SnippetCompletionItemProvider = new SnippetCompletionItemProvider()

    private keywordTargets = new Map<string, Map<string, CompletionItem>>();
    private allCallTargets = new Map<string, CompletionItem>();

    public reInitCallMap(settings: ICOBOLSettings):SnippetCompletionItemProvider {
        this.allCallTargets.clear();
        const callMap = KnownAPIs.getCallTargetMap();
        for (const [api,] of callMap) {
            const ci = this.getCompletionItemForAPI(settings, ExtensionDefaults.defaultCOBOLLanguage, api, "call");
            if (ci !== undefined) {
                this.allCallTargets.set(api, ci);
            }
        }

        for (const simpleSnippet of simpleSnippets) {
            this.addKeywordSnippet(settings, simpleSnippet, ExtensionDefaults.defaultCOBOLLanguage);
        }

        return this;
    }

    private addKeywordSnippet(settings: ICOBOLSettings, snippet: ISimpleSnippet, langId: string): void {
        if (!this.keywordTargets.has(snippet.prefix)) {
            this.keywordTargets.set(snippet.prefix, new Map<string, CompletionItem>());
        }

        const target = this.keywordTargets.get(snippet.prefix);
        if (target === undefined) {
            return;
        }

        let preselect = snippet.prefix;
        let kiSnippet = "";
        switch (settings.intellisense_style) {
            case intellisenseStyle.CamelCase:
                preselect = SourceScannerUtils.camelize(snippet.prefix);
                kiSnippet = this.foldKeywordLine(snippet.body, FoldStyle.CamelCase, langId);
                break;
            case intellisenseStyle.UpperCase:
                preselect = preselect.toUpperCase();
                kiSnippet = this.foldKeywordLine(snippet.body, FoldStyle.UpperCase, langId);
                break;
            case intellisenseStyle.LowerCase:
                preselect = preselect.toLowerCase();
                kiSnippet = this.foldKeywordLine(snippet.body, FoldStyle.LowerCase, langId);
                break;
            case intellisenseStyle.Unchanged:
                kiSnippet = snippet.body.join(jsonCRLF);
                break;

        }

        const ci = new CompletionItem(preselect);
        ci.keepWhitespace = false;
        ci.kind = CompletionItemKind.Keyword;
        ci.insertText = new SnippetString(kiSnippet);
        ci.preselect = true;
        ci.commitCharacters = [];
        ci.documentation = snippet.description;
        ci.detail = snippet.description;
        target.set(snippet.prefix, ci);
    }

    private foldKeywordLine(texts: string[], foldstyle: FoldStyle, languageid: string): string {
        const sb = [];
        for (const text of texts) {
            sb.push(COBOLUtils.foldTokenLine(text, undefined, FoldAction.Keywords, foldstyle, false, languageid));
        }

        return sb.join(jsonCRLF);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private getCompletionItemForAPI(settings: ICOBOLSettings, langId: string, api: string, keyword: string): CompletionItem | undefined {
        const ki = KnownAPIs.getCallTarget(api);
        if (ki === undefined) {
            return undefined;
        }
        let kiExample = "";
        let kiSnippet = "";
        let callStatement = keyword;

        switch (settings.intellisense_style) {
            case intellisenseStyle.CamelCase:
                kiSnippet = this.foldKeywordLine(ki.snippet, FoldStyle.CamelCase, langId);
                kiExample = this.foldKeywordLine(ki.example, FoldStyle.CamelCase, langId);
                callStatement = SourceScannerUtils.camelize(callStatement);
                break;
            case intellisenseStyle.UpperCase:
                kiSnippet = this.foldKeywordLine(ki.snippet, FoldStyle.UpperCase, langId);
                kiExample = this.foldKeywordLine(ki.example, FoldStyle.UpperCase, langId);
                callStatement = callStatement.toUpperCase();
                break;
            case intellisenseStyle.LowerCase:
                kiSnippet = this.foldKeywordLine(ki.snippet, FoldStyle.LowerCase, langId);
                kiExample = this.foldKeywordLine(ki.example, FoldStyle.LowerCase, langId);
                callStatement = callStatement.toLowerCase();
                break;
            case intellisenseStyle.Unchanged:
                kiSnippet = ki.snippet.join(jsonCRLF);
                kiExample = ki.example.join(jsonCRLF);
                break;
        }
        const preselect = `${callStatement} "${api}"`;
        const ci = new CompletionItem(preselect);
        ci.keepWhitespace = false;
        ci.kind = CompletionItemKind.Snippet;
        ci.insertText = new SnippetString(kiSnippet);
        ci.preselect = true;
        ci.filterText = preselect;
        ci.commitCharacters = [`${callStatement} "${api}"`];

        let documentation = ki === undefined ? "" : ki.description.join(jsonCRLF);

        if (ki !== undefined && ki.example.length !== 0) {
            documentation += `${jsonCRLF}${jsonCRLF}Example:${jsonCRLF}~~~${jsonCRLF}${kiExample}${jsonCRLF}~~~`;
        }

        ci.documentation = new MarkdownString(documentation);
        return ci;
    }

    private getSnippetForPreviousWord(document: TextDocument, position: Position, preWordLower: string) {
        const position_plus1 = new Position(position.line, position.character + 1);
        const position_plus1_char = document.getText(new Range(position, position_plus1));
        const snippets: CompletionItem[] = [];

        for (const [, ci] of this.allCallTargets) {
            if (ci.insertText !== undefined) {
                const line = document.lineAt(position.line);
                const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf(preWordLower);
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

    public getKeywordSnippet(word: string): CompletionItem[] {
        const snippets: CompletionItem[] = [];
        const targets = this.keywordTargets.get(word.toLowerCase());
        if (targets === undefined) {
            return snippets;
        }

        for (const [, ci] of targets) {
            if (ci.insertText !== undefined) {
                snippets.push(ci);
            }
        }
        return snippets;
    }


    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        const config = VSCOBOLConfiguration.get();
        if (config.snippets === false) {
            return [];
        }
        const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, config);
        if (qcp === undefined) {
            return [];
        }
        const wordRange = document.getWordRangeAtPosition(new Position(position.line, position.character - 2)); // 1 space -1
        if (!wordRange) return [];

        const position_plus1 = new Position(position.line, position.character + 1);
        const position_plus1_char = document.getText(new Range(position, position_plus1));
        const snippets: CompletionItem[] = [];
        const preWord = document.getText(wordRange);
        if (preWord !== undefined) {
            const preWordLower = preWord.toLowerCase();
            if (preWordLower === "call") {
                return this.getSnippetForPreviousWord(document, position, preWordLower);
            }

            const prevWordChar = position.character - preWord.length - 3; // 2 spaces -1
            if (prevWordChar >= 0) {
                const line = document.lineAt(position.line);
                const wordRangeForCall = document.getWordRangeAtPosition(new Position(position.line, prevWordChar));
                const pre2Word = document.getText(wordRangeForCall);
                if (pre2Word !== undefined && pre2Word.toLowerCase() === "call") {
                    const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf("call");
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
        }

        return snippets;
    }

}