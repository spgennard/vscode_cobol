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
    label: string;
    body: string[];
    description: string;
    scope: string;
}

const simpleSnippets: ISimpleSnippet[] = [
    {
        "prefix": "add",
        "label": "add ⦃⦄ to ⦃⦄ giving ⦃⦄",
        "body": [
            "add ${1:a} to ${2:b} giving ${3:c}",
            "$0"
        ],
        "description": "Add {identifier-1|literal-1} to {identifier-2} giving {identifier-3}",
        "scope": "cobol"
    },
    {
        "prefix": "accept",
        "label": "accept ⦃⦄",
        "body": [
            "accept ${1:variable}",
            "$0"
        ],
        "scope": "cobol",
        "description": "Accept identifier-1"
    },
    {
        "prefix": "accept",
        "label": "accept ⦃⦄ from ⦃date/time/..⦄",
        "body": [
            "accept ${1:variable} ${2|from date,from day,from day-of-week,from time|}",
            "$0"
        ],
        "description": "Accept identifier-1 from {date/day/week/time}",
        "scope": "cobol"
    },
    {
        "prefix": "copy",
        "label": "copy ⦃⦄ replacing ==⦃⦄== by ==⦃⦄==",
        "body": [
            "copy \"${1:subprog.cpy}\"",
            "    replacing ==${2:ABC}== by ==${3:DEF}==.",
        ],
        "description": "Copy {text-name-1|literal-1} replacing =={}== by =={}==",
        "scope": "cobol"
    },
    {
        "prefix": "divide",
        "label": "divide ⦃⦄ by ⦃⦄ giving ⦃⦄ [remainder ⦃⦄]",
        "body": [
            "divide ${1:a} by ${2:b} giving ${3:c} ${4:remainder ${5:d}}"
        ],
        "description": "divide a by b giving [remainder}",
        "scope": "cobol"
    }, {
        "prefix": "entry",
        "label": "entry ⦃literlal-1⦄",
        "body": [
            "${1:AlternateEP}-ep section.",
            "entry \"$1\".",
            "    $0\t",
            "    goback.",
            ""
        ],
        "description": "entry ⦃literlal-1⦄",
        "scope": "cobol"
    },
    {
        "prefix": "evaluate",
        "label": "evaluate ⦃⦄ .. when..",
        "body": [
            "evaluate ${1:Expression}",
            "    when ${2:Phrase}",
            "       $0",
            "    when other",
            "       continue",
            "end-evaluate",
            ""
        ],
        "description": "evaluate ⦃⦄.. when.. when continue",
        "scope": "cobol"
    },
    {
        "prefix": "if",
        "label": "if ⦃⦄ end-if",
        "body": [
            "if ${1:condition}",
            "   $0",
            "end-if"
        ],
        "description": "if ⦃⦄ end-if",
        "scope": "cobol"
    },
    {
        "prefix": "if",
        "label": "if ⦃⦄ else ⦃⦄  end-if",
        "body": [
            "if ${1:condition}",
            "   $2",
            "else",
            "   $0",
            "end-if"
        ],
        "description": "if ⦃⦄ else ⦃⦄  end-if",
        "scope": "cobol"
    },
    {
        "prefix": "delimited",
        "label": "delimited by size",
        "body": [
            "delimited by size"
        ],
        "description": "delimited by size (string)",
        "scope": "cobol"
    },
    {
        "prefix": "delimited",
        "label": "delimited by space",
        "body": [
            "delimited by space"
        ],
        "description": "delimited by size (string)",
        "scope": "cobol"
    }, {
        "prefix": "inspect",
        "label": "inspect converting",
        "body": [
            "inspect $1 converting '${2:ABCDEFGHIJKLMNOPQRSTUVWXYZ}' to '${3:abcdefghijklmnopqrstuvwxyz}'"
        ],
        "description": "inspect converting ⦃⦄ to ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "inspect",
        "label": "inspect replacing",
        "body": [
            "inspect $1 replacing all ${2|spaces,zero|} by '${3}'"
        ],
        "description": "inspect ⦃⦄ replacing [⦃⦄|spaces,zero] by ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "inspect",
        "label": "inspect tallying",
        "body": [
            "move 0 to ${2:counter}",
            "inspect ${1:source} tallying ${2:counter} for ${3|all spaces,all \"abc\",characters|}"
        ],
        "description": "Inspect ⦃⦄ tallying ⦃⦄ for [⦃⦄|all spaces,all ⦃⦄]]..",
        "scope": "cobol"
    },
    {
        "prefix": "linkage",
        "label": "linkage section.",
        "body": [
            "linkage section.",
            "$0"
        ],
        "description": "linkage section.",
        "scope": "cobol"
    },
    {
        "prefix": "procedure",
        "label": "procedure division",
        "body": [
            "procedure division.",
            "$0"
        ],
        "description": "procedure division",
        "scope": "cobol"
    },
    {
        "prefix": "program-id",
        "label": "program-id",
        "body": [
            "program-id. ${1:${TM_FILENAME/(.*)\\..+$/$1/}}.",
            "$0"
        ],
        "description": "program-id. literal-1",
        "scope": "cobol"
    },
    {
        "prefix": "local-storage",
        "label": "local-storage section",
        "description": "local-storage section",
        "body": [
            "local-storage section.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "working-storage",
        "label": "working-storage section",
        "description": "working-storage section",
        "body": [
            "working-storage section.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "data",
        "label": "data division",
        "description": "data division",
        "body": [
            "data division.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "environment",
        "label": "environment division",
        "description": "environment division",
        "body": [
            "environment division.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "identification",
        "label": "identification division",
        "description": "identification division",
        "body": [
            "identification division.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "cancel",
        "label": "cancel",
        "body": [
            "cancel \"$1\"",
            "$0"
        ],
        "description": "CANCEL literal",
        "scope": "cobol"
    },
    {
        "prefix": "call",
        "label": "call literal",
        "body": [
            "call \"$1\" using",
            "    by ${2|value,reference,content|} ${3:identifer}",
            "    returning ${4:return-code}",
            "end-call",
            "$0"
        ],
        "description": "CALL literal",
        "scope": "cobol"
    },
    {
        "prefix": "exit",
        "label": "exit program",
        "body": [
            "exit program returning ${1:item}"
        ],
        "description": "exit program returning ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "string",
        "label": "string",
        "body": [
            "string ${1:item1} delimited by size",
            "       ${2:item2} delimited by size",
            "       into ${3:result}",
            "end-string",
            "$0"
        ],
        "description": "string delimited by size",
        "scope": "cobol"
    },
    {
        "prefix": "subtract",
        "label": "subtract",
        "body": [
            "subtract ${1:a} from ${2:b} giving ${3:c}"
        ],
        "description": "subtract ⦃⦄ from ⦃⦄ giving ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "multiply",
        "label": "multiply",
        "body": [
            "multiply ${1:a} by ${2:b} giving ${3:c}"
        ],
        "description": "Multiply ⦃⦄ by ⦃⦄ giving ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform paragraph ⦃⦄ times",
        "body": [
            "perform ${1:paragraph-name} ${2:value-1} times"
        ],
        "description": "perform paragraph ⦃⦄ times",
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform .. end-perform",
        "body": [
            "perform ${1}",
            "\t${2}",
            "end-perform",
            "${0}"
        ],
        "description": "perform <block> end-perform",
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform paragraph varying",
        "body": [
            "perform ${1:paragraph-name} varying ${2:field-1}",
            " from ${3:value-1} by ${4:value-2}",
            " until ${5:condition}",
            "$0"
        ],
        "description": "perform paragraph varying",
        "scope": "cobol"
    },
    {
        "prefix": "end",
        "label": "end program",
        "body": [
            "end program ${1:${TM_FILENAME/(.*)\\..+$/$1/}}."
        ],
        "description": "end program literal.",
        "scope": "cobol"
    },
    {
        "prefix": "display",
        "label": "display",
        "body": [
            "display \"$0\""
        ],
        "description": "display literal",
        "scope": "cobol"
    }
];

class SnippetHelper {
    protected foldKeywordLine(texts: string[], foldstyle: FoldStyle, languageid: string): string {
        const sb = [];
        for (const text of texts) {
            sb.push(COBOLUtils.foldTokenLine(text, undefined, FoldAction.Keywords, foldstyle, false, languageid));
        }

        return sb.join(jsonCRLF);
    }
}

export class KeywordSnippetProvider extends SnippetHelper {

    public static Default: KeywordSnippetProvider = new KeywordSnippetProvider()

    private keywordTargets = new Map<string, Map<string, CompletionItem[]>>();

    public reInitKeyMap(settings: ICOBOLSettings): KeywordSnippetProvider {
        for (const simpleSnippet of simpleSnippets) {
            this.addKeywordSnippet(settings, simpleSnippet, ExtensionDefaults.defaultCOBOLLanguage);
        }

        return this;
    }

    private addKeywordSnippet(settings: ICOBOLSettings, snippet: ISimpleSnippet, langId: string): void {
        if (!this.keywordTargets.has(snippet.prefix)) {
            this.keywordTargets.set(snippet.prefix, new Map<string, CompletionItem[]>());
        }

        const target = this.keywordTargets.get(snippet.prefix);
        if (target === undefined) {
            return;
        }

        let preselect = snippet.label;
        let kiSnippet = "";
        switch (settings.intellisense_style) {
            case intellisenseStyle.CamelCase:
                preselect = SourceScannerUtils.camelize(snippet.label);
                kiSnippet = super.foldKeywordLine(snippet.body, FoldStyle.CamelCase, langId);
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
        ci.documentation = "";
        ci.detail = snippet.description;

        const cisU: CompletionItem[] | undefined = target.get(snippet.prefix);
        let cis: CompletionItem[];
        if (cisU === undefined) {
            cis = [];
            target.set(snippet.prefix, cis);
        } else {
            cis = cisU;
        }
        cis.push(ci);
        target.set(snippet.prefix, cis);
    }

    public getKeywordSnippet(word: string): CompletionItem[] {
        const snippets: CompletionItem[] = [];
        const targets = this.keywordTargets.get(word.toLowerCase());
        if (targets === undefined) {
            return snippets;
        }

        for (const [, cis] of targets) {
            for (const ci of cis) {
                if (ci.insertText !== undefined) {
                    snippets.push(ci);
                }
            }
        }
        return snippets;
    }
}

export class SnippetCompletionItemProvider extends SnippetHelper implements CompletionItemProvider {
    public static Default: SnippetCompletionItemProvider = new SnippetCompletionItemProvider()

    private allCallTargets = new Map<string, CompletionItem>();

    public reInitCallMap(settings: ICOBOLSettings): SnippetCompletionItemProvider {
        this.allCallTargets.clear();
        const callMap = KnownAPIs.getCallTargetMap();
        for (const [api,] of callMap) {
            const ci = this.getCompletionItemForAPI(settings, ExtensionDefaults.defaultCOBOLLanguage, api);
            if (ci !== undefined) {
                this.allCallTargets.set(api, ci);
            }
        }

        KeywordSnippetProvider.Default.reInitKeyMap(settings);

        return this;
    }


    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private getCompletionItemForAPI(settings: ICOBOLSettings, langId: string, api: string): CompletionItem | undefined {
        const keyword = "call";
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

    private getSnippetForPreviousWord(document: TextDocument, position: Position, prevWordLower: string,  targets: Map<string, CompletionItem>) {
        const position_plus1 = new Position(position.line, position.character + 1);
        const position_plus1_char = document.getText(new Range(position, position_plus1));
        const snippets: CompletionItem[] = [];

        for (const [, ci] of targets) {
            if (ci.insertText !== undefined) {
                const line = document.lineAt(position.line);
                const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf(prevWordLower);
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




    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        const config = VSCOBOLConfiguration.get();
        if (config.snippets === false) {
            return [];
        }
        const currentLine: string = document.lineAt(position.line).text;
        if (currentLine.endsWith(" ")) {
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
            const prevWordLower = preWord.toLowerCase();
            if (prevWordLower === "call") {
                return this.getSnippetForPreviousWord(document, position, prevWordLower, this.allCallTargets);
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