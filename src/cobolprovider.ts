import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from 'vscode';
import VSQuickCOBOLParse from './vscobolquickparse';
import { ICOBOLSettings, COBOLSettings } from './iconfiguration';
import COBOLQuickParse, { COBOLToken } from './cobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import TrieSearch from 'trie-search';

export class CobolSourceCompletionItemProvider implements CompletionItemProvider {

    private iconfig: ICOBOLSettings;

    public constructor(config: ICOBOLSettings) {
        this.iconfig = config;
    }

    private getPerformTargets(document: TextDocument): TrieSearch {
        let sf: COBOLQuickParse | undefined = VSQuickCOBOLParse.getCachedObject(document, document.fileName);

        if (sf !== undefined) {
            if (sf.cpPerformTargets === undefined) {
                sf.cpPerformTargets = new TrieSearch("tokenName");
                var words = sf.cpPerformTargets;

                for (let [key, token] of sf.sections) {
                    if (token.inProcedureDivision) {
                        words.add(token);
                    }
                }
                for (let [key, token] of sf.paragraphs) {
                    if (token.inProcedureDivision) {
                        words.add(token);
                    }
                }
                return words;
            }
            else {
                return sf.cpPerformTargets;
            }
        }

        return new TrieSearch('tokenName');
    }

    private getConstantsOrVariables(document: TextDocument): TrieSearch  {
        let sf = VSQuickCOBOLParse.getCachedObject(document, document.fileName);

        if (sf !== undefined) {
            if (sf.cpConstantsOrVars === undefined) {
                sf.cpPerformTargets = new TrieSearch('tokenName');
                let words: TrieSearch = sf.cpPerformTargets;

                for (let key of sf.constantsOrVariables.keys()) {
                    let tokens: COBOLToken[] | undefined = sf.constantsOrVariables.get(key);
                    if (tokens !== undefined) {
                        for (let token of tokens) {
                            words.add(token);
                        }
                    }
                }
                return words;
            } else {
                return sf.cpConstantsOrVars;
            }
        }

        return new TrieSearch('tokenName');
    }

    private camelize(text: string): string {
        let ret="";
        let uppercaseNext: boolean = true;
        for(let c=0; c<text.length; c++) {
            let ch = text[c];
            if (uppercaseNext) {
                ret += ch.toUpperCase();
                uppercaseNext = false;
            } else {
                if (ch === '-' || ch === '_') {
                    uppercaseNext = true;
                }

                ret += ch.toLocaleLowerCase();
            }
        }

        return ret;
    }

    private getItemsFromList(tsearch: TrieSearch, wordToComplete: string, kind: CompletionItemKind): CompletionItem[] {
        let  iconfig:COBOLSettings = VSCOBOLConfiguration.get();

        let includeUpper: boolean = iconfig.intellisense_include_uppercase;
        let includeLower: boolean = iconfig.intellisense_include_lowercase;
        let includeAsIS:boolean = iconfig.intellisense_include_unchanged;
        let includeCamelCase: boolean = iconfig.intellisense_include_camalcase;
        let limit: number = iconfig.intellisense_item_limit;

        let words: COBOLToken[] = tsearch.get(wordToComplete);
        let numberOfWordsInResults = words.length;

        const items: CompletionItem[] = [];
        for (let c = 0; c < numberOfWordsInResults; c++) {

            //if the text is uppercase, the present the items as uppercase
            let key: COBOLToken = words[c];

            if (includeAsIS) {
                let completionItem = new CompletionItem(key.tokenName, kind);
                items.push(completionItem);
            }

            if (includeLower && key.tokenName !== key.tokenNameLower) {
                let completionItem = new CompletionItem(key.tokenNameLower, kind);
                items.push(completionItem);
            }

            if (includeUpper && key.tokenName !== key.tokenName.toUpperCase()) {
                let completionItem = new CompletionItem(key.tokenName.toUpperCase(), kind);
                items.push(completionItem);
            }

            if (includeCamelCase) {
                let completionItem = new CompletionItem(this.camelize(key.tokenName), kind);
                items.push(completionItem);
            }

            if (items.length >= limit) {
                return items;
            }
        }

        // logMessage("Search for [" + wordToComplete + "] gives " + items.length + " words");
        return items;

    }

    provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        const items: CompletionItem[] = [];

        if (this.iconfig.enable_data_provider === false) {
            return items;
        }

        let wordToComplete = '';
        let lineBefore = "";
        const range = document.getWordRangeAtPosition(position);
        if (range) {
            wordToComplete = document.getText(new Range(range.start, position));
            lineBefore = document.getText(new Range(new Position(range.start.line, 0), new Position(position.line, position.character - wordToComplete.length))).trim();
            let lastSpace = lineBefore.lastIndexOf(" ");
            if (lastSpace !== -1) {
                let lineOrg = lineBefore;
                lineBefore = lineBefore.substr(1 + lastSpace);
                if (lineBefore === "to") {
                    if (lineOrg.toLocaleLowerCase().indexOf("go") !== -1) {
                        lineBefore = "goto";
                    }
                }
            }
        } else {
            let currentLine: string = document.lineAt(position.line).text.trim();
            let lastSpace = currentLine.lastIndexOf(" ");
            if (lastSpace === -1) {
                lineBefore = currentLine;
            } else {
                lineBefore = currentLine.substr(1 + lastSpace);
            }
        }

        if (lineBefore.length !== 0) {
            switch (lineBefore.toLocaleLowerCase()) {
                case "perform":
                case "goto": {
                    const words = this. getPerformTargets(document);
                    return this.getItemsFromList(words, wordToComplete, CompletionItemKind.Method);
                }
                case "move":
                    {
                        const words = this.getConstantsOrVariables(document);

                        // TODO:
                        //
                        // if (this.iconfig.intellisense_include_uppercase &&
                        //     words.indexOf("SPACE") === -1) {
                        //     words.push("SPACE");
                        //     words.push("SPACES");
                        //     words.push("LOW-VALUES");
                        //     words.push("HIGH-VALUES");
                        // }

                        // if (words.hasWord("space") === false) {
                        //     words.addWord("space");
                        //     words.addWord("spaces");
                        //     words.addWord("low-values");
                        //     words.addWord("high-values");
                        // }

                        return this.getItemsFromList(words, wordToComplete, CompletionItemKind.Variable);
                    }

                case "initialize":
                case "set":
                case "into":
                case "to":
                case "from":
                case "add":
                case "subtract":
                case "multiply":
                case "divide":
                case "compute":
                case "giving":
                    const words = this.getConstantsOrVariables(document);
                    return this.getItemsFromList(words, wordToComplete, CompletionItemKind.Variable);
            }
        }

        return items;
    }

}
