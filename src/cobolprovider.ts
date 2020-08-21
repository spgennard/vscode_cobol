import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from 'vscode';
import VSQuickCOBOLParse from './vscobolscanner';
import { ICOBOLSettings, COBOLSettings } from './iconfiguration';
import COBOLSourceScanner, { COBOLToken, camelize } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './configuration';
import TrieSearch from 'trie-search';
import { performance_now, logMessage, logTimeThreshold } from './extension';

export class CobolSourceCompletionItemProvider implements CompletionItemProvider {

    private iconfig: ICOBOLSettings;

    public constructor(config: ICOBOLSettings) {
        this.iconfig = config;
    }

    private getPerformTargets(document: TextDocument): TrieSearch {
        const sf: COBOLSourceScanner|undefined = VSQuickCOBOLParse.getCachedObject(document);

        if (sf !== undefined) {
            if (sf.cpPerformTargets === undefined) {
                sf.cpPerformTargets = new TrieSearch("tokenName");
                const words = sf.cpPerformTargets;

                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                for (const [key, token] of sf.sections) {
                    if (token.inProcedureDivision) {
                        words.add(token);
                    }
                }
                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                for (const [key, token] of sf.paragraphs) {
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

    private getConstantsOrVariables(document: TextDocument): TrieSearch {
        const sf = VSQuickCOBOLParse.getCachedObject(document);

        if (sf !== undefined) {
            if (sf.cpConstantsOrVars === undefined) {
                sf.cpPerformTargets = new TrieSearch('tokenName');
                const words: TrieSearch = sf.cpPerformTargets;

                for (const key of sf.constantsOrVariables.keys()) {
                    const tokens: COBOLToken[] | undefined = sf.constantsOrVariables.get(key);
                    if (tokens !== undefined) {
                        for (const token of tokens) {
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


    private getItemsFromList(tsearch: TrieSearch, wordToComplete: string, kind: CompletionItemKind): CompletionItem[] {
        const iconfig: COBOLSettings = VSCOBOLConfiguration.get();

        const includeUpper: boolean = iconfig.intellisense_include_uppercase;
        const includeLower: boolean = iconfig.intellisense_include_lowercase;
        const includeAsIS: boolean = iconfig.intellisense_include_unchanged;
        const includeCamelCase: boolean = iconfig.intellisense_include_camelcase;
        const limit: number = iconfig.intellisense_item_limit;

        const words: COBOLToken[] = tsearch.get(wordToComplete);
        const numberOfWordsInResults = words.length;

        const items: CompletionItem[] = [];
        for (let c = 0; c < numberOfWordsInResults; c++) {

            //if the text is uppercase, the present the items as uppercase
            const key: COBOLToken = words[c];

            if (includeAsIS) {
                const completionItem = new CompletionItem(key.tokenName, kind);
                items.push(completionItem);
            }

            if (includeLower && key.tokenName !== key.tokenNameLower) {
                const completionItem = new CompletionItem(key.tokenNameLower, kind);
                items.push(completionItem);
            }

            if (includeUpper && key.tokenName !== key.tokenName.toUpperCase()) {
                const completionItem = new CompletionItem(key.tokenName.toUpperCase(), kind);
                items.push(completionItem);
            }

            if (includeCamelCase) {
                const completionItem = new CompletionItem(camelize(key.tokenName), kind);
                items.push(completionItem);
            }

            if (items.length >= limit) {
                return items;
            }
        }

        // logMessage("Search for [" + wordToComplete + "] gives " + items.length + " words");
        return items;

    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        let items: CompletionItem[] = [];

        if (this.iconfig.enable_data_provider === false) {
            return items;
        }

        const startTime = performance_now();
        let wordToComplete = '';
        let lineBefore = "";
        const range = document.getWordRangeAtPosition(position);
        if (range) {
            wordToComplete = document.getText(new Range(range.start, position));
            lineBefore = document.getText(new Range(new Position(range.start.line, 0), new Position(position.line, position.character - wordToComplete.length))).trim();
            const lastSpace = lineBefore.lastIndexOf(" ");
            if (lastSpace !== -1) {
                const lineOrg = lineBefore;
                lineBefore = lineBefore.substr(1 + lastSpace);
                if (lineBefore === "to") {
                    if (lineOrg.toLocaleLowerCase().indexOf("go") !== -1) {
                        lineBefore = "goto";
                    }
                }
            }
        } else {
            const currentLine: string = document.lineAt(position.line).text.trim();
            const lastSpace = currentLine.lastIndexOf(" ");
            if (lastSpace === -1) {
                lineBefore = currentLine;
            } else {
                lineBefore = currentLine.substr(1 + lastSpace);
            }
        }

        if (lineBefore.length !== 0) {
            switch (lineBefore.toLocaleLowerCase()) {
                case "perform":
                case "goto":
                    {
                        const words = this.getPerformTargets(document);
                        items = this.getItemsFromList(words, wordToComplete, CompletionItemKind.Method);
                        break;
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

                        items = this.getItemsFromList(words, wordToComplete, CompletionItemKind.Variable);
                        break;
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
                    {
                        const words = this.getConstantsOrVariables(document);
                        items = this.getItemsFromList(words, wordToComplete, CompletionItemKind.Variable);
                        break;
                    }
            }
        }

        const totalTimeInMS = performance_now() - startTime;
        const timeTaken = totalTimeInMS.toFixed(2);
        if (totalTimeInMS > logTimeThreshold) {
            logMessage(" - CobolSourceCompletionItemProvider took " + timeTaken + " ms");
        }
        return items;
    }

}
