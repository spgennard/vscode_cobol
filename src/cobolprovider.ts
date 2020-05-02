import trie from 'trie-prefix-tree';
import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from 'vscode';
import VSQuickCOBOLParse from './vscobolquickparse';
import { ICOBOLSettings } from './iconfiguration';
import COBOLQuickParse from './cobolquickparse';


export class CobolSourceCompletionItemProvider implements CompletionItemProvider {

    private iconfig: ICOBOLSettings;

    public constructor(config: ICOBOLSettings) {
        this.iconfig = config;
    }

    private getPerformTargets(document: TextDocument): any {
        let sf: COBOLQuickParse | undefined = VSQuickCOBOLParse.getCachedObject(document, document.fileName);

        if (sf !== undefined) {
            if (sf.cpPerformTargets === undefined) {
                sf.cpPerformTargets = trie([]);
                let words = sf.cpPerformTargets;

                for (let [key, token] of sf.sections) {
                    if (token.inProcedureDivision) {
                        if (token.tokenNameLower === token.tokenNameLower) {
                            words.addWord(token.tokenName);
                            words.addWord(token.tokenName.toUpperCase());
                        } else {
                            words.addWord(token.tokenName);
                            words.addWord(token.tokenNameLower);
                        }
                    }
                }
                for (let [key, token] of sf.paragraphs) {
                    if (token.inProcedureDivision) {
                        if (token.tokenNameLower === token.tokenNameLower) {
                            words.addWord(token.tokenName);
                            words.addWord(token.tokenName.toUpperCase());
                        } else {
                            words.addWord(token.tokenName);
                            words.addWord(token.tokenNameLower);
                        }

                    }
                }
                return words;
            }
            else {
                return sf.cpPerformTargets;
            }
        }
    }

    private getConstantsOrVariables(document: TextDocument): any {
        let sf = VSQuickCOBOLParse.getCachedObject(document, document.fileName);

        if (sf !== undefined) {
            if (sf.cpConstantsOrVars === undefined) {
                sf.cpConstantsOrVars = trie([]);
                let words = sf.cpConstantsOrVars;
                for (let [key, token] of sf.constantsOrVariables) {
                    if (token.length >= 1) {
                        if (token[0].tokenName === token[0].tokenNameLower) {
                            words.addWord(token[0].tokenName);
                            words.addWord(token[0].tokenName.toUpperCase());

                        } else {
                            words.addWord(token[0].tokenName);
                            words.addWord(token[0].tokenNameLower);
                        }
                    }
                }
                return words;
            } else {
                return sf.cpConstantsOrVars;
            }
        }
    }

    private getItemsFromTrie(words: any, wordToComplete: string, limit: number, kind: CompletionItemKind): CompletionItem[] {
        const results = words.getPrefix(wordToComplete);
        const isUpper = wordToComplete.toUpperCase() === wordToComplete;

        const numberOfWordsInResults = results.length;
        const items: CompletionItem[] = [];
        for (let c = 0; c < numberOfWordsInResults; c++) {

            //if the text is uppercase, the present the items as uppercase
            let key = results[c];
            if (isUpper) {
                key = key.toUpperCase();
            }

            let completionItem = new CompletionItem(key, kind);
            items.push(completionItem);
            if (items.length >= limit) {
                return items;
            }
        }

        return items;

    }

    provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        const items: CompletionItem[] = [];

        if (this.iconfig.experimential_features === false) {
            return items;
        }

        let wordToComplete = '';
        let lineBefore = "";
        const range = document.getWordRangeAtPosition(position);
        if (range) {
            wordToComplete = document.getText(new Range(range.start, position));
            const isUpper = wordToComplete.toUpperCase() === wordToComplete;
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

            if (lineBefore.length !== 0) {
                switch (lineBefore.toLocaleLowerCase()) {
                    case "perform":
                    case "goto": {
                        const words = this.getPerformTargets(document);
                        return this.getItemsFromTrie(words, wordToComplete, 120, CompletionItemKind.Method);
                    }
                    case "move":
                        {
                            const words = this.getConstantsOrVariables(document);
                            if (isUpper) {
                                if (words.hasWord("SPACE") === false) {
                                    words.addWord("SPACE");
                                    words.addWord("SPACES");
                                    words.addWord("LOW-VALUES");
                                    words.addWord("HIGH-VALUES");
                                }

                            } else {
                                if (words.hasWord("space") === false) {
                                    words.addWord("space");
                                    words.addWord("spaces");
                                    words.addWord("low-values");
                                    words.addWord("high-values");
                                }
                            }
                            return this.getItemsFromTrie(words, wordToComplete, 120, CompletionItemKind.Variable);
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
                        return this.getItemsFromTrie(words, wordToComplete, 120, CompletionItemKind.Variable);
                }
            }
        }
        return items;
    }

}
