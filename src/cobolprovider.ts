import trie from 'trie-prefix-tree';
import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from 'vscode';
import VSQuickCOBOLParse from './vscobolquickparse';


export class CobolSourceCompletionItemProvider implements CompletionItemProvider {

    public constructor() {
    }

    private getPerformTargets(words: any, document: TextDocument) {
        let sf = VSQuickCOBOLParse.getCachedObject(document, document.fileName);

        if (sf !== undefined) {
            for (let [key, token] of sf.sections) {
                if (token.inProcedureDivision) {
                    words.addWord(token.tokenName);
                }
            }
            for (let [key, token] of sf.paragraphs) {
                if (token.inProcedureDivision) {
                    words.addWord(token.tokenName);
                }
            }
        }
    }

    private getConstantsOrVariables(words: any, document: TextDocument) {
        let sf = VSQuickCOBOLParse.getCachedObject(document, document.fileName);

        if (sf !== undefined) {
            for (let [key, token] of sf.constantsOrVariables) {
                if (token.length >= 1) {
                    words.addWord(token[0].tokenName);
                }
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
                const words = trie([]);
                switch (lineBefore.toLocaleLowerCase()) {
                    case "perform":
                    case "goto":
                        this.getPerformTargets(words, document);
                        return this.getItemsFromTrie(words, wordToComplete, 120, CompletionItemKind.Method);
                    case "move":
                        if (isUpper) {
                            words.addWord("SPACE");
                            words.addWord("SPACES");
                            words.addWord("LOW-VALUES");
                            words.addWord("HIGH-VALUES");

                        } else {
                            words.addWord("space");
                            words.addWord("spaces");
                            words.addWord("low-values");
                            words.addWord("high-values");
                        }
                    case "into":
                    case "to":
                    case "add":
                    case "from":
                        this.getConstantsOrVariables(words, document);
                        return this.getItemsFromTrie(words, wordToComplete, 120, CompletionItemKind.Variable);
                }
            }
        }
        return items;
    }

}
