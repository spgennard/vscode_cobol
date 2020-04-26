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

    private stringPad(input: string, padLength: number, padString: string): string {
        const count = Math.max(0, padLength - input.length);
        const padMultiple = Math.ceil(count / padString.length);
        const paddingString = padString.repeat(padMultiple).substr(0, count);
        return paddingString + input;
    }

    provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        const items: CompletionItem[] = [];
        let wordToComplete = '';
        let lineBefore = "";
        const range = document.getWordRangeAtPosition(position);
        if (range) {
            wordToComplete = document.getText(new Range(range.start, position));
            lineBefore = document.getText(new Range(new Position(range.start.line, 0), new Position(position.line, position.character - wordToComplete.length))).trim();
            let lastSpace = lineBefore.lastIndexOf(" ");
            if (lastSpace !== -1) {
                lineBefore = lineBefore.substr(1+lastSpace);
            }
            if (lineBefore.length !== 0) {
                if (lineBefore.toLocaleLowerCase().endsWith("perform")) {
                    const words = trie([]);
                    this.getPerformTargets(words, document);
                    return this.getItemsFromTrie(words, wordToComplete, 120, CompletionItemKind.Method);
                }
            }
        }
        return items;
    }

}
