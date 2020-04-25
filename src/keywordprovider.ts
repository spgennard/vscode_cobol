import TrieSearch from 'trie-search';
import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from 'vscode';

interface TrieObject {
	key: string;
	index: number;
}

export class KeywordAutocompleteCompletionItemProvider implements CompletionItemProvider {
	private words: TrieSearch = new TrieSearch('key');

	public constructor(keywords: string[]) {
		let i = 0;

		/* in the future, this could be extended to add a pri rather than just use the position
	     * the array
		 */
		this.words.addAll(keywords.map((value: string) => {
			return {
				key: value,
				index: ++i
			};
		}));
	}

	private getKeywordsGivenPartialWord(wordToComplete: string, limit: number) : CompletionItem[]  {
		const results = this.words.get(wordToComplete);
		const isUpper = wordToComplete.toUpperCase() === wordToComplete;

		// Sort the results by index
		results.sort((a: TrieObject, b: TrieObject) => {
			return a.index - b.index;
		});

		const numberOfWordsInResults = results.length;
		const items: CompletionItem[] = [];
		for (let [i, tag] of results.entries()) {

			//if the text is uppercase, the present the items as uppercase
			let key = tag.key;
			if (isUpper) {
				key = tag.key.toUpperCase();
			}

			let completionItem = new CompletionItem(key, CompletionItemKind.Keyword);

			// Set sortText to order the value when displaying them in the autocompletion menu
			completionItem.sortText = this.stringPad(i.toString(), numberOfWordsInResults.toString().length, '0');
			items.push(completionItem);
			if (items.length >= limit) {
				return items;
			}
		}

		return items;
	}

	public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
		let wordToComplete = '';
		const range = document.getWordRangeAtPosition(position);
		if (range) {
			wordToComplete = document.getText(new Range(range.start, position));
		}

		return this.getKeywordsGivenPartialWord(wordToComplete, 128);
	}

	private stringPad(input: string, padLength: number, padString: string): string {
		const count = Math.max(0, padLength - input.length);
		const padMultiple = Math.ceil(count / padString.length);
		const paddingString = padString.repeat(padMultiple).substr(0, count);
		return paddingString + input;
	}
}
