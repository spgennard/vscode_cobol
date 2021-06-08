import TrieSearch from 'trie-search';
import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from 'vscode';
import { camelize } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './configuration';
import { ICOBOLSettings } from './iconfiguration';

interface TrieObject {
	key: string;
	index: number;
}

export class KeywordAutocompleteCompletionItemProvider implements CompletionItemProvider {
	private words: TrieSearch = new TrieSearch('key');
	private isCOBOL: boolean;

	public constructor(keywords: string[], forCOBOL: boolean) {
		let i = 0;
		this.isCOBOL = forCOBOL;

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

	private getKeywordsGivenPartialWord(wordToComplete: string, limit: number): CompletionItem[] {
		const iconfig: ICOBOLSettings = VSCOBOLConfiguration.get();
		const results = this.words.get(wordToComplete);
		const includeCamelCase: boolean = iconfig.intellisense_include_camelcase;
		const includeUpper: boolean = iconfig.intellisense_include_uppercase;
		const includeLower: boolean = iconfig.intellisense_include_lowercase;
		const includeAsIS: boolean = iconfig.intellisense_include_unchanged;

		// Sort the results by index
		results.sort((a: TrieObject, b: TrieObject) => {
			return a.index - b.index;
		});

		const items: CompletionItem[] = [];
		for (const [, tag] of results.entries()) {
			const retKeys = [];

			//if the text is uppercase, the present the items as uppercase
			const key = tag.key;
			if (includeAsIS) {
				retKeys.push(key);
			}

			if (this.isCOBOL && includeCamelCase) {
				retKeys.push(camelize(key));
			}

			if (includeUpper) {
				retKeys.push(key.toUpperCase());
			}

			if (includeLower) {
				retKeys.push(key.toLowerCase());
			}

			const uniqueRetKeys = retKeys.filter(function (elem, index, self) {
				return index === self.indexOf(elem);
			})

			for (const uniqueRetKey of uniqueRetKeys) {
				items.push(new CompletionItem(uniqueRetKey, CompletionItemKind.Keyword));
			}

			if (items.length >= limit) {
				return items;
			}
		}

		return items;
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
		let wordToComplete = '';
		let lineBefore = '';
		const range = document.getWordRangeAtPosition(position);
		if (range) {
			wordToComplete = document.getText(new Range(range.start, position));
			lineBefore = document.getText(new Range(new Position(range.start.line, 0), new Position(position.line, position.character - wordToComplete.length))).trim();
		}

		if (this.isCOBOL) {
			let inComment = false;

			if (lineBefore.indexOf("*>") !== -1) {
				inComment = true;
			}
			if (lineBefore.length > 7 && lineBefore[6] === '*') {
				inComment = true;
			}

			if (inComment) {
				const items: CompletionItem[] = [];
				return items;
			}
		}

		// subjective code, checks to see if the previous word is know
		//  tries to have "verb verb" but that can be problematic
		//  so need to re-think it and remove it or replace it..
		//
		// const lastSpace = lineBefore.lastIndexOf(" ");
		// if (lastSpace !== -1) {
		// 	lineBefore = lineBefore.substr(1 + lastSpace);
		// }
		// const prevWords = this.words.get(lineBefore);
		// if (prevWords.length !== 0) {
		// 	const items: CompletionItem[] = [];
		// 	return items;
		// }

		return this.getKeywordsGivenPartialWord(wordToComplete, 128);
	}
}
