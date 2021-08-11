import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from 'vscode';
import { camelize } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './vsconfiguration';
import { ICOBOLSettings } from './iconfiguration';

export class KeywordAutocompleteCompletionItemProvider implements CompletionItemProvider {
	private words: string[];
	private isCOBOL: boolean;

	public constructor(keywords: string[], forCOBOL: boolean) {
		this.isCOBOL = forCOBOL;

		/* in the future, this could be extended to add a pri rather than just use the position
		 * the array
		 */
		this.words = keywords;
	}

	private getKeywordsGivenPartialWord(wordToComplete: string, limit: number): CompletionItem[] {
		if (wordToComplete.length === 0) {
			return [];
		}

		const iconfig: ICOBOLSettings = VSCOBOLConfiguration.get();
		const includeCamelCase: boolean = iconfig.intellisense_include_camelcase;
		const includeUpper: boolean = iconfig.intellisense_include_uppercase;
		const includeLower: boolean = iconfig.intellisense_include_lowercase;
		const includeAsIS: boolean = iconfig.intellisense_include_unchanged;

		const items: CompletionItem[] = [];
		const wordToCompleteLower = wordToComplete.toLowerCase();
		for (let key of this.words) {
			const keyLower = key.toLowerCase();
			if (keyLower.startsWith(wordToCompleteLower) === false) {
				continue;
			}
			const retKeys = [];

			const orgKey = key;
			key = orgKey.substr(wordToComplete.length);

			//if the text is uppercase, the present the items as uppercase
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
			});

			for (const uniqueRetKey of uniqueRetKeys) {
				const ci = new CompletionItem(uniqueRetKey, CompletionItemKind.Keyword);
				ci.detail = orgKey;
				items.push(ci);
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
		const currentLine: string = document.lineAt(position.line).text;

		if (currentLine.endsWith(" ")) {
			return [];
		}

		const range = document.getWordRangeAtPosition(position);
		if (range) {
			wordToComplete = document.getText(new Range(range.start, position));
			lineBefore = document.getText(new Range(new Position(range.start.line, 0), new Position(position.line, position.character - wordToComplete.length))).trim();
		} else {
			wordToComplete = currentLine.trim();
			const prevSpace = wordToComplete.lastIndexOf(" ");
			if (prevSpace !== -1) {
				wordToComplete = wordToComplete.substr(prevSpace).trim();
			}
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
