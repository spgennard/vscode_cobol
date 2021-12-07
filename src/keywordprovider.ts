import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from "vscode";
import { SourceScannerUtils } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";
import { getCOBOLKeywordList } from "./keywords/cobolKeywords";
import { jclStatements } from "./keywords/jclstatements";

export class KeywordAutocompleteCompletionItemProvider implements CompletionItemProvider {
	private isCOBOL: boolean;

	public constructor(forCOBOL: boolean) {
		this.isCOBOL = forCOBOL;
	}

	private getKeywordsGivenPartialWord(wordToComplete: string, limit: number, langid: string): CompletionItem[] {
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
		const words:string[] = this.isCOBOL === false ? jclStatements : getCOBOLKeywordList(langid);

		for (const key of words) {
			const keyLower = key.toLowerCase();
			if (keyLower.startsWith(wordToCompleteLower) === false) {
				continue;
			}
			const retKeys = new Map<string,string>();

			//if the text is uppercase, the present the items as uppercase
			if (includeAsIS) {
				retKeys.set(key,key+" ");
			}

			if (includeCamelCase) {
				const camelKey = SourceScannerUtils.camelize(key);
				if (!retKeys.has(camelKey)) {
					retKeys.set(camelKey,camelKey+" ");
				}
			}

			if (includeUpper) {
				const upperKey = key.toUpperCase();
				if (!retKeys.has(upperKey)) {
					retKeys.set(upperKey,upperKey+" ");
				}
			}

			if (includeLower) {
				if (!retKeys.has(keyLower)) {
					retKeys.set(keyLower,keyLower+" ");
				}
			}

			for(const [uniqueRetKey, uniqueRetKeySpace] of retKeys) {
				const ci = new CompletionItem(uniqueRetKeySpace, CompletionItemKind.Keyword);
				ci.detail = `COBOL keyword ${uniqueRetKey}`;
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
		let wordToComplete = "";
		let lineBefore = "";
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
			if (lineBefore.length >= 7 && (lineBefore[6] === "*" || lineBefore[6] === "/")) {
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

		return this.getKeywordsGivenPartialWord(wordToComplete, 128, document.languageId);
	}
}
