import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, CompletionItemKind, Range } from "vscode";
import { SourceScannerUtils } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";
import { getCOBOLKeywordList } from "./keywords/cobolKeywords";
import { jclStatements } from "./keywords/jclstatements";
import { KeywordSnippetProvider, SnippetCompletionItemProvider } from "./vssnippetprovider";
import { VSCustomIntelliseRules } from "./vscustomrules";
import { VSExternalFeatures } from "./vsexternalfeatures";

export class KeywordAutocompleteCompletionItemProvider implements CompletionItemProvider {
	private isCOBOL: boolean;

	private onlyShowSnippetKeywords = new Map<string, string>([["dfhresp", "dfhresp"]]);

	private includeExtraSpaceKeywords = new Map<string, string>();
	private dollarGTWordRegEx = new RegExp("[#>$0-9a-zA-Z][>a-zA-Z0-9-_]*");

	public static Default4COBOL: KeywordAutocompleteCompletionItemProvider;

	public constructor(forCOBOL: boolean, settings: ICOBOLSettings) {
		this.isCOBOL = forCOBOL;

		if (this.isCOBOL) {
			KeywordAutocompleteCompletionItemProvider.Default4COBOL = this;
			this.reFreshConfiguration(settings);
		}
	}

	public reFreshConfiguration(settings: ICOBOLSettings) {
		this.includeExtraSpaceKeywords.clear();
		for (const kwNoSpace of settings.intellisense_add_space_keywords) {
			this.includeExtraSpaceKeywords.set(kwNoSpace.toLowerCase(), "");
		}
	}

	private getKeywordsGivenPartialWord(iconfig: ICOBOLSettings, wordToComplete: string, limit: number, langid: string): CompletionItem[] {
		if (wordToComplete.length === 0) {
			return [];
		}

		const items: CompletionItem[] = [];
		const wordToCompleteLower = wordToComplete.toLowerCase();
		const words: string[] = this.isCOBOL === false ? jclStatements : getCOBOLKeywordList(langid);

		if (!this.isCOBOL) {
			for (const key of words) {
				const keyLower = key.toLowerCase();
				if (keyLower.startsWith(wordToCompleteLower) === false) {
					continue;
				}
				const ci = new CompletionItem(key, CompletionItemKind.Keyword);
				ci.detail = `Keyword ${key}`;
				items.push(ci);
			}

			return items;
		}

		switch (wordToCompleteLower) {
			case "function":
				for (const snip of SnippetCompletionItemProvider.Default.getAllFunctions()) {
					items.push(snip);
				}
				break;
		}

		for (const key of words) {
			const keyLower = key.toLowerCase();
			if (keyLower.startsWith(wordToCompleteLower) === false) {
				continue;
			}
			const retKeys = new Map<string, string>();
			const keywordSnippets = KeywordSnippetProvider.Default.getKeywordSnippet(key);
			let invokeNext = false;
			if (this.onlyShowSnippetKeywords.has(keyLower)) {
				invokeNext = true;
			} else {
				const extraKey = this.includeExtraSpaceKeywords.has(keyLower) ? " " : "";
				const istyle =  VSCustomIntelliseRules.Default.findCustomIStyle(iconfig,key, iconfig.intellisense_style); 
				switch (istyle) {
					case intellisenseStyle.CamelCase:
						{
							const camelKey = SourceScannerUtils.camelize(key);
							if (!retKeys.has(camelKey)) {
								retKeys.set(camelKey, camelKey + extraKey);
							}
						}
						break;
					case intellisenseStyle.UpperCase:
						{
							const upperKey = key.toUpperCase();
							if (!retKeys.has(upperKey)) {
								retKeys.set(upperKey, upperKey + extraKey);
							}
						}
						break;
					case intellisenseStyle.LowerCase:
						if (!retKeys.has(keyLower)) {
							retKeys.set(keyLower, keyLower + extraKey);
						}
						break;
					case intellisenseStyle.Unchanged:
						retKeys.set(key, key + extraKey);
						break;
				}
			}

			for (const [uniqueRetKey, uniqueRetKeySpace] of retKeys) {
				const ci = new CompletionItem(uniqueRetKeySpace, CompletionItemKind.Keyword);
				ci.detail = `COBOL keyword ${uniqueRetKey}`;
				items.push(ci);
				if (keywordSnippets.length > 1 || keyLower === "call" || keyLower === "function") {
					ci.command = { command: "editor.action.triggerSuggest", title: "Re-trigger completions..." };
				}
			}

			// do we have any specific keyword snippets?
			for (const snip of keywordSnippets) {
				snip.preselect = false;
				if (invokeNext) {
					snip.command = { command: "editor.action.triggerSuggest", title: "Re-trigger completions..." };
				}
				items.push(snip);
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

		const range = document.getWordRangeAtPosition(position, this.dollarGTWordRegEx);		// include prefix words such as $if.. >>if, just so we dont act on simular keywords
		if (range) {
			wordToComplete = document.getText(new Range(range.start, position));
			lineBefore = document.getText(new Range(new Position(range.start.line, 0), new Position(position.line, position.character - wordToComplete.length))).trim();
		} else {
			wordToComplete = currentLine.trim();
			const prevSpace = wordToComplete.lastIndexOf(" ");
			if (prevSpace !== -1) {
				wordToComplete = wordToComplete.substring(prevSpace).trim();
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

		const iconfig: ICOBOLSettings = VSCOBOLConfiguration.get_resource_settings(document,VSExternalFeatures);
		return this.getKeywordsGivenPartialWord(iconfig, wordToComplete, 128, document.languageId);
	}
}
