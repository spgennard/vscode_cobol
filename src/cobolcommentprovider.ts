import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList, Range, SnippetString, MarkdownString } from 'vscode';
import VSQuickCOBOLParse from './vscobolscanner';
import { ICOBOLSettings } from './iconfiguration';
import COBOLSourceScanner, { CobolDocStyle, UsingState } from './cobolsourcescanner';

export class CobolCommentProvider implements CompletionItemProvider {

    private iconfig: ICOBOLSettings;

    public constructor(config: ICOBOLSettings) {
        this.iconfig = config;
    }

    provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList<CompletionItem>> {
        const items: CompletionItem[] = [];

        const lineBefore = document.lineAt(position.line).text.substr(0, position.character);
        let inComment = false;
        if (lineBefore.indexOf("*>") !== -1) {
            inComment = true;
        }
        if (lineBefore.length > 7 && lineBefore[6] === '*') {
            inComment = true;
        }

        // not in a comment.. drop out now!
        if (!inComment) {
            return items;
        }

        const sf: COBOLSourceScanner | undefined = VSQuickCOBOLParse.getCachedObject(document);
        if (sf === undefined) {
            return items;
        }

        // can only do coboldoc at the moment
        if (!(sf.commentStyle === CobolDocStyle.COBOLDOC || sf.commentStyle === CobolDocStyle.unknown)) {
            return items;
        }

        // const snippetCompletion = new CompletionItem('Good part of the day');
        // snippetCompletion.insertText = new SnippetString('Good ${1|morning,afternoon,evening|}. It is ${1}, right?');
        // snippetCompletion.documentation = new MarkdownString("Inserts a snippet that lets you select the _appropriate_ part of the day for your greeting.");
        // items.push(snippetCompletion);

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        for (const [key, callTarget] of sf.callTargets) {
            if (callTarget !== undefined) {
                const snippet = new CompletionItem(`Auto-Doc: for ${callTarget.Token.tokenName}`);
                let c = 1;
                let snippetText = "\n*> ${1:Description of " + callTarget.Token.tokenName + "}\n";
                c++;
                for (const p of callTarget.CallParameters) {
                    snippetText += "*> @param " + p.name + " ${" + c + ":argument description}\n";
                    c++;
                }
                snippetText += `*> @return return-code`;
                snippet.insertText = new SnippetString(snippetText);
                items.push(snippet);
            }
        }

        for (const [key, callTarget] of sf.functionTargets) {
            if (callTarget !== undefined) {
                let addedReturn = false;
                const snippet = new CompletionItem(`Auto-Doc: for function ${callTarget.Token.tokenName}`);
                let c = 1;
                let snippetText = "\n*> ${1:Description of " + callTarget.Token.tokenName + "}\n";
                c++;
                for (const p of callTarget.CallParameters) {
                    if (p.using !== UsingState.RETURNING) {
                        snippetText += "*> @param " + p.name + " ${" + c + ":argument description}\n";
                    } else {
                        snippetText += "*> @return " + p.name + " ${" + c + ":argument description}\n";
                        addedReturn = true;
                    }
                    c++;
                }
                if (!addedReturn) {
                    snippetText += `*> @return return-code`;
                }
                snippet.insertText = new SnippetString(snippetText);
                items.push(snippet);
            }
        }
        return items;
    }
}