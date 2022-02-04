import { CancellationToken, CompletionContext, CompletionItem, CompletionItemKind, CompletionItemProvider, CompletionList, Position, ProviderResult, Range, SnippetString, TextDocument } from "vscode";
import { COBOLSourceScanner } from "./cobolsourcescanner";
import { KnownAPIs } from "./keywords/cobolCallTargets";

const snippetMap = new Map<string,SnippetString>(
    [
        [ "CBL_CLOSE_FILE", new SnippetString("call \"CBL_CLOSE_FILE\" using ${1:HANDLE}\n  returning ${2:STATUS-CODE}\nend-call\n$0") ]
    ]);




export class SnippetCompletionItemProvider implements CompletionItemProvider {


    private getCompletionItemForAPI(api:string, position: Position) : CompletionItem|undefined {
        const snippet = snippetMap.get(api);
        if (snippet === undefined) {
            return snippet;
        }
        const preselect = `call "${api}"`;
        const ci = new CompletionItem(preselect);
        ci.keepWhitespace = false;
        ci.kind = CompletionItemKind.Snippet;
        ci.insertText = snippet;
        ci.preselect = true;
        ci.filterText = preselect;
        ci.commitCharacters = [ `call "${api}"`];
        const before = new Range(new Position(position.line, position.character-preselect.length),
            new Position(position.line, position.character + snippet.value.length)
        );

        ci.range = before;
        const ki = KnownAPIs.getCallTarget(api);
        if (ki !== undefined) {
            ci.documentation = ki.description;
        }
        return ci;          
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {

        const wordRange = document.getWordRangeAtPosition(new Position(position.line, position.character - 2));
        if (!wordRange) return [];

        // "CBL_CLOSE_FILE": {
        // 	"prefix" : [
        // 		"call \"CBL_CLOSE_FILE\""
        // 	],
        // 	"body": [
        // 		"call \"CBL_CLOSE_FILE\" using",
        // 		"\tby value ${1:handle}",
        // 		"\treturning ${2:status}",
        // 		"end-call",
        // 		"$0"
        // 	],
        // 	"description" : "CBL_CLOSE_FILE - Closes an open file\r\n\r\n\u2192 HANDLE is pic x(4) comp-x",
        // 	"scope" : "cobol"
        // }
        const snippets: CompletionItem[] = [];
        const preWord = document.getText(wordRange);
        if (preWord !== undefined) {
            const preTrimmedWork = COBOLSourceScanner.trimLiteral(preWord);
            if (preTrimmedWork === "CBL_CLOSE_FILE") {
                const ci = this.getCompletionItemForAPI(preTrimmedWork,position);
                if (ci !== undefined) {
                    snippets.push(ci);
                }
            }
        }

        return snippets;
    }

}