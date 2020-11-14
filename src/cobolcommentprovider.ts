// FROM package.json:

// "coboleditor.coboldoc_workspace_folder": {
//     "type": "string",
//     "title": "Default folder name for coboldoc files",
//     "description": "Default folder name for coboldoc files",
//     "default": "coboldoc"
// },

// FROM: cobol.json
// ,
// 	"coboldoc: file header": {
// 		"prefix": "*>**",
// 		"description": "coboldoc: file header",
// 		"body": [
// 			"*>**",
// 			"*>  ${1:Short description about ${TM_FILENAME/(.*)\\..+$/$1/}.}",
// 			"*>  @author ${2:author} ${3:(http://github.com/...)}",
// 			"*>  @license ${4|AGPLv3: GNU Affero General Public License version 3,AL2: Apache License version 2,BSD2: BSD 2-clause License,BSD3: BSD 3-clause License (New BSD License),BSL1: Boost Software License - Version 1.0,GPLv2: GNU General Public License version 2,GPLv3: GNU General Public License version 3,LGPLv3: GNU Lesser General Public License version 3,MIT: MIT License,MPLv2: Mozilla Public License version 2,CC-BY-3: Creative Commons Attribution 3.0,CC-BY-SA-3: Creative Commons Attribution-ShareAlike 3.0,CC-BY-NC-3: Creative Commons Attribution-NonCommercial 3.0,CC-BY-ND-3: Creative Commons Attribution-NoDerivs 3.0,CC-BY-NC-SA-3: Creative Commons Attribution-NonCommercial-ShareAlike 3.0,CC-BY-NC-ND-3: Creative Commons Attribution-NonCommercial-NoDerivs 3.0,CC-BY-4: Creative Commons Attribution 4.0,CC-BY-SA-4: Creative Commons Attribution-ShareAlike 4.0,CC-BY-NC-4: Creative Commons Attribution-NonCommercial 4.0,CC-BY-ND-4: Creative Commons Attribution-NoDerivs 4.0,CC-BY-NC-SA-4: Creative Commons Attribution-NonCommercial-ShareAlike 4.0,CC-BY-NC-ND-4: Creative Commons Attribution-NonCommercial-NoDerivs 4.0,CC0-1: CC0 1.0,UNL: The Unlicense,WTFPL: Do What The Fuck You Want To Public License,zlib: zlib License,custom: provide details|}",
// 			"*>",
// 			"*>  ${5:insert your license text here, see https://opensource.org/licenses}",
// 			"*>**",
// 			"${0}"
// 		],
// 		"scope": "cobol"
// 	}

// /* eslint-disable @typescript-eslint/no-unused-vars */
// import { CompletionItemProvider, TextDocument, Position, CancellationToken, CompletionItem, CompletionContext, ProviderResult, CompletionList,  SnippetString } from 'vscode';
// import VSCOBOLSourceScanner from './vscobolscanner';
// import { ICOBOLSettings } from './iconfiguration';
// import COBOLSourceScanner, { CobolDocStyle, UsingState } from './cobolsourcescanner';

// export class CobolCommentProvider implements CompletionItemProvider {

//     private iconfig: ICOBOLSettings;

//     public constructor(config: ICOBOLSettings) {
//         this.iconfig = config;
//     }

//     provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList<CompletionItem>> {
//         const items: CompletionItem[] = [];

//         const lineBefore = document.lineAt(position.line).text.substr(0, position.character);
//         let inComment = false;
//         if (lineBefore.indexOf("*>") !== -1) {
//             inComment = true;
//         }
//         if (lineBefore.length > 7 && lineBefore[6] === '*') {
//             inComment = true;
//         }

//         // not in a comment.. drop out now!
//         if (!inComment) {
//             return items;
//         }

//         const sf: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document);
//         if (sf === undefined) {
//             return items;
//         }

//         // can only do coboldoc at the moment
//         if (!(sf.commentDocStyle === CobolDocStyle.COBOLDOC || sf.commentDocStyle === CobolDocStyle.unknown)) {
//             return items;
//         }

//         // const snippetCompletion = new CompletionItem('Good part of the day');
//         // snippetCompletion.insertText = new SnippetString('Good ${1|morning,afternoon,evening|}. It is ${1}, right?');
//         // snippetCompletion.documentation = new MarkdownString("Inserts a snippet that lets you select the _appropriate_ part of the day for your greeting.");
//         // items.push(snippetCompletion);

//         // eslint-disable-next-line @typescript-eslint/no-unused-vars
//         for (const [key, callTarget] of sf.callTargets) {
//             if (callTarget !== undefined) {
//                 const snippet = new CompletionItem(`Auto-Doc: for ${callTarget.Token.tokenName}`);
//                 let c = 1;
//                 let snippetText = "\n*> ${1:Description of " + callTarget.Token.tokenName + "}\n";
//                 c++;
//                 for (const p of callTarget.CallParameters) {
//                     snippetText += "*> @param " + p.name + " ${" + c + ":argument description}\n";
//                     c++;
//                 }
//                 snippetText += `*> @return return-code`;
//                 snippet.insertText = new SnippetString(snippetText);
//                 items.push(snippet);
//             }
//         }

//         for (const [, callTarget] of sf.functionTargets) {
//             if (callTarget !== undefined) {
//                 let addedReturn = false;
//                 const snippet = new CompletionItem(`Auto-Doc: for function ${callTarget.Token.tokenName}`);
//                 let c = 1;
//                 let snippetText = "\n*> ${1:Description of " + callTarget.Token.tokenName + "}\n";
//                 c++;
//                 for (const p of callTarget.CallParameters) {
//                     if (p.using !== UsingState.RETURNING) {
//                         snippetText += "*> @param " + p.name + " ${" + c + ":argument description}\n";
//                     } else {
//                         snippetText += "*> @return " + p.name + " ${" + c + ":argument description}\n";
//                         addedReturn = true;
//                     }
//                     c++;
//                 }
//                 if (!addedReturn) {
//                     snippetText += `*> @return return-code`;
//                 }
//                 snippet.insertText = new SnippetString(snippetText);
//                 items.push(snippet);
//             }
//         }
//         return items;
//     }
// }