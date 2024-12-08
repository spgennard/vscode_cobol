import * as vscode from 'vscode';
import { COBOLSourceScanner, COBOLToken, COBOLTokenStyle } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './vsconfiguration';
import { VSExternalFeatures } from './vsexternalfeatures';
import { VSCOBOLSourceScanner } from './vscobolscanner';

export class COBOLHierarchyProvider implements vscode.CallHierarchyProvider {
    private current: COBOLSourceScanner | undefined;

    prepareCallHierarchy(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CallHierarchyItem | vscode.CallHierarchyItem[]> {
        const range = document.getWordRangeAtPosition(position);
        if (range) {
            const word = document.getText(range);
            const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
            this.current = VSCOBOLSourceScanner.getCachedObject(document, settings);
            return this.createCallHierarchyItem(word, '', document, range);
        } else {
            return undefined;
        }
    }

    private createCallHierarchyItem(word: string, type: string, document: vscode.TextDocument, range: vscode.Range): vscode.CallHierarchyItem {
        return new vscode.CallHierarchyItem(vscode.SymbolKind.Method, word, `(${type})`, document.uri, range, range);
    }



    provideCallHierarchyIncomingCalls(item: vscode.CallHierarchyItem, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CallHierarchyIncomingCall[]> {
        let results: vscode.CallHierarchyIncomingCall[] = []

        return results;
    }
    
    provideCallHierarchyOutgoingCalls(item: vscode.CallHierarchyItem, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CallHierarchyOutgoingCall[]> {

        let results: vscode.CallHierarchyOutgoingCall[] = []
        let word = item.name;
        let wordLower = word.toLowerCase();

        if (this.current === undefined) {
            return results;
        }

        const qp: COBOLSourceScanner = this.current;

        if (qp.paragraphs.has(wordLower) || qp.sections.has(wordLower)) {
            // const paraToken: COBOLToken | undefined = qp.paragraphs.get(wordLower);
            // if (paraToken !== undefined) {
            //     const range = new vscode.Range(new vscode.Position(paraToken.startLine, paraToken.startColumn),
            //                     new vscode.Position(paraToken.endLine, paraToken.endColumn));
            //     const newitem = new vscode.CallHierarchyItem(vscode.SymbolKind.Method, "@"+word, "", item.uri, range, range);

            //     let call = new vscode.CallHierarchyIncomingCall(newitem,[range]);
            //     results.push(call);
            // }

            let inToken: COBOLToken | undefined = qp.sections.get(wordLower);
            if (inToken === undefined) {
                inToken = qp.paragraphs.get(wordLower);
            }
            if (inToken !== undefined) {
                const state = qp.sourceReferences.state;
                if (state.currentSectionInRefs !== undefined && state.currentSectionInRefs.has(inToken.tokenName)) {
                    const srefs = state.currentSectionInRefs.get(inToken.tokenName);
                    if (srefs !== undefined) {
                        for (const sr of srefs) {
                            if (sr.name.toLowerCase() === wordLower) {
                                continue
                            }
                            if (sr.tokenStyle === COBOLTokenStyle.Section || sr.tokenStyle === COBOLTokenStyle.Paragraph) {
                                const range = new vscode.Range(new vscode.Position(sr.line, sr.column),
                                    new vscode.Position(sr.line, sr.column + sr.length));
                                const newitem = new vscode.CallHierarchyItem(vscode.SymbolKind.Method, sr.name, "", item.uri, range, range);
                                let call = new vscode.CallHierarchyOutgoingCall(newitem, [range]);
                                results.push(call);
                            }
                        }
                    }
                }

            }
        }
        return results;
    }

}