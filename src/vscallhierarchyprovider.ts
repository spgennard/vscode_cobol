import * as vscode from 'vscode';
import { COBOLSourceScanner, COBOLToken, COBOLTokenStyle, SharedSourceReferences, SourceReference_Via_Length } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './vsconfiguration';
import { VSExternalFeatures } from './vsexternalfeatures';
import { VSCOBOLSourceScanner } from './vscobolscanner';

export class COBOLHierarchyProvider implements vscode.CallHierarchyProvider {
    private current: COBOLSourceScanner | undefined

    prepareCallHierarchy(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CallHierarchyItem | vscode.CallHierarchyItem[]> {
        let range = document.getWordRangeAtPosition(position);
        if (range) {
            let word = document.getText(range);
            // let wordLower = word.toLowerCase();wordLower
            const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
            if (settings.enable_call_hierarchy === false) {
                return undefined;
            }
            
            this.current = VSCOBOLSourceScanner.getCachedObject(document, settings);
            let detail = "";
            if (this.current !== undefined) {
                const sourceRefs: SharedSourceReferences = this.current.sourceReferences;
                if (sourceRefs.targetReferences.has(word.toLowerCase()) === false) {
                    const foundToken = this.current.findNearestSectionOrParagraph(position.line);
                    if (foundToken !== undefined) {
                        word = foundToken.description;
                        // wordLower = foundToken.tokenNameLower;
                        detail = `@${1+foundToken.startLine}`;
                        range = new vscode.Range(new vscode.Position(foundToken.rangeStartLine, foundToken.rangeStartColumn),
                                                 new vscode.Position(foundToken.rangeEndLine, foundToken.rangeEndColumn));
                    }
                }

                // if (sourceRefs.targetReferences.has(wordLower) === true) {
                return new vscode.CallHierarchyItem(vscode.SymbolKind.Method, word, detail, document.uri, range, range);
                // }
            }
        }
        return undefined;
    }

    provideCallHierarchyIncomingCalls(item: vscode.CallHierarchyItem, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CallHierarchyIncomingCall[]> {
        let results: vscode.CallHierarchyIncomingCall[] = []

        if (this.current === undefined) {
            return results;
        }

        const sourceRefs: SharedSourceReferences = this.current.sourceReferences;
        let wordLower = item.name.toLowerCase();

        if (sourceRefs.targetReferences.has(wordLower) === true) {
            const targetRefs: SourceReference_Via_Length[] | undefined = sourceRefs.targetReferences.get(wordLower);
            let targetToken = this.current.sections.get(wordLower);
            if (targetToken === undefined) {
                targetToken = this.current.paragraphs.get(wordLower);
            }
            if (targetToken === undefined) {
                return results;
            }

            this.current.paragraphs
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    const sr = targetRefs[trpos];

                    // skip definition
                    if (sr.line === targetToken.startLine && sr.column === targetToken.startColumn) {
                        continue;
                    }

                    const uiref = vscode.Uri.parse(sourceRefs.filenameURIs[sr.fileIdentifer]);
                    const range = new vscode.Range(new vscode.Position(sr.line, sr.column),
                        new vscode.Position(sr.line, sr.column + sr.length));
                    const r = sr.reason.length == 0 ? sr.name : sr.reason+" "+sr.name;
                    const d = `@${1+sr.line}`;
                    const newitem = new vscode.CallHierarchyItem(vscode.SymbolKind.Method, r, d, uiref, range, range);
                    let call = new vscode.CallHierarchyIncomingCall(newitem, [range]);
                    results.push(call);
                }
            }

            return results;
        }

        return undefined;
    }

    provideCallHierarchyOutgoingCalls(item: vscode.CallHierarchyItem, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CallHierarchyOutgoingCall[]> {

        let results: vscode.CallHierarchyOutgoingCall[] =   []
        let wordLower = item.name.toLowerCase();

        if (this.current === undefined) {
            return results;
        }

        const qp: COBOLSourceScanner = this.current;

        if (qp.paragraphs.has(wordLower) || qp.sections.has(wordLower)) {
            let inToken: COBOLToken | undefined = qp.sections.get(wordLower);
            if (inToken === undefined) {
                inToken = qp.paragraphs.get(wordLower);
            }
            if (inToken !== undefined) {
                const state = qp.sourceReferences.state;
                if (state.currentSectionOutRefs !== undefined && state.currentSectionOutRefs.has(inToken.tokenNameLower)) {
                    const srefs = state.currentSectionOutRefs.get(inToken.tokenNameLower);
                    if (srefs !== undefined) {
                        for (const sr of srefs) {
                            if (sr.nameLower === wordLower) {
                                continue
                            }
                            if (sr.tokenStyle === COBOLTokenStyle.Section || sr.tokenStyle === COBOLTokenStyle.Paragraph) {
                                const range = new vscode.Range(new vscode.Position(sr.line, sr.column),
                                    new vscode.Position(sr.line, sr.column + sr.length));
                                const r = `@${1+sr.line}`;
                                const newitem = new vscode.CallHierarchyItem(vscode.SymbolKind.Method, sr.name, r, item.uri, range, range);
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