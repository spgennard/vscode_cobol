import * as vscode from 'vscode';

export class COBOLHierarchyProvider implements vscode.CallHierarchyProvider {
    prepareCallHierarchy(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): vscode.ProviderResult<vscode.CallHierarchyItem | vscode.CallHierarchyItem[]> {
        const range = document.getWordRangeAtPosition(position);
        if (range) {
            const word = document.getText(range);
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

        return results;
    }
}