
import * as vscode from 'vscode';
import { VSCodeSourceHandler } from './VSCodeSourceHandler';
import COBOLQuickParse, { SourceReference, COBOLToken } from './cobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import VSQuickCOBOLParse from './vscobolquickparse';

const wordRegEx: RegExp = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');

export class CobolReferenceProvider implements vscode.ReferenceProvider {
    private files: vscode.Uri[] = [];

    public provideReferences(
        document: vscode.TextDocument, position: vscode.Position,
        options: { includeDeclaration: boolean }, token: vscode.CancellationToken):
        Thenable<vscode.Location[] | null> {


        return this.processSearch(document, position);
    }

    private processSearch(
        document: vscode.TextDocument,
        position: vscode.Position): Thenable<vscode.Location[] | null> {
        let list: vscode.Location[] = [];
        let wordRange = document.getWordRangeAtPosition(position, wordRegEx);
        let word = wordRange ? document.getText(wordRange) : '';
        if (word === "") {
            return Promise.resolve(null);
        }

        let workLower = word.toLocaleLowerCase();

        let file = new VSCodeSourceHandler(document, false);
        let qp = new COBOLQuickParse(file, document.fileName, VSCOBOLConfiguration.get(), "", true);

        if (qp.paragraphs.has(workLower) || qp.sections.has(workLower)) {
            let paraToken: COBOLToken|undefined = qp.paragraphs.get(workLower);
            if (paraToken !== undefined) {
                list.push(new vscode.Location(document.uri, new vscode.Position(paraToken.startLine, paraToken.startColumn)));
            }

            let sectionToken: COBOLToken|undefined = qp.sections.get(workLower);
            if (sectionToken !== undefined) {
                list.push(new vscode.Location(document.uri, new vscode.Position(sectionToken.startLine, sectionToken.startColumn)));
            }

            if (qp.targetReferences.has(workLower) === true) {
                let targetRefs: SourceReference[] | undefined = qp.targetReferences.get(workLower);
                if (targetRefs === undefined) {
                    return Promise.resolve(null);
                }
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    let tref = targetRefs[trpos];
                    list.push(new vscode.Location(document.uri, new vscode.Position(tref.line, tref.columnn)));
                }
            }
        }

        if (list.length > 0) {
            return Promise.resolve(list);
        }
        else {
            return Promise.resolve(null);
        }
    }

}