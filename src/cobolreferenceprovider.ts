
import * as vscode from 'vscode';
import COBOLQuickParse, { SourceReference, COBOLToken, SharedSourceReferences } from './cobolquickparse';
import VSQuickCOBOLParse from './vscobolquickparse';

const wordRegEx: RegExp = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');

export class CobolReferenceProvider implements vscode.ReferenceProvider {
    public provideReferences(
        document: vscode.TextDocument, position: vscode.Position,
        options: { includeDeclaration: boolean }, token: vscode.CancellationToken):
        Thenable<vscode.Location[] | null> {

        this.currentVersion = 0;
        return this.processSearch(document, position);
    }

    private current?: COBOLQuickParse;
    private currentVersion?: number;
    private sourceRefs?: SharedSourceReferences;

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

        // cache current document, interatives search to be faster
        if (this.current === undefined || this.currentVersion !== document.version) {
            this.current = VSQuickCOBOLParse.getCachedObject(document);
            if (this.current !== undefined) {
                this.sourceRefs = this.current.sourceReferences;
                this.currentVersion = document.version;
            }
        }

        if (this.current === undefined || this.sourceRefs === undefined) {
            return Promise.resolve(null);
        }

        let qp: COBOLQuickParse = this.current;
        let sourceRefs: SharedSourceReferences = this.sourceRefs;

        if (qp.paragraphs.has(workLower) || qp.sections.has(workLower)) {
            let paraToken: COBOLToken | undefined = qp.paragraphs.get(workLower);
            if (paraToken !== undefined) {
                let qpsUrl: vscode.Uri = vscode.Uri.file(paraToken.filename);
                list.push(new vscode.Location(qpsUrl, new vscode.Position(paraToken.startLine, paraToken.startColumn)));
            }

            let sectionToken: COBOLToken | undefined = qp.sections.get(workLower);
            if (sectionToken !== undefined) {
                let qpsUrl: vscode.Uri = vscode.Uri.file(sectionToken.filename);
                list.push(new vscode.Location(qpsUrl, new vscode.Position(sectionToken.startLine, sectionToken.startColumn)));
            }
        }

        if (qp.constantsOrVariables.has(workLower)) {
            let paraTokens: COBOLToken[] | undefined = qp.constantsOrVariables.get(workLower);
            if (paraTokens !== undefined) {
                for (let ptref = 0; ptref < paraTokens.length; ptref++) {
                    let paraToken = paraTokens[ptref];
                    let qpsUrl: vscode.Uri = vscode.Uri.file(paraToken.filename);
                    list.push(new vscode.Location(qpsUrl, new vscode.Position(paraToken.startLine, paraToken.startColumn)));
                }
            }
        }

        if (sourceRefs.targetReferences.has(workLower) === true) {
            let targetRefs: SourceReference[] | undefined = sourceRefs.targetReferences.get(workLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    let tref = targetRefs[trpos];
                    list.push(new vscode.Location(sourceRefs.filenames[tref.fileIdentifer], new vscode.Position(tref.line, tref.columnn)));
                }
            }
        }

        if (sourceRefs.constantsOrVariablesReferences.has(workLower) === true) {
            let targetRefs: SourceReference[] | undefined = sourceRefs.constantsOrVariablesReferences.get(workLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    let tref = targetRefs[trpos];
                    list.push(new vscode.Location(sourceRefs.filenames[tref.fileIdentifer], new vscode.Position(tref.line, tref.columnn)));
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