
import * as vscode from 'vscode';
import COBOLSourceScanner, { SourceReference, COBOLToken, SharedSourceReferences } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './vsconfiguration';
import VSCOBOLSourceScanner from './vscobolscanner';

const wordRegEx = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');

export class CobolReferenceProvider implements vscode.ReferenceProvider {
    public provideReferences(
        document: vscode.TextDocument, position: vscode.Position,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        options: { includeDeclaration: boolean }, token: vscode.CancellationToken):
        Thenable<vscode.Location[] | null> {

        this.currentVersion = 0;
        return this.processSearch(document, position);
    }

    private current?: COBOLSourceScanner;
    private currentVersion?: number;
    private sourceRefs?: SharedSourceReferences;

    private processSearch(
        document: vscode.TextDocument,
        position: vscode.Position): Thenable<vscode.Location[] | null> {
        const list: vscode.Location[] = [];
        const wordRange = document.getWordRangeAtPosition(position, wordRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        if (word === "") {
            return Promise.resolve(null);
        }

        const workLower = word.toLowerCase();
        const settings = VSCOBOLConfiguration.get();
        // cache current document, so interactive searches can be faster
        if (this.current === undefined || this.currentVersion !== document.version) {
            this.current = VSCOBOLSourceScanner.getCachedObject(document, settings);
            if (this.current !== undefined) {
                this.sourceRefs = this.current.sourceReferences;
                this.currentVersion = document.version;
            }
        }

        if (this.current === undefined || this.sourceRefs === undefined) {
            return Promise.resolve(null);
        }

        const qp: COBOLSourceScanner = this.current;
        const sourceRefs: SharedSourceReferences = this.sourceRefs;

        if (qp.paragraphs.has(workLower) || qp.sections.has(workLower)) {
            const paraToken: COBOLToken | undefined = qp.paragraphs.get(workLower);
            if (paraToken !== undefined) {
                const qpsUrl: vscode.Uri = vscode.Uri.file(paraToken.filename);
                list.push(new vscode.Location(qpsUrl, new vscode.Position(paraToken.startLine, paraToken.startColumn)));
            }

            const sectionToken: COBOLToken | undefined = qp.sections.get(workLower);
            if (sectionToken !== undefined) {
                const qpsUrl: vscode.Uri = vscode.Uri.file(sectionToken.filename);
                list.push(new vscode.Location(qpsUrl, new vscode.Position(sectionToken.startLine, sectionToken.startColumn)));
            }
        }

        if (qp.constantsOrVariables.has(workLower)) {
            const paraTokens: COBOLToken[] | undefined = qp.constantsOrVariables.get(workLower);
            if (paraTokens !== undefined) {
                for (let ptref = 0; ptref < paraTokens.length; ptref++) {
                    const paraToken = paraTokens[ptref];
                    const qpsUrl: vscode.Uri = vscode.Uri.file(paraToken.filename);
                    list.push(new vscode.Location(qpsUrl, new vscode.Position(paraToken.startLine, paraToken.startColumn)));
                }
            }
        }

        if (sourceRefs.targetReferences.has(workLower) === true) {
            const targetRefs: SourceReference[] | undefined = sourceRefs.targetReferences.get(workLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    const tref = targetRefs[trpos];
                    const uiref = vscode.Uri.file(sourceRefs.filenames[tref.fileIdentifer]);
                    list.push(new vscode.Location(uiref, new vscode.Position(tref.line, tref.column)));
                }
            }
        }

        if (sourceRefs.constantsOrVariablesReferences.has(workLower) === true) {
            const targetRefs: SourceReference[] | undefined = sourceRefs.constantsOrVariablesReferences.get(workLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    const tref = targetRefs[trpos];
                    const uiref = vscode.Uri.file(sourceRefs.filenames[tref.fileIdentifer]);
                    list.push(new vscode.Location(uiref, new vscode.Position(tref.line, tref.column)));
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