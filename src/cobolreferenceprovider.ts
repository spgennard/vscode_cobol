
import * as vscode from 'vscode';
import { VSCodeSourceHandler } from './VSCodeSourceHandler';
import COBOLQuickParse, { SourceReference, COBOLToken, SharedSourceReferences } from './cobolquickparse';
import { VSCOBOLConfiguration } from './configuration';
import VSQuickCOBOLParse from './vscobolquickparse';
import { expandLogicalCopyBookToFilenameOrEmpty } from './opencopybook';
import { logException } from './extension';
import { FileSourceHandler } from './FileSourceHandler';

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
        let sourceRefs: SharedSourceReferences = new SharedSourceReferences();
        let qp: COBOLQuickParse;

        // cache current document, interatives search to be faster
        if (this.current === undefined || this.currentVersion !== document.version) {
            qp = new COBOLQuickParse(file, document.fileName, VSCOBOLConfiguration.get(), "", sourceRefs);
        } else {
            qp = this.current;
        }

        if (qp !== this.current) {
            this.currentVersion = document.version;
            this.current = qp;
        }

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