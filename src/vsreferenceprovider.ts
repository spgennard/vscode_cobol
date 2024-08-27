
import * as vscode from "vscode";
import { COBOLSourceScanner, SourceReference_Via_Length, COBOLToken, SharedSourceReferences } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";

const wordRegEx = new RegExp("[#0-9a-zA-Z][a-zA-Z0-9-_]*");

export class CobolReferenceProvider implements vscode.ReferenceProvider {
    public provideReferences(
        document: vscode.TextDocument, position: vscode.Position,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        options: { includeDeclaration: boolean }, token: vscode.CancellationToken):
        Thenable<vscode.Location[] | null> {

        this.currentVersion = 0;
        return this.processSearch(document, position);
    }

    private current: COBOLSourceScanner | undefined;
    private currentVersion?: number;
    private sourceRefs?: SharedSourceReferences;

    private processSearch(
        document: vscode.TextDocument,
        position: vscode.Position): Thenable<vscode.Location[] | null> {
        const list: vscode.Location[] = [];
        const wordRange = document.getWordRangeAtPosition(position, wordRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return Promise.resolve(null);
        }

        const wordLower = word.toLowerCase();
        const settings = VSCOBOLConfiguration.get();
        // cache current document, so interactive searches can be faster
        if (this.current === undefined || this.currentVersion !== document.version) {
            const newCurrent = VSCOBOLSourceScanner.getCachedObject(document, settings);
            if (newCurrent !== undefined) {
                this.current = newCurrent;
                this.sourceRefs = this.current.sourceReferences;
                this.currentVersion = document.version;
            }
        }

        if (this.current === undefined || this.sourceRefs === undefined) {
            return Promise.resolve(null);
        }

        const qp: COBOLSourceScanner = this.current;
        const sourceRefs: SharedSourceReferences = this.sourceRefs;

        if (qp.paragraphs.has(wordLower) || qp.sections.has(wordLower)) {
            const paraToken: COBOLToken | undefined = qp.paragraphs.get(wordLower);
            if (paraToken !== undefined) {
                const qpsUrl: vscode.Uri = vscode.Uri.parse(paraToken.filenameAsURI);
                list.push(new vscode.Location(qpsUrl, new vscode.Position(paraToken.startLine, paraToken.startColumn)));
            }

            const sectionToken: COBOLToken | undefined = qp.sections.get(wordLower);
            if (sectionToken !== undefined) {
                const qpsUrl: vscode.Uri = vscode.Uri.parse(sectionToken.filenameAsURI);
                list.push(new vscode.Location(qpsUrl, new vscode.Position(sectionToken.startLine, sectionToken.startColumn)));
            }
        }

        // is the word, a known "exec sql" cursor declare
        if (qp.execSQLDeclare.has(wordLower) === true) {
            const sd = qp.execSQLDeclare.get(wordLower);
            if (sd !== undefined) {
                for (const tref of sd.sourceReferences) {
                    const uiref = vscode.Uri.parse(sourceRefs.filenameURIs[tref.fileIdentifer]);
                    const p1 = new vscode.Position(tref.line, tref.column);
                    const p2 = new vscode.Position(tref.endLine, tref.endColumn)
                    list.push(new vscode.Location(uiref, new vscode.Range(p1, p2)));
                }
            }
        }

        if (sourceRefs.targetReferences.has(wordLower) === true) {
            const targetRefs: SourceReference_Via_Length[] | undefined = sourceRefs.targetReferences.get(wordLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    const tref = targetRefs[trpos];
                    const uiref = vscode.Uri.parse(sourceRefs.filenameURIs[tref.fileIdentifer]);
                    list.push(new vscode.Location(uiref, new vscode.Position(tref.line, tref.column)));
                }
            }
        }

        if (sourceRefs.constantsOrVariablesReferences.has(wordLower) === true) {
            const targetRefs: SourceReference_Via_Length[] | undefined = sourceRefs.constantsOrVariablesReferences.get(wordLower);
            if (targetRefs !== undefined) {
                for (let trpos = 0; trpos < targetRefs.length; trpos++) {
                    const tref = targetRefs[trpos];
                    const uiref = vscode.Uri.parse(sourceRefs.filenameURIs[tref.fileIdentifer]);
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