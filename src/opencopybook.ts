"use strict";

import * as path from "path";

import * as vscode from "vscode";
import { Range, TextDocument, Definition, Position, CancellationToken, Uri } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { COBOLSourceScanner } from "./cobolsourcescanner";
import { VSCOBOLFileUtils } from "./vsfileutils";
import { IExternalFeatures } from "./externalfeatures";



export class COBOLCopyBookProvider implements vscode.DefinitionProvider {

    private features: IExternalFeatures;

    constructor(features: IExternalFeatures) {
        this.features = features;
    }

    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
        return this.resolveDefinitions(document, position, token);
    }

    private async resolveDefinitions(document: TextDocument, pos: Position, ct: CancellationToken): Promise<Definition> {
        const locations: vscode.Location[] = [];

        const config = VSCOBOLConfiguration.get();
        const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, config);
        if (qcp === undefined) {
            return this.resolveDefinitionsFallback(document, pos, ct);
        }

        for (const [, b] of qcp.copyBooksUsed) {
            const st = b.statementInformation;
            if (st.fileName.length !== 0) {
                const stPos = new Range(new Position(st.startLineNumber, st.startCol), new Position(st.endLineNumber, st.endCol));

                if (stPos.contains(pos)) {
                    return new vscode.Location(
                        Uri.file(st.fileName),
                        new Range(new Position(0, 0), new Position(0, 0))
                    );
                }
            }
        }

        return locations;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async resolveDefinitionsFallback(doc: TextDocument, pos: Position, ct: CancellationToken): Promise<Definition> {
        const config = VSCOBOLConfiguration.get();
        const line = doc.lineAt(pos);
        const text = line.text;
        const textLower = text.toLowerCase().replace("\t", " ");
        let filename = this.extractCopyBoolFilename(text);
        let inOrOfPos = 0;

        const activeTextEditor = vscode.window.activeTextEditor;
        if (activeTextEditor && filename === undefined) {
            const sel = activeTextEditor.selection;

            const ran = new vscode.Range(sel.start, sel.end);
            const textSelection = activeTextEditor.document.getText(ran);
            if (textSelection !== undefined) {
                filename = textSelection.trim();
            }
        }

        // leave asap
        if (filename === undefined) {
            return [];
        }

        // exec sql include "has" in in the line.. so take care..
        if (textLower.indexOf("copy") !== -1) {
            inOrOfPos = textLower.indexOf(" in ");
            if (inOrOfPos === -1) {
                inOrOfPos = textLower.indexOf(" of ");
            }
            inOrOfPos !== -1 ? inOrOfPos += 4 : inOrOfPos = 0;
        }

        let inDirectory = inOrOfPos !== 0 ? text.substr(inOrOfPos) : "";

        if (inDirectory.length !== 0) {
            let inDirItems = inDirectory.trim();

            if (inDirItems.endsWith(".")) {
                inDirItems = inDirItems.substr(0, inDirItems.length - 1);
            }

            if (inDirItems.endsWith("\"") && inDirItems.startsWith("\"")) {
                inDirItems = inDirItems.substr(1, inDirItems.length - 2);
            }

            if (inDirItems.endsWith("'") && inDirItems.startsWith("'")) {
                inDirItems = inDirItems.substr(1, inDirItems.length - 2);
            }

            inDirectory = inDirItems;
        }

        if (filename !== null && filename.length !== 0) {
            const fullPath = COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(filename.trim(), inDirectory, config, this.features);
            if (fullPath.length !== 0) {
                return new vscode.Location(
                    Uri.file(fullPath),
                    new Range(new Position(0, 0), new Position(0, 0))
                );
            }
        }

        return [];

    }

    public static expandLogicalCopyBookOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings, features: IExternalFeatures): string {

        if (inDirectory === null || inDirectory.length === 0) {
            const fullPath = VSCOBOLFileUtils.findCopyBook(filename, config, features);
            if (fullPath.length !== 0) {
                return path.normalize(fullPath);
            }

            return fullPath;
        }

        const fullPath = VSCOBOLFileUtils.findCopyBookInDirectory(filename, inDirectory, config, features);
        if (fullPath.length !== 0) {
            return path.normalize(fullPath);
        }

        return fullPath;
    }

    private extractCopyBoolFilename(str: string): string | undefined {
        const copyPos = str.toLowerCase().indexOf("copy");
        if (copyPos !== -1) {
            const noCopyStr = str.substr(4 + copyPos).trimStart();
            const spacePos = noCopyStr.indexOf(" ");
            let justCopyArg = noCopyStr;
            if (spacePos !== -1) {
                justCopyArg = justCopyArg.substr(0, spacePos).trim();
            }

            // remove trailing .
            if (justCopyArg.endsWith(".")) {
                justCopyArg = justCopyArg.substr(0, justCopyArg.length - 1);
                justCopyArg = justCopyArg.trim();
            }

            // remove double quote
            if (justCopyArg.startsWith("\"") && justCopyArg.endsWith("\"")) {
                justCopyArg = justCopyArg.substr(1, justCopyArg.length - 2);
                justCopyArg = justCopyArg.trim();
                return justCopyArg;
            }

            // remove single quote
            if (justCopyArg.startsWith("'") && justCopyArg.endsWith("'")) {
                justCopyArg = justCopyArg.substr(1, justCopyArg.length - 2);
                justCopyArg = justCopyArg.trim();
            }

            return justCopyArg;
        }

        const strLower = str.toLowerCase();
        if (strLower.indexOf("exec") !== -1) {
            if (strLower.includes("sql", strLower.indexOf("exec"))) {
                let includePos = strLower.indexOf("include");
                if (includePos !== -1) {
                    includePos += 7;
                    const strRight = str.substr(includePos).trimStart();
                    const strRightLower = strRight.toLowerCase();
                    const endExecPos = strRightLower.indexOf("end-exec");
                    if (endExecPos !== -1) {
                        const filename = strRight.substr(0, endExecPos).trim();

                        return filename;
                    }
                }
            }
        }
        return undefined;
    }

}
