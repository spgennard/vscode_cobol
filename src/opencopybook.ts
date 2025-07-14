"use strict";

import * as path from "path";

import * as vscode from "vscode";
import { Range, TextDocument, Definition, Position, CancellationToken, Uri } from "vscode";
import { IVSCOBOLSettings, VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLFileUtils } from "./vsfileutils";
import { IExternalFeatures } from "./externalfeatures";
import { VSLogger } from "./vslogger";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { ICOBOLSourceScanner } from "./icobolsourcescanner";

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

    private async getURIForCopybook(copybook: string, externalfeatures: IExternalFeatures, settings: ICOBOLSettings): Promise<vscode.Uri | undefined> {
        try {
            const uriString = await VSCOBOLFileUtils.findCopyBookViaURL(copybook, settings, this.features)
            return uriString.length === 0 ? undefined : Uri.parse(uriString);
        } catch (e) {
            VSLogger.logException("getURIForCopybook", e as Error);
        }

        return undefined;
    }

    private async getURIForCopybookInDirectory(copybook: string, inDirectory: string, externalfeatures: IExternalFeatures, settings: ICOBOLSettings): Promise<vscode.Uri | undefined> {
        try {
            const uriString = await VSCOBOLFileUtils.findCopyBookInDirectoryViaURL(copybook, inDirectory, settings, this.features)
            return uriString.length === 0 ? undefined : Uri.parse(uriString);
        } catch (e) {
            VSLogger.logException("getURIForCopybook", e as Error);
        }

        return undefined;
    }

    private async resolveDefinitions(document: TextDocument, pos: Position, ct: CancellationToken): Promise<Definition> {
        const locations: vscode.Location[] = [];

        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
        const qcp: ICOBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, config);
        if (qcp === undefined) {
            return this.resolveDefinitionsFallback(true, document, pos, ct);
        }

        for (const [, b] of qcp.copyBooksUsed) {
            const st = b.statementInformation;
            if (st !== undefined) {
                const stPos = new Range(new Position(st.startLineNumber, st.startCol), new Position(st.endLineNumber, st.endCol));
                if (stPos.contains(pos)) {
                    if (document.uri.scheme === "file") {
                        if (st.fileName.length !== 0) {
                            return new vscode.Location(
                                Uri.file(st.fileName),
                                new Range(new Position(0, 0), new Position(0, 0))
                            );
                        }
                    } else {
                        if (st.isIn) {
                            const uri4copybook = await this.getURIForCopybookInDirectory(st.copyBook, st.literal2, VSExternalFeatures, config);
                            if (uri4copybook !== undefined) {
                                return new vscode.Location(
                                    uri4copybook,
                                    new Range(new Position(0, 0), new Position(0, 0))
                                );
                            }
                        } else {
                            const uri4copybook = await this.getURIForCopybook(st.copyBook, VSExternalFeatures, config);
                            if (uri4copybook !== undefined) {
                                return new vscode.Location(
                                    uri4copybook,
                                    new Range(new Position(0, 0), new Position(0, 0))
                                );
                            }
                        }
                    }
                }
            }
        }

        if (locations.length !== 0) {
            return locations;
        }

        return this.resolveDefinitionsFallback(false, document, pos, ct);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async resolveDefinitionsFallback(everything: boolean, doc: TextDocument, pos: Position, ct: CancellationToken): Promise<Definition> {
        const config = VSCOBOLConfiguration.get_resource_settings(doc, VSExternalFeatures);
        const line = doc.lineAt(pos);
        const text = line.text;
        const textLower = text.toLowerCase().replace("\t", " ");
        let filename = this.extractCopyBookFilename(everything, config, text);
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

        var sourceFilename = doc.fileName;
        if (filename !== null && filename.length !== 0) {
            const fullPath = COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(filename.trim(), inDirectory, config, sourceFilename, this.features);
            if (fullPath.length !== 0) {
                return new vscode.Location(
                    Uri.file(fullPath),
                    new Range(new Position(0, 0), new Position(0, 0))
                );
            }
        }

        const sourceDepPos = text.indexOf(config.scan_comment_copybook_token.toLowerCase());
        if (sourceDepPos !== -1) {
            const fileregex = new RegExp("[-9a-zA-Z\/\\ \.:_]+");
            const wordRange = doc.getWordRangeAtPosition(pos, fileregex);
            const wordText = wordRange ? doc.getText(wordRange) : "";
            if (wordText !== undefined && wordText.length !== 0) {
                for (const possibleFilename of wordText.split(" ")) {
                    const fullPath = COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(possibleFilename.trim(), inDirectory, config, sourceFilename, this.features);
                    if (fullPath.length !== 0) {
                        if (this.features.isFile(fullPath) && !this.features.isDirectory(fullPath)) {
                            return new vscode.Location(
                                Uri.file(fullPath),
                                new Range(new Position(0, 0), new Position(0, 0))
                            );
                        }
                    }
                }
            }
        }

        return [];

    }

    public static expandLogicalCopyBookOrEmpty(filename: string, inDirectory: string, config: IVSCOBOLSettings, sourceFilename: string, features: IExternalFeatures): string {

        if (config.perfile_copybookdirs.length !== 0) {
            // fileDirname
            var fileDirname = path.dirname(sourceFilename);
            for (var _perCopydir of config.perfile_copybookdirs) {
                var perFileDir = _perCopydir.replace("${fileDirname}", fileDirname);
                /* check for the file as is.. */
                const firstPossibleFile = path.join(perFileDir, filename);
                if (features.isFile(firstPossibleFile)) {
                    return firstPossibleFile;
                }

                const hasDot = filename.indexOf(".");
                /* no extension? */
                if (hasDot === -1) {
                    // search through the possible extensions
                    for (const ext of config.copybookexts) {
                        const possibleFile = path.join(perFileDir, filename + "." + ext);

                        if (features.isFile(possibleFile)) {
                            return possibleFile;
                        }
                    }
                }
            }
        }

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

    private extractCopyBookFilename(everything: boolean, config: ICOBOLSettings, str: string): string | undefined {
        const strLower = str.toLowerCase();

        if (everything) {
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
        }

        return undefined;
    }

}
