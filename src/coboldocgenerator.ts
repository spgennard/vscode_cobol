import * as vscode from 'vscode';
import COBOLSourceScanner, { CobolDocStyle, CobolTagStyle } from './cobolsourcescanner';
import { logMessage, isDirectory, logChannelSetPreserveFocus } from './extension';
import VSCOBOLSourceScanner from './vscobolscanner';
import path from 'path';
import fs from 'fs';
import { exec } from 'child_process';
import { ICOBOLSettings } from './iconfiguration';
import { getWorkspaceFolders } from './cobolfolders';
import { parse } from 'node-html-parser';

export class COBOLDocumentationGenerator {

    private static getTempDirectory(settings: ICOBOLSettings): string {
        let firstCacheDir = "";
        const ws = getWorkspaceFolders();
        if (ws !== undefined) {
            for (const folder of ws) {
                const cacheDir2: string = path.join(folder.uri.fsPath, settings.coboldoc_workspace_folder);
                if (isDirectory(cacheDir2)) {
                    return cacheDir2;
                }
                if (firstCacheDir === "") {
                    firstCacheDir = cacheDir2;
                }
            }
        }

        if (firstCacheDir.length === 0) {
            return "";
        }

        if (isDirectory(firstCacheDir) === false) {
            try {
                fs.mkdirSync(firstCacheDir);
            }
            catch {
                return "";
            }
        }

        return firstCacheDir;
    }

    static cobolDocPanel: vscode.WebviewPanel | undefined = undefined;

    public static showCOBOLDOCDocumentation(activeTextEditor: vscode.TextEditor, settings: ICOBOLSettings): void {

        const sf: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(activeTextEditor.document);
        if (sf === undefined) {
            return;
        }

        const tagStype = sf.commentTagStyle === CobolTagStyle.FREE ? "-s free" : "-s microfocus";
        const tmpArea = COBOLDocumentationGenerator.getTempDirectory(settings);
        if (tmpArea === "") {
            logChannelSetPreserveFocus(false);
            logMessage("Unable to find or create a coboldoc directory");

            return;
        }
        const textToSend = sf.commentDocStyle === CobolDocStyle.MSDN ?
            `coboldoc -o ${tmpArea} ${tagStype} -a msdn -f html generate ${sf.filename}` :
            `coboldoc -o ${tmpArea} ${tagStype} -a tag -f html generate ${sf.filename}`;

        exec(textToSend, (error, stdout, stderr) => {
            if (error) {
                logChannelSetPreserveFocus(false);
                logMessage(`coboldoc: [${error.message}]`);
                return;
            }
            if (stderr) {
                logChannelSetPreserveFocus(false);
                logMessage(`coboldoc: [${stderr}]`);
                return;
            }

            logMessage(`coboldoc: ${stdout}`);

            const shortFilename = path.basename(sf.filename);
            const htmlFilename = `${tmpArea}/${shortFilename}.html`;

            const htmlContents = COBOLDocumentationGenerator.fixupHTML(fs.readFileSync(htmlFilename).toString());
            if (COBOLDocumentationGenerator.cobolDocPanel === undefined) {
                COBOLDocumentationGenerator.cobolDocPanel = vscode.window.createWebviewPanel("coboldoc", "COBOL Documentation",
                    vscode.ViewColumn.Beside, { enableScripts: true });

                COBOLDocumentationGenerator.cobolDocPanel.onDidDispose(() => {
                    COBOLDocumentationGenerator.cobolDocPanel = undefined;
                });
            }

            COBOLDocumentationGenerator.cobolDocPanel.title = `coboldoc: ${shortFilename}`;
            COBOLDocumentationGenerator.cobolDocPanel.webview.html = htmlContents;
        });
    }

    private static fixupHTML(html: string) : string {
        const root = parse(html);

        const style = root.querySelector("style");
        style.set_content('\
        table {\
            font-family: "Courier New", Courier, monospace;\
            border: 2px solid #FFFFFF;\
            border-collapse: collapse;\
            width: 90%;\
          }\
          table th {\
            border: 4px solid #0B6FA4;\
            padding: 3px 2px;\
            background: #0B6FA4; \
          }\
          table td {\
            border: 4px solid #0B6FA4;\
            padding: 3px 2px;\
          }\
          }');

        return root.toString();
    }
}