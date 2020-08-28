import * as vscode from 'vscode';
import { CobolDocStyle, CobolTagStyle } from './cobolsourcescanner';
import { logMessage, isDirectory, logChannelSetPreserveFocus } from './extension';
import VSQuickCOBOLParse from './vscobolscanner';
import path from 'path';
import fs from 'fs';
import { exec } from 'child_process';
import { ICOBOLSettings } from './iconfiguration';
import { getWorkspaceFolders } from './cobolfolders';


export class COBOLDocumentationGenerator {

    private static getTempDirectory(settings: ICOBOLSettings) : string {
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


    public static showCOBOLDOCDocumentation(activeTextEditor: vscode.TextEditor, settings: ICOBOLSettings):void {

        const sf = VSQuickCOBOLParse.getCachedObject(activeTextEditor.document);
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
        const textToSend = sf.commentStyle === CobolDocStyle.MSDN ?
                    `coboldoc -o ${tmpArea} ${tagStype} -a msdn -f md generate ${sf.filename}` :
                    `coboldoc -o ${tmpArea} ${tagStype} -a tag -f md generate ${sf.filename}`;

        exec(textToSend,  (error, stdout, stderr) => {
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
            const openPreview = "markdown.showPreview";
            const shortFilename = path.basename(sf.filename);
            vscode.commands.executeCommand("vscode.open", vscode.Uri.parse(`${tmpArea}/${shortFilename}.md`)).then(() =>
                vscode.commands.executeCommand(openPreview)
                );
        });
    }
}