import * as vscode from 'vscode';
import { CobolDocStyle, CobolTagStyle } from './cobolsourcescanner';
import { logMessage } from './extension';
import VSQuickCOBOLParse from './vscobolscanner';
import path from 'path';
import { exec } from 'child_process';


export class COBOLDocumentationGenerator {

    public static showCOBOLDOCDocumentation(activeTextEditor: vscode.TextEditor):void {

        const sf = VSQuickCOBOLParse.getCachedObject(activeTextEditor.document);
        if (sf === undefined) {
            return;
        }

        const tagStype = sf.commentTagStyle === CobolTagStyle.FREE ? "-s free" : "-s microfocus";
        const tmpArea = "/tmp/coboldoc";
        const textToSend = sf.commentStyle === CobolDocStyle.MSDN ?
                    `coboldoc -o ${tmpArea} ${tagStype} -a msdn -f md generate ${sf.filename}` :
                    `coboldoc -o ${tmpArea} ${tagStype} -a tag -f md generate ${sf.filename}`;

        exec(textToSend,  (error, stdout, stderr) => {
            if (error) {
                logMessage(`coboldoc: [${error.message}]`);
                return;
            }
            if (stderr) {
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