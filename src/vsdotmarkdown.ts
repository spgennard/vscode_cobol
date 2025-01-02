import * as vscode from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { COBOLSourceScanner, COBOLToken, COBOLTokenStyle, ParseState, SourceReference_Via_Length } from "./cobolsourcescanner";

function generate_partial_graph(linesArray: string[], state: ParseState, para_or_section: Map<string, COBOLToken>) {
    for (const [paragraph, targetToken] of para_or_section) {
        const wordLower = paragraph.toLowerCase();
        const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);
        if (targetRefs !== undefined) {
            if (targetToken.isImplicitToken) {
                linesArray.push(`${targetToken.tokenNameLower}[${targetToken.description}]`);
            }

            for (const sr of targetRefs) {
                // skip definition
                if (sr.line === targetToken.startLine && sr.column === targetToken.startColumn) {
                    continue;
                }

                if (sr.tokenStyle === COBOLTokenStyle.Paragraph || sr.tokenStyle === COBOLTokenStyle.Section) {
                    if (sr.reason === 'perform') {
                        linesArray.push(`${targetToken.tokenNameLower} --> ${sr.nameLower}`);
                    } else {
                        linesArray.push(`${targetToken.tokenNameLower} -->|${sr.reason}|${sr.nameLower}`);
                    }
                }
            }
        }
    }
}

export async function view_dot_callgraph(context: vscode.ExtensionContext, settings: ICOBOLSettings) {
    if (!vscode.window.activeTextEditor) {
        return;
    }

    const current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, settings);
    if (current === undefined) {
        return;
    }

    const linesArray: string[] = getCurrentProgramCallGraph(settings, current, false);

    const webviewPanel = vscode.window.createWebviewPanel(
        'vscodeTest',
        'VsCode test webview',
        {
            viewColumn: vscode.ViewColumn.Beside,
            preserveFocus: true
        },
        {
            enableScripts: true
        }
    );
    webviewPanel.webview.onDidReceiveMessage(
        message => {
            switch (message.command) {
                case 'saveAsPng':
                    saveAsPng(message.text);
                    return;
            }
        },
        undefined,
        context.subscriptions
    );
    setHtmlContent(webviewPanel.webview, context, linesArray);
}

function saveAsPng(messageText: string) {
    const dataUrl = messageText.split(',');
    return dataUrl;
}

function setHtmlContent(webview: vscode.Webview, extensionContext: vscode.ExtensionContext, linesArray: string[]) {
    let htmlContent = `<html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
     <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src cspSource; script-src 'nonce-nonce';">
    <link href="webview.css" rel="stylesheet">
  </head>
  <body>
    <div id="buttons">
      <input type="button" id="saveAsPngButton" value="Save as png">
    </div>
    <script type="text/javascript" src="body.js"></script>
    <script type="text/javascript" src="mermaid.min.js"></script>
    <script>mermaid.initialize({startOnLoad:true});</script>

    <div class="mermaid">
    ${linesArray.join("\n")}
    </div>

  </body>
</html>`;
    const jsFilePath = vscode.Uri.joinPath(extensionContext.extensionUri, 'web', 'body.js');
    const visUri = webview.asWebviewUri(jsFilePath);
    htmlContent = htmlContent.replace('body.js', visUri.toString());

    const jsMermaidPath = vscode.Uri.joinPath(extensionContext.extensionUri, 'web', 'mermaid.min.js');
    const jsMerVis = webview.asWebviewUri(jsMermaidPath);
    htmlContent = htmlContent.replace('mermaid.min.js', jsMerVis.toString());


    const cssPath = vscode.Uri.joinPath(extensionContext.extensionUri, 'css', 'webview.css');
    const cssUri = webview.asWebviewUri(cssPath);
    htmlContent = htmlContent.replace('webview.css', cssUri.toString());

    const nonce = getNonce();
    htmlContent = htmlContent.replace('nonce-nonce', `nonce-${nonce}`);
    htmlContent = htmlContent.replace(/<script /g, `<script nonce="${nonce}" `);
    htmlContent = htmlContent.replace('cspSource', webview.cspSource);

    webview.html = htmlContent;
}

function getNonce() {
    let text = '';
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    for (let i = 0; i < 32; i++) {
        text += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    return text;
}

function getCurrentProgramCallGraph(settings: ICOBOLSettings, current: COBOLSourceScanner, asMarkdown: boolean) {
    const linesArray: string[] = [];
    const state = current.sourceReferences.state;

    if (asMarkdown) {
        if (current.ImplicitProgramId.length !== 0) {
            linesArray.push("# " + current.ImplicitProgramId);
        } else {
            if (current.ProgramId.length !== 0) {
                linesArray.push("# " + current.ProgramId);
            } else {
                linesArray.push(" # Unknown");
            }
        }

        linesArray.push("");
        linesArray.push("```mermaid");
    }
    linesArray.push("graph TD;");

    generate_partial_graph(linesArray, state, current.sections);
    generate_partial_graph(linesArray, state, current.paragraphs);

    if (asMarkdown) {
        linesArray.push("```")
    }

    return linesArray;
}

export async function newFile_dot_callgraph(settings: ICOBOLSettings) {
    if (!vscode.window.activeTextEditor) {
        return;
    }

    const current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, settings);
    if (current === undefined) {
        return;
    }
    const doclang = "markdown";
    const linesArray: string[] = getCurrentProgramCallGraph(settings, current, true);
    vscode.workspace.openTextDocument({ language: "markdown" }).then(async document => {
        vscode.window.showTextDocument(document);
        const editor = await vscode.window.showTextDocument(document);
        if (editor !== undefined) {
            const linesAsOne = linesArray.join("\n");
            await editor.insertSnippet(new vscode.SnippetString(linesAsOne), new vscode.Range(0, 0, 1 + linesArray.length, 0));
            await vscode.languages.setTextDocumentLanguage(document, doclang);
        }
        // await document.save();
    });
}
