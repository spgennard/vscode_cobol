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

export class DotGraphPanelView {
    public static currentPanel: DotGraphPanelView | undefined;
    private readonly _panel: vscode.WebviewPanel;
    private _disposables: vscode.Disposable[] = [];

    private constructor(context: vscode.ExtensionContext, panel: vscode.WebviewPanel, linesArray: string[]) {
        // ... other code ...
        this._panel = panel;
        this._panel.webview.html = this._getWebviewContent(context, panel, linesArray);
    
        this._panel.onDidDispose(() => this.dispose(), null, this._disposables);
    }

    public static render(context: vscode.ExtensionContext, linesArray: string[]) {
        if (DotGraphPanelView.currentPanel) {
            DotGraphPanelView.currentPanel._panel.reveal(vscode.ViewColumn.Beside,true);
            DotGraphPanelView.currentPanel._panel.webview.html = DotGraphPanelView.currentPanel._getWebviewContent(context, DotGraphPanelView.currentPanel._panel, linesArray);

            return DotGraphPanelView.currentPanel._panel;
        } else {
            const resourcesDirectory = vscode.Uri.joinPath(context.extensionUri, 'resources')

            const panel = vscode.window.createWebviewPanel(
                'cobolCallGraph',
                'COBOL Call Graph',
                {
                    viewColumn: vscode.ViewColumn.Beside,
                    preserveFocus: true
                },
                {
                    enableScripts: true,
                    localResourceRoots: [resourcesDirectory]
                }
            );
            DotGraphPanelView.currentPanel = new DotGraphPanelView(context, panel, linesArray);
            return panel;
        }
    }
    private _getWebviewContent(context: vscode.ExtensionContext, panel: vscode.WebviewPanel, linesArray: string[]) {
        let htmlContent = `<!DOCTYPE html>
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="Content-Security-Policy" 
        content="default-src 'none'; 
        style-src cspSource 'unsafe-inline';
        script-src 'nonce-nonce' 'unsafe-inline';
        ">
    <style type="text/css">
        .diagram-container {
            width: 100%;
            height: 100%;
            overflow: hidden;
            border: 1px solid #ccc;
            position: relative;
            margin-bottom: 10px;
        }
        svg {
            cursor: grab;
        }
    </style>
  </head>
  <body class="vscode-body">
    <script type="text/javascript">
      const _config = {
        vscode: {
          minimap: false,
          dark: 'dark',
          light: 'neutral'
        }
      };
    </script>
    
    <script src="panzoom.min.js"></script>

    <script type="module">
      import mermaid from 'mermaid.esm.min.mjs';

      let config = { startOnLoad: false, useMaxWidth: true, theme: "neutral" };
      mermaid.initialize(config);
      await mermaid.run({
        querySelector: '.mermaid',
        postRenderCallback: (id) => {
            const container = document.getElementById("diagram-container");
            const svgElement = container.querySelector("svg");

            // Initialize Panzoom
            const panzoomInstance = Panzoom(svgElement, {
                maxScale: 5,
                minScale: 0.5,
                step: 0.1,
            });

            // Add mouse wheel zoom
            container.addEventListener("wheel", (event) => {
                panzoomInstance.zoomWithWheel(event);
            });
        }
      });
    </script>

    <div class="diagram-container" id="diagram-container">
    <div class="mermaid">
    ${linesArray.join("\n")}
    </div>
    </div>

  </body>
</html>`;

        const jsMermaidPath = vscode.Uri.joinPath(context.extensionUri, 'resources', 'mermaid', 'mermaid.esm.min.mjs');
        const jsMerVis = panel.webview.asWebviewUri(jsMermaidPath);
        htmlContent = htmlContent.replace('mermaid.esm.min.mjs', jsMerVis.toString());

        const jsPanzoomPath = vscode.Uri.joinPath(context.extensionUri, 'resources', 'panzoom', 'panzoom.min.js');
        const jsPanzoomPathVis = panel.webview.asWebviewUri(jsPanzoomPath);
        htmlContent = htmlContent.replace('panzoom.min.js', jsPanzoomPathVis.toString());

        const nonce = getNonce();
        htmlContent = htmlContent.replace('nonce-nonce', `nonce-${nonce}`);
        htmlContent = htmlContent.replace(/<script /g, `<script nonce="${nonce}" `);
        htmlContent = htmlContent.replace(/<link /g, `<link nonce="${nonce}" `);

        htmlContent = htmlContent.replace('cspSource', panel.webview.cspSource);

        return htmlContent;
    }

    public dispose() {
        DotGraphPanelView.currentPanel = undefined;

        this._panel.dispose();

        while (this._disposables.length) {
            const disposable = this._disposables.pop();
            if (disposable) {
                disposable.dispose();
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

    if (settings.enable_call_hierarchy === false) {
        vscode.window.showErrorMessage("COBOL call-graph is not enabled (coboleditor.enable_call_hierarchy)");
        return;
    }

    const linesArray: string[] = getCurrentProgramCallGraph(settings, current, false);
    const webviewPanel = DotGraphPanelView.render(context, linesArray);
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

    vscode.workspace.onDidChangeTextDocument(changeEvent => {
        if (vscode.window.activeTextEditor?.document) {
            if (changeEvent.document.uri != vscode.window.activeTextEditor.document.uri) return;
            let current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, settings);
            if (current === undefined) {
                return;
            }
            const updatedLinesArray: string[] = getCurrentProgramCallGraph(settings, current, false);
            DotGraphPanelView.render(context, updatedLinesArray);
        }
    });
}

function saveAsPng(messageText: string) {
    const dataUrl = messageText.split(',');
    return dataUrl;
}

// function setHtmlContent(webview: vscode.Webview, extensionContext: vscode.ExtensionContext) {

//     webview.html = htmlContent;
// }

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
