import * as vscode from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { COBOLToken, COBOLTokenStyle, ParseState, SourceReference_Via_Length } from "./cobolsourcescanner";
import { ICOBOLSourceScanner } from "./icobolsourcescanner";
var fs = require('fs');

class programWindowState {
    current_style: string;
    current_program: ICOBOLSourceScanner;

    private constructor(current_style: string, current_program: ICOBOLSourceScanner) {
        this.current_style = current_style;
        this.current_program = current_program;
    }

    static current_style_map = new Map<string,programWindowState>();

    static get_programWindowState(url: string, current_program: ICOBOLSourceScanner):programWindowState {
        let value = programWindowState.current_style_map.get(url)   
        if (value !== undefined) {
            return value;
        }

        value = new programWindowState("TD", current_program);
        programWindowState.current_style_map.set(url, value);
        return value;
    }
}

function generate_partial_graph(linesArray: string[], clickLines: string[], state: ParseState, para_or_section: Map<string, COBOLToken>) {
    for (const [paragraph, targetToken] of para_or_section) {
        const wordLower = paragraph.toLowerCase();
        const targetRefs: SourceReference_Via_Length[] | undefined = state.currentSectionOutRefs.get(wordLower);

        clickLines.push(`click ${targetToken.tokenNameLower} call callback("${targetToken.tokenName}","${targetToken.filenameAsURI}", ${targetToken.startLine},${targetToken.startColumn}) "${targetToken.description}"`)

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

    private constructor(context: vscode.ExtensionContext, url:string, state: programWindowState, panel: vscode.WebviewPanel, linesArray: string[]) {
        // ... other code ...
        this._panel = panel;
        this._panel.webview.html = this._getWebviewContent(context, url, state, panel, linesArray);

        this._panel.onDidDispose(() => this.dispose(), null, this._disposables);
    }

    public static render(context: vscode.ExtensionContext, url: string, wstate: programWindowState, linesArray: string[], programName: string) {
        if (DotGraphPanelView.currentPanel) {
            DotGraphPanelView.currentPanel._panel.reveal(vscode.ViewColumn.Beside, true);
            DotGraphPanelView.currentPanel._panel.webview.html = DotGraphPanelView.currentPanel._getWebviewContent(context, url, wstate, DotGraphPanelView.currentPanel._panel, linesArray);

            return DotGraphPanelView.currentPanel._panel;
        } else {
            const resourcesDirectory = vscode.Uri.joinPath(context.extensionUri, 'resources')

            const panel = vscode.window.createWebviewPanel(
                'cobolCallGraph',
                'Program:' + programName,
                {
                    viewColumn: vscode.ViewColumn.Beside,
                    preserveFocus: true
                },
                {
                    enableScripts: true,
                    localResourceRoots: [resourcesDirectory]
                }
            );
            DotGraphPanelView.currentPanel = new DotGraphPanelView(context, url, wstate, panel, linesArray);
            return panel;
        }
    }

    private _getWebviewContent(context: vscode.ExtensionContext, url:string, wstyle:programWindowState, panel: vscode.WebviewPanel, linesArray: string[]) {
        const style = wstyle.current_style;
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
            width: 98%;
            overflow: hidden;
            border: 1px solid #ccc;
            margin-bottom: 10px;
            display: block;
            margin-left: auto;
            margin-right: auto;
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
      const vscode = acquireVsCodeApi();

      window.callback = function (message,filename,line,col) {
        // Call back to the extension context to save the image to the workspace folder.
        vscode.postMessage({
            command: 'golink',
            text: message+","+filename+","+line+","+col
        });
      };
      
      document.querySelector('#chart-style').addEventListener("change", function() {
        // Call back to the extension context to save the image to the workspace folder
        vscode.postMessage({
            command: 'change-style',
            text: "__url__"+","+this.value
        });
      });

      let config = { startOnLoad: false, 
                     useMaxWidth: true, 
                     theme: "neutral",
                     securityLevel: 'loose'
                     };
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
    <h3>Program : __program__</h3>
    <p/>
    <div class="diagram-container" id="diagram-container">
     <div class="mermaid" id="mermaid1">
    ${linesArray.join("\n")}
     </div>
    </div>

    <div class="vscode-select">
      <p>Style:
      <select name="chart-style" id="chart-style">
          <option value="" selected="selected" hidden="hidden">Choose here</option>
          <option value="TD">Top Down</option>
          <option value="BT">Bottom-to-top</option>
          <option value="RL">Right-to-left</option>
          <option value="LR">Left-to-right</option>
      </select>
     </p>
    </div>

    <p />
    <hr class="vscode-divider">
    <p />
    <h5>This is a navigation aid not a source analysis feature.</h5>

  </body>
</html>`;

        const jsMermaidPath = vscode.Uri.joinPath(context.extensionUri, 'resources', 'mermaid', 'mermaid.esm.min.mjs');
        const jsMerVis = panel.webview.asWebviewUri(jsMermaidPath);
        htmlContent = htmlContent.replace('mermaid.esm.min.mjs', jsMerVis.toString());

        const jsPanzoomPath = vscode.Uri.joinPath(context.extensionUri, 'resources', 'panzoom', 'panzoom.min.js');
        const jsPanzoomPathVis = panel.webview.asWebviewUri(jsPanzoomPath);
        htmlContent = htmlContent.replace('panzoom.min.js', jsPanzoomPathVis.toString());

        const cssElements = vscode.Uri.joinPath(context.extensionUri, 'resources', 'vscode-elements.css');

        const elementsCode: string = fs.readFileSync(cssElements.path).toString();
        htmlContent = htmlContent.replace('vscode-elements.', elementsCode);

        const nonce = getNonce();
        htmlContent = htmlContent.replace('nonce-nonce', `nonce-${nonce}`);
        htmlContent = htmlContent.replace(/<script /g, `<script nonce="${nonce}" `);
        htmlContent = htmlContent.replace(/<link /g, `<link nonce="${nonce}" `);

        htmlContent = htmlContent.replace('cspSource', panel.webview.cspSource);

        htmlContent = htmlContent.replace("__url__", url);
        htmlContent = htmlContent.replace("__style__", style);
        htmlContent = htmlContent.replace("__program__", getProgramName(wstyle.current_program));
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
    if (settings.enable_program_information === false) {
        vscode.window.showErrorMessage("Unable to generate call graph (coboleditor.enable_program_information)");
        return;
    }
    if (!vscode.window.activeTextEditor) {
        return;
    }

    const current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, settings);
    if (current === undefined) {
        return;
    }

    const url = current.sourceHandler.getUriAsString();
    const current_state = programWindowState.get_programWindowState(url, current)
    const linesArray: string[] = getCurrentProgramCallGraph(current_state, false, true);
    const curp = getProgramName(current);
    const webviewPanel = DotGraphPanelView.render(context, url, current_state, linesArray, curp);
    webviewPanel.webview.onDidReceiveMessage(
        async message => {
            switch (message.command) {
                case 'golink':
                    await goto_link(message.text);
                    return;
                case 'change-style':
                    const messages = message.text.split(",");
                    const new_url = messages[0];
                    const new_style = messages[1];
                    const current_state = programWindowState.get_programWindowState(url, current)
                    current_state.current_style = new_style;
                    const newLinesArray: string[] = getCurrentProgramCallGraph(current_state, false, true);
                    DotGraphPanelView.render(context, new_url, current_state, newLinesArray, curp);
                    return;
            }
        },
        undefined,
        context.subscriptions
    );

    vscode.workspace.onDidChangeTextDocument(changeEvent => {
        if (settings.enable_program_information === false) {
            return;
        }

        if (vscode.window.activeTextEditor?.document) {
            if (changeEvent.document.uri != vscode.window.activeTextEditor.document.uri) return;
            let current = VSCOBOLSourceScanner.getCachedObject(vscode.window.activeTextEditor.document, settings);
            if (current === undefined) {
                return;
            }
            const url = current.sourceHandler.getUriAsString();
            const current_state = programWindowState.get_programWindowState(url, current)
            current_state.current_program = current;
            const updatedLinesArray: string[] = getCurrentProgramCallGraph(current_state, false, true);
            DotGraphPanelView.render(context, url, current_state, updatedLinesArray, getProgramName(current));
        }

    });
}

async function goto_link(messageText: string) {
    const messages = messageText.split(',');
    // const place = messages[0];
    const place_url = messages[1];
    const place_line = Number.parseInt(messages[2]);
    const place_col = Number.parseInt(messages[3]);
    var pos1 = new vscode.Position(place_line, place_col);
    var openPath = vscode.Uri.parse(place_url);
    await vscode.workspace.openTextDocument(openPath).then(doc => {
        vscode.window.showTextDocument(doc).then(editor => {
            editor.selections = [new vscode.Selection(pos1, pos1)];
            var range = new vscode.Range(pos1, pos1);
            editor.revealRange(range);
        });
    });

}

function getNonce() {
    let text = '';
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    for (let i = 0; i < 32; i++) {
        text += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    return text;
}

function getProgramName(current: ICOBOLSourceScanner) {
    if (current.ImplicitProgramId.length !== 0) {
        return current.ImplicitProgramId;
    } else {
        if (current.ProgramId.length !== 0) {
            return current.ProgramId;
        } else {
            return "?"
        }
    }
}

function getCurrentProgramCallGraph(current_state: programWindowState, asMarkdown: boolean, includeEvents: boolean) {
    const current = current_state.current_program;
    const current_style = current_state.current_style;

    const linesArray: string[] = [];
    const clickArray: string[] = [];
    const state = current.sourceReferences.state;

    if (asMarkdown) {
        linesArray.push(`# ${getProgramName(current)}`);
        linesArray.push("");
        linesArray.push("```mermaid");
    }
    linesArray.push(`flowchart ${current_style};`);

    generate_partial_graph(linesArray, clickArray, state, current.sections);
    generate_partial_graph(linesArray, clickArray, state, current.paragraphs);

    if (includeEvents) {
        linesArray.push(...clickArray);
    }

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

    const current_state = programWindowState.get_programWindowState(current.sourceHandler.getUriAsString(), current)
    current_state.current_program = current;
    const doclang = "markdown";
    const linesArray: string[] = getCurrentProgramCallGraph(current_state, true, false);
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
