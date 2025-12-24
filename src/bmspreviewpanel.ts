import * as vscode from 'vscode';

export class BmsPreviewPanel {
    public static currentPanel: BmsPreviewPanel | undefined;
    public static readonly viewType = 'bmsPreview';

    private readonly _panel: vscode.WebviewPanel;
    private _disposables: vscode.Disposable[] = [];
    private _currentDocument: vscode.TextDocument | undefined;

    public static createOrShow(extensionUri: vscode.Uri, document?: vscode.TextDocument) {
        const column = vscode.ViewColumn.Beside; // Always open beside the active editor

        // If we already have a panel, show it
        if (BmsPreviewPanel.currentPanel) {
            BmsPreviewPanel.currentPanel._panel.reveal(column);
            if (document) {
                BmsPreviewPanel.currentPanel._updatePreview(document);
            }
            return;
        }

        // Otherwise, create a new panel
        const panel = vscode.window.createWebviewPanel(
            BmsPreviewPanel.viewType,
            'BMS Preview',
            column,
            {
                enableScripts: true,
                retainContextWhenHidden: true,
                localResourceRoots: [extensionUri]
            }
        );

        BmsPreviewPanel.currentPanel = new BmsPreviewPanel(panel, document);
    }

    public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
        BmsPreviewPanel.currentPanel = new BmsPreviewPanel(panel);
    }

    private constructor(panel: vscode.WebviewPanel, document?: vscode.TextDocument) {
        this._panel = panel;
        this._currentDocument = document;

        this._update(document);

        this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

        this._panel.onDidChangeViewState(
            e => {
                if (this._panel.visible) {
                    this._update(this._currentDocument);
                }
            },
            null,
            this._disposables
        );

        // Listen for when the user changes the active text editor
        vscode.window.onDidChangeActiveTextEditor(
            e => {
                if (e && e.document.languageId === 'bms') {
                    this._updatePreview(e.document);
                }
            },
            null,
            this._disposables
        );

        // Listen for changes to BMS documents
        vscode.workspace.onDidChangeTextDocument(
            e => {
                if (e.document.languageId === 'bms' && this._panel.visible) {
                    this._updatePreview(e.document);
                }
            },
            null,
            this._disposables
        );
    }

    public dispose() {
        BmsPreviewPanel.currentPanel = undefined;

        this._panel.dispose();

        while (this._disposables.length) {
            const x = this._disposables.pop();
            if (x) {
                x.dispose();
            }
        }
    }

    private _update(document?: vscode.TextDocument) {
        const webview = this._panel.webview;
        
        this._panel.title = document ? `BMS Preview - ${document.fileName}` : 'BMS Preview';
        this._panel.webview.html = this._getHtmlForWebview(webview, document);
    }

    private _updatePreview(document: vscode.TextDocument) {
        this._currentDocument = document;
        this._panel.title = `BMS Preview - ${document.fileName}`;
        this._panel.webview.postMessage({
            command: 'update',
            text: document.getText(),
            fileName: document.fileName
        });
    }

    private _getHtmlForWebview(webview: vscode.Webview, document?: vscode.TextDocument) {
        const bmsText = document ? document.getText() : '';
        
        return `<!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>BMS Preview</title>
            <style>
                /* Use system monospace fonts that provide good 3270-like appearance */
                :root {
                    --terminal-font-family: 
                        /* System fonts with good 3270 characteristics */
                        'SF Mono', 'Monaco', 'Consolas', 'Roboto Mono', 'Source Code Pro', 
                        'Inconsolata', 'Liberation Mono', 'DejaVu Sans Mono', 
                        /* Classic monospace fallbacks */
                        'Courier New', 'Lucida Console', 
                        /* Generic fallback */
                        monospace;
                    --vscode-editor-font-size: 14px;
                    --vscode-editor-background: #1e1e1e;
                    --vscode-editor-foreground: #d4d4d4;
                    --vscode-panel-border: #454545;
                    --vscode-textCodeBlock-background: #2d2d2d;
                    --vscode-descriptionForeground: #cccccc;
                    --vscode-textPreformat-foreground: #569cd6;
                    --vscode-list-hoverBackground: #2a2d2e;
                    --vscode-list-activeSelectionBackground: #094771;
                    --vscode-string-foreground: #ce9178;
                }

                * {
                    box-sizing: border-box;
                }
                
                body {
                    font-family: var(--terminal-font-family);
                    font-size: var(--vscode-editor-font-size);
                    background-color: var(--vscode-editor-background);
                    color: var(--vscode-editor-foreground);
                    margin: 0;
                    padding: 0;
                    overflow-x: auto;
                }
                
                .preview-container {
                    padding: 20px;
                    max-width: 100%;
                }
                
                .preview-header {
                    display: flex;
                    align-items: center;
                    justify-content: space-between;
                    margin-bottom: 20px;
                    padding: 10px 0;
                    border-bottom: 1px solid var(--vscode-panel-border);
                }
                
                .preview-title {
                    font-size: 18px;
                    font-weight: bold;
                    margin: 0;
                }
                
                .preview-info {
                    font-size: 12px;
                    color: var(--vscode-descriptionForeground);
                }
                
                .bms-screen-container {
                    margin: 20px 0;
                    border: 1px solid var(--vscode-panel-border);
                    border-radius: 4px;
                    overflow: auto;
                    background: var(--vscode-textCodeBlock-background);
                    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
                    max-width: 100%;
                    padding: 10px;
                }
                
                .bms-screen {
                    width: 80ch;
                    height: 24em;
                    position: relative;
                    background-color: #000040;
                    margin: 0 auto;
                    font-family: var(--terminal-font-family);
                    font-size: 14px;
                    line-height: 1.2;
                    overflow: hidden;
                    border: 2px solid #00ff00;
                    transition: all 0.3s ease;
                    font-variant-ligatures: none;
                    letter-spacing: 0;
                    font-feature-settings: "kern" 0;
                    /* Add subtle text shadow for better visibility */
                    text-shadow: 0 0 2px rgba(0, 255, 0, 0.3);
                }
                
                .bms-field {
                    position: absolute;
                    border: 1px dotted transparent;
                    min-height: 1.2em;
                    overflow: hidden;
                    box-sizing: border-box;
                    white-space: pre;
                    cursor: default;
                    transition: all 0.2s ease;
                    font-family: var(--terminal-font-family) !important;
                    font-size: var(--vscode-editor-font-size) !important;
                    font-variant-ligatures: none;
                    font-feature-settings: "kern" 0;
                }
                
                .bms-field:hover {
                    border-color: #ffffff;
                    background-color: rgba(255, 255, 255, 0.1) !important;
                }
                
                .bms-field.protected {
                    background-color: rgba(0, 255, 0, 0.1);
                    color: #00ff00;
                    border-color: rgba(0, 255, 0, 0.3);
                }
                
                .bms-field.unprotected {
                    background-color: rgba(255, 255, 0, 0.1);
                    color: #ffff00;
                    border-color: rgba(255, 255, 0, 0.3);
                }
                
                .bms-field.bright {
                    font-weight: bold;
                    filter: brightness(150%);
                }
                
                .bms-field.dim {
                    opacity: 0.6;
                }
                
                .bms-field.numeric {
                    text-align: right;
                }
                
                .field-label {
                    font-size: 0.7em;
                    color: #888;
                    position: absolute;
                    top: -15px;
                    left: 0;
                    background-color: #000040;
                    padding: 0 3px;
                    border-radius: 2px;
                    pointer-events: none;
                    z-index: 1;
                }
                
                .bms-mapset-info {
                    margin: 20px 0;
                    padding: 15px;
                    background-color: var(--vscode-textCodeBlock-background);
                    border: 1px solid var(--vscode-panel-border);
                    border-radius: 4px;
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                    gap: 10px;
                }
                
                .info-item {
                    display: flex;
                    flex-direction: column;
                }
                
                .info-label {
                    font-weight: bold;
                    color: var(--vscode-textPreformat-foreground);
                    font-size: 0.9em;
                    margin-bottom: 2px;
                }
                
                .info-value {
                    color: var(--vscode-editor-foreground);
                    font-family: var(--terminal-font-family) !important;
                    font-size: var(--vscode-editor-font-size);
                }
                
                .field-list {
                    margin-top: 20px;
                }
                
                .section-title {
                    font-size: 16px;
                    font-weight: bold;
                    margin: 20px 0 10px 0;
                    color: var(--vscode-textPreformat-foreground);
                    border-bottom: 1px solid var(--vscode-panel-border);
                    padding-bottom: 5px;
                }
                
                .field-item {
                    margin: 8px 0;
                    padding: 12px;
                    background-color: var(--vscode-list-hoverBackground);
                    border-radius: 4px;
                    border-left: 4px solid var(--vscode-textPreformat-foreground);
                    font-size: 0.9em;
                    transition: background-color 0.2s ease;
                }
                
                .field-item:hover {
                    background-color: var(--vscode-list-activeSelectionBackground);
                }
                
                .field-name {
                    font-weight: bold;
                    color: var(--vscode-textPreformat-foreground);
                }
                
                .field-details {
                    color: var(--vscode-descriptionForeground);
                    margin-top: 4px;
                    font-size: 0.85em;
                }
                
                .field-initial {
                    color: var(--vscode-string-foreground);
                    margin-top: 4px;
                    font-style: italic;
                }
                
                .no-content {
                    text-align: center;
                    color: var(--vscode-descriptionForeground);
                    margin: 40px 0;
                    font-style: italic;
                }
                
                .screen-dimensions {
                    position: absolute;
                    top: -25px;
                    right: 0;
                    font-size: 0.8em;
                    color: #888;
                    background-color: #000040;
                    padding: 2px 6px;
                    border-radius: 2px;
                }
            </style>
        </head>
        <body>
            <div class="preview-container">
                <div class="preview-header">
                    <h1 class="preview-title">BMS Screen Preview</h1>
                    <div class="preview-info">
                        <div id="fileName">${document ? document.fileName : 'No BMS file selected'}</div>
                    </div>
                </div>
                
                <div id="mapsetInfo" class="bms-mapset-info"></div>
                
                <div class="bms-screen-container">
                    <div class="bms-screen" id="bmsScreen">
                        <div class="screen-dimensions" id="screenDimensions">24×80</div>
                        ${!bmsText ? '<div class="no-content">No BMS content to preview</div>' : ''}
                    </div>
                </div>
                
                <div class="field-list">
                    <h3 class="section-title">Field Information</h3>
                    <div id="fieldList"></div>
                </div>
            </div>

            <script>
                const vscode = acquireVsCodeApi();
                
                window.addEventListener('message', event => {
                    const message = event.data;
                    switch (message.command) {
                        case 'update':
                            updatePreview(message.text, message.fileName);
                            break;
                    }
                });

                function updatePreview(bmsText, fileName) {
                    document.getElementById('fileName').textContent = fileName || 'No BMS file selected';
                    if (!bmsText || bmsText.trim() === '') {
                        const screen = document.getElementById('bmsScreen');
                        screen.style.width = '80ch';
                        screen.style.height = '24em';
                        screen.innerHTML = '<div class="screen-dimensions" id="screenDimensions">24×80</div><div class="no-content">No BMS content to preview</div>';
                        document.getElementById('mapsetInfo').innerHTML = '<div class="info-item"><div class="info-label">Status</div><div class="info-value">No content</div></div>';
                        document.getElementById('fieldList').innerHTML = '<div class="no-content">No fields to display</div>';
                        return;
                    }
                    
                    const result = parseBms(bmsText);
                    document.getElementById('bmsScreen').innerHTML = '<div class="screen-dimensions" id="screenDimensions">24×80</div>' + result.screenHtml;
                    document.getElementById('mapsetInfo').innerHTML = result.mapsetInfo;
                    document.getElementById('fieldList').innerHTML = result.fieldList || '<div class="no-content">No fields found</div>';
                }

                function parseBms(text) {
                    const lines = text.split('\\n');
                    let screenHtml = '';
                    let mapsetInfo = '';
                    let fieldList = '';
                    let fields = [];
                    
                    let currentMapset = '';
                    let currentMap = '';
                    let mapSize = '';
                    let screenRows = 24;
                    let screenCols = 80;

                    // Preprocess lines to handle continuation lines (X and *)
                    const processedLines = [];
                    let currentLine = '';
                    
                    for (let i = 0; i < lines.length; i++) {
                        const line = lines[i];
                        const trimmedLine = line.trim();
                        
                        if (trimmedLine === '' || trimmedLine.startsWith('*')) {
                            if (currentLine) {
                                processedLines.push(currentLine);
                                currentLine = '';
                            }
                            continue;
                        }
                        
                        // Check if this line ends with X or * (continuation)
                        if (trimmedLine.endsWith('X') || trimmedLine.endsWith('*')) {
                            // Remove the continuation character and add to current line
                            currentLine += ' ' + trimmedLine.slice(0, -1).trim();
                        } else {
                            // Add to current line and finish it
                            currentLine += ' ' + trimmedLine;
                            processedLines.push(currentLine.trim());
                            currentLine = '';
                        }
                    }
                    
                    // Add any remaining line
                    if (currentLine) {
                        processedLines.push(currentLine.trim());
                    }

                    let unnamedFieldCounter = 1;

                    for (let i = 0; i < processedLines.length; i++) {
                        const line = processedLines[i];
                        
                        if (line.trim() === '') {
                            continue;
                        }

                        // Parse DFHMSD (mapset definition)
                        const mapsetMatch = line.match(/([A-Z0-9]+)\\s+DFHMSD/);
                        if (mapsetMatch) {
                            currentMapset = mapsetMatch[1];
                            continue;
                        }

                        // Parse DFHMDI (map definition)
                        const mapMatch = line.match(/([A-Z0-9]+)\\s+DFHMDI/);
                        if (mapMatch) {
                            currentMap = mapMatch[1];
                            
                            // Extract map size if present
                            const sizeMatch = line.match(/SIZE\\s*=\\s*\\((\\d+),\\s*(\\d+)\\)/);
                            if (sizeMatch) {
                                screenRows = parseInt(sizeMatch[1]);
                                screenCols = parseInt(sizeMatch[2]);
                                mapSize = \`\${screenRows}×\${screenCols}\`;
                            }
                            continue;
                        }

                        // Parse DFHMDF (field definition) - handle both named and unnamed fields
                        let fieldMatch = line.match(/\\s*([A-Z0-9]+)\\s+DFHMDF/); // Named field
                        let fieldName = null;
                        
                        if (fieldMatch) {
                            fieldName = fieldMatch[1];
                        } else {
                            // Check for unnamed field (starts directly with DFHMDF)
                            const unnamedMatch = line.match(/^\\s*DFHMDF/);
                            if (unnamedMatch) {
                                fieldName = 'UNNAMED_' + unnamedFieldCounter;
                                unnamedFieldCounter++;
                            }
                        }
                        
                        if (fieldName) {
                            const field = parseField(line, fieldName);
                            if (field) {
                                fields.push(field);
                                screenHtml += createFieldHtml(field);
                                fieldList += createFieldListItem(field);
                            }
                        }
                    }

                    // Build mapset info
                    mapsetInfo = '<div class="info-item"><div class="info-label">Mapset</div><div class="info-value">' + (currentMapset || 'Not specified') + '</div></div>';
                    mapsetInfo += '<div class="info-item"><div class="info-label">Map</div><div class="info-value">' + (currentMap || 'Not specified') + '</div></div>';
                    mapsetInfo += '<div class="info-item"><div class="info-label">Size</div><div class="info-value">' + (mapSize || '24×80 (default)') + '</div></div>';
                    mapsetInfo += '<div class="info-item"><div class="info-label">Fields</div><div class="info-value">' + fields.length + '</div></div>';

                    // Update screen dimensions
                    updateScreenSize(screenRows, screenCols);

                    return { screenHtml, mapsetInfo, fieldList };
                }

                function parseField(line, fieldName) {
                    // Extract position
                    const posMatch = line.match(/POS\\s*=\\s*\\((\\d+),\\s*(\\d+)\\)/);
                    if (!posMatch) return null;
                    
                    const row = parseInt(posMatch[1]);
                    const col = parseInt(posMatch[2]);

                    // Extract length
                    const lengthMatch = line.match(/LENGTH\\s*=\\s*(\\d+)/);
                    const length = lengthMatch ? parseInt(lengthMatch[1]) : 10;

                    // Extract attributes
                    const isProtected = (line.includes('PROT') && !line.includes('UNPROT')) || line.includes('ASKIP');
                    const isBright = line.includes('BRT');
                    const isDim = line.includes('DRK');
                    const isNumeric = line.includes('NUM');
                    const isIC = line.includes('IC');

                    // Extract initial value - handle both single and double quotes
                    let initialValue = '';
                    const singleQuoteMatch = line.match(/INITIAL\\s*=\\s*'([^']*)'/i);
                    const doubleQuoteMatch = line.match(/INITIAL\\s*=\\s*"([^"]*)"/i);
                    
                    if (singleQuoteMatch) {
                        initialValue = singleQuoteMatch[1];
                    } else if (doubleQuoteMatch) {
                        initialValue = doubleQuoteMatch[1];
                    }

                    return {
                        name: fieldName,
                        row,
                        col,
                        length,
                        isProtected,
                        isBright,
                        isDim,
                        isNumeric,
                        isIC,
                        initialValue,
                        rawLine: line.trim()
                    };
                }

                function createFieldHtml(field) {
                    const top = (field.row - 1) * 1.2;
                    const left = (field.col - 1) * 0.6;
                    
                    let fieldClass = 'bms-field';
                    fieldClass += field.isProtected ? ' protected' : ' unprotected';
                    fieldClass += field.isBright ? ' bright' : '';
                    fieldClass += field.isDim ? ' dim' : '';
                    fieldClass += field.isNumeric ? ' numeric' : '';

                    // Display initial value if present, otherwise show underscores for field length
                    let displayValue;
                    if (field.initialValue && field.initialValue.trim() !== '') {
                        // Escape HTML characters and preserve spaces
                        displayValue = field.initialValue
                            .replace(/&/g, '&amp;')
                            .replace(/</g, '&lt;')
                            .replace(/>/g, '&gt;')
                            .replace(/ /g, '&nbsp;');
                    } else {
                        displayValue = '&nbsp;'.repeat(field.length).replace(/&nbsp;/g, '_');
                    }

                    return '<div class="' + fieldClass + '"' + 
                        ' style="top: ' + top + 'em; left: ' + left + 'em; width: ' + (field.length * 0.6) + 'em; height: 1.2em; line-height: 1.2em;"' +
                        ' title="' + field.name + ': ' + field.rawLine + '">' +
                        '<div class="field-label">' + field.name + '</div>' +
                        displayValue +
                    '</div>';
                }

                function createFieldListItem(field) {
                    const attributes = [];
                    if (field.isProtected) attributes.push('Protected');
                    if (!field.isProtected) attributes.push('Unprotected');
                    if (field.isBright) attributes.push('Bright');
                    if (field.isDim) attributes.push('Dim');
                    if (field.isNumeric) attributes.push('Numeric');
                    if (field.isIC) attributes.push('Initial Cursor');
                    
                    let html = \`<div class="field-item">
                        <div class="field-name">\${field.name}</div>
                        <div class="field-details">Position: (\${field.row},\${field.col}) • Length: \${field.length} • Attributes: \${attributes.join(', ') || 'None'}</div>\`;
                    
                    if (field.initialValue) {
                        html += \`<div class="field-initial">Initial: "\${field.initialValue}"</div>\`;
                    }
                    
                    html += '</div>';
                    return html;
                }
                
                function updateScreenSize(rows, cols) {
                    const screen = document.getElementById('bmsScreen');
                    if (screen) {
                        screen.style.width = cols + 'ch';
                        screen.style.height = rows + 'em';
                        
                        // Update the dimensions display
                        const dimensionsDisplay = screen.querySelector('.screen-dimensions');
                        if (dimensionsDisplay) {
                            dimensionsDisplay.textContent = rows + '×' + cols;
                        }
                    }
                }

                // Initial parse if content exists
                if (${JSON.stringify(bmsText)}) {
                    updatePreview(${JSON.stringify(bmsText)}, ${JSON.stringify(document?.fileName || '')});
                }
            </script>
        </body>
        </html>`;
    }
}