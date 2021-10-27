import * as vscode from "vscode";
import * as path from "path";

import { SourceItem, SourceFolderItem } from "./sourceItem";
import { workspace } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { getVSWorkspaceFolders } from "./cobolfolders";
import {  VSLogger } from "./vslogger";
import { COBOLFileUtils } from "./fileutils";
import { VSCOBOLSourceScannerTools } from "./vssourcescannerutils";

let sourceTreeView: SourceViewTree | undefined = undefined;
let sourceTreeWatcher: vscode.FileSystemWatcher | undefined = undefined;
let commandTerminal: vscode.Terminal | undefined = undefined;
const commandTerminalName = "COBOL Application";

export class VSSourceTreeViewHandler {
    static setupSourceViewTree(config: ICOBOLSettings, reinit: boolean):void{

        if ((config.sourceview === false || reinit) && sourceTreeView !== undefined) {
            sourceTreeWatcher?.dispose();
            sourceTreeView = undefined;
        }

        if (config.sourceview && sourceTreeView === undefined) {
            sourceTreeView = new SourceViewTree(config);
            sourceTreeWatcher = workspace.createFileSystemWatcher("**/*");

            sourceTreeWatcher.onDidCreate((uri) => {
                if (sourceTreeView !== undefined) {
                    sourceTreeView.checkFile(uri);
                }
            });

            sourceTreeWatcher.onDidDelete((uri) => {
                if (sourceTreeView !== undefined) {
                    sourceTreeView.clearFile(uri);
                }
            });

            vscode.window.registerTreeDataProvider("flat-source-view", sourceTreeView);
            return;
        }

    }

    static actionSourceViewItemFunction(si: SourceItem, debug: boolean):void {
        if (commandTerminal === undefined) {
            commandTerminal = vscode.window.createTerminal(commandTerminalName);
        }

        let prefRunner = "";
        let prefRunnerDebug = "";
        let fsPath = "";
        if (si !== undefined && si.uri !== undefined) {
            fsPath = si.uri.path as string;
        }

        if (COBOLFileUtils.isWin32) {
            if (fsPath.endsWith("acu")) {
                prefRunner = "wrun32";
                prefRunnerDebug = debug ? "-d " : "";
            } else if (fsPath.endsWith("int") || fsPath.endsWith("gnt")) {
                prefRunner = "cobrun";
                prefRunnerDebug = debug ? "(+A) " : "";
            }
        } else {
            // todo consider adding threaded version, cobmode
            if (fsPath.endsWith("int") || fsPath.endsWith("gnt")) {
                prefRunner = debug ? "anim" : "cobrun";
            } else if (fsPath.endsWith("acu")) {
                prefRunner = "runcbl";
                prefRunnerDebug = debug ? "-d " : "";
            }
        }

        commandTerminal.show(true);
        commandTerminal.sendText(`${prefRunner} ${prefRunnerDebug}${fsPath}`);
    }
}

export class SourceViewTree implements vscode.TreeDataProvider<SourceItem> {
    private cobolItem: SourceItem;
    private copyBookItem: SourceItem;
    private jclItem: SourceItem;
    private pliItem: SourceItem;
    private hlasmItem: SourceItem;
    private documentItem: SourceItem;
    private scriptItem: SourceItem;
    private objectItem: SourceItem;

    private topLevelItems: SourceItem[] = [];

    private cobolItems = new Map<string, SourceFolderItem>();
    private copyBookItems = new Map<string, SourceFolderItem>();
    private jclItems = new Map<string, SourceFolderItem>();
    private pliItems = new Map<string, SourceFolderItem>();
    private hlasmItems = new Map<string, SourceFolderItem>();
    private documentItems = new Map<string, SourceFolderItem>();
    private scriptItems = new Map<string, SourceFolderItem>();
    private objectItems = new Map<string, SourceFolderItem>();

    private settings: ICOBOLSettings;

    constructor(config: ICOBOLSettings) {
        this.settings = config;

        this.cobolItem = new SourceFolderItem("COBOL");
        this.copyBookItem = new SourceFolderItem("Copybooks");
        this.jclItem = new SourceFolderItem("JCL");
        this.hlasmItem = new SourceFolderItem("HLASM");
        this.pliItem = new SourceFolderItem("PL/I");
        this.documentItem = new SourceFolderItem("Documents");
        this.scriptItem = new SourceFolderItem("Scripts");
        this.objectItem = new SourceFolderItem("Objects");

        this.topLevelItems.push(this.cobolItem);
        this.topLevelItems.push(this.copyBookItem);

        if (config.sourceview_include_jcl_files) {
            this.topLevelItems.push(this.jclItem);
        }

        if (config.sourceview_include_hlasm_files) {
            this.topLevelItems.push(this.hlasmItem);
        }

        if (config.sourceview_include_pli_files) {
            this.topLevelItems.push(this.pliItem);
        }

        if (config.sourceview_include_doc_files) {
            this.topLevelItems.push(this.documentItem);
        }

        if (config.sourceview_include_script_files) {
            this.topLevelItems.push(this.scriptItem);
        }

        if (config.sourceview_include_object_files) {
            this.topLevelItems.push(this.objectItem);
        }

        const folders = getVSWorkspaceFolders();
        if (folders) {
            for (const folder of folders) {
                this.addWorkspace(folder);
            }
        }
    }

    private async addWorkspace(standardFolder: vscode.WorkspaceFolder) {
        this.addFolder(standardFolder.uri.fsPath);
    }

    private async addFolder(topLevel: string) {
        const topLevelUri = vscode.Uri.file(topLevel);
        const entries = await workspace.fs.readDirectory(topLevelUri);

        for (const entry of entries) {
            switch (entry[1]) {
                case vscode.FileType.File: {
                    const filename = entry[0];
                    const lastDot = filename.lastIndexOf(".");
                    if (lastDot !== -1) {
                        const ext = filename.substr(1 + lastDot);
                        const subDir = path.join(topLevel, entry[0]);
                        this.addExtension(ext, vscode.Uri.file(subDir));
                    }
                }
                    break;

                case vscode.FileType.Directory: {
                    if (!VSCOBOLSourceScannerTools.ignoreDirectory(entry[0])) {
                        const subDir = path.join(topLevel, entry[0]);
                        this.addFolder(subDir);
                    }
                }
                    break;
            }
        }
        this.refreshItems();
    }

    private getCommand(fileUri: vscode.Uri, ext: string): vscode.Command | undefined {
        const location = new vscode.Range(new vscode.Position(0, 0), new vscode.Position(0, 0));

        let actionCommand = "vscode.open";
        if (ext === "acu" || ext === "int" || ext === "gnt") {
            actionCommand = "";
        }
        return {
            arguments: [
                fileUri,
                { selection: location },
            ],
            command: actionCommand,
            title: "Open",
        };
    }

    private newSourceItem(contextValue: string, label: string, file: vscode.Uri, lnum: number, ext:string): SourceItem {
        const item = new SourceItem(label, file, lnum);
        item.command = this.getCommand(file,ext);
        item.contextValue = contextValue;
        item.tooltip = file.fsPath;
        return item;
    }

    private _onDidChangeTreeData: vscode.EventEmitter<SourceItem | undefined> = new vscode.EventEmitter<SourceItem | undefined>();
    readonly onDidChangeTreeData: vscode.Event<SourceItem | undefined> = this._onDidChangeTreeData.event;

    private refreshAll(): void {
        this._onDidChangeTreeData.fire(this.cobolItem);
        this._onDidChangeTreeData.fire(this.copyBookItem);
        this._onDidChangeTreeData.fire(this.jclItem);
        this._onDidChangeTreeData.fire(this.pliItem);
        this._onDidChangeTreeData.fire(this.hlasmItem);
        this._onDidChangeTreeData.fire(this.documentItem);
        this._onDidChangeTreeData.fire(this.scriptItem);
        this._onDidChangeTreeData.fire(this.objectItem);
    }


    private addExtensionIfInList(ext: string, file: vscode.Uri, validExtensions: string[], sourceItem: string, base: string, items: Map<string, SourceFolderItem>): boolean {
        for (const validExtension of validExtensions) {
            if (validExtension.length > 0 && ext === validExtension) {
                const f = file.fsPath;
                if (items.has(f) === false) {
                    items.set(f, this.newSourceItem(sourceItem, base, file, 0, validExtension));
                    return true;
                }
            }
        }

        return false;
    }


    private addExtension(ext: string, file: vscode.Uri) {
        const base = path.basename(file.fsPath);

        if (ext !== undefined) {
            this.addExtensionIfInList(ext.toLowerCase(), file, this.settings.program_extensions, this.cobolItem.contextValue, base, this.cobolItems);
            this.addExtensionIfInList(ext.toLowerCase(), file, this.settings.copybookexts, "copybook", base, this.copyBookItems);

            const fsp = file.fsPath;
            const extLower = ext.toLowerCase();
            switch (extLower) {
                case "jcl":
                case "job":
                case "cntl":
                case "prc":
                case "proc":
                    if (this.jclItems.has(fsp) === false) {
                        this.jclItems.set(fsp, this.newSourceItem("jcl", base, file, 0,extLower));
                    }
                    break;
                case "hlasm":
                case "asm":
                case "s":
                case "asmpgm":
                case "mac":
                case "mlc":
                case "asmmac":
                    if (this.hlasmItems.has(fsp) === false) {
                        this.hlasmItems.set(fsp, this.newSourceItem("hlasm", base, file, 0,extLower));
                    }
                    break;

                case "pl1":
                case "pli":
                case "plinc":
                case "pc":
                case "pci":
                case "pcx":
                case "inc":
                    if (this.pliItems.has(fsp) === false) {
                        this.pliItems.set(fsp, this.newSourceItem("pli", base, file, 0,extLower));
                    }
                    break;

                case "md":
                case "txt":
                case "html":
                    if (this.documentItems.has(fsp) === false) {
                        this.documentItems.set(fsp, this.newSourceItem("document", base, file, 0,extLower));
                    }
                    break;
                case "sh":
                case "bat":
                    if (this.scriptItems.has(fsp) === false) {
                        this.scriptItems.set(fsp, this.newSourceItem("scripts", base, file, 0,extLower));
                    }
                    break;
                case "int":
                case "gnt":
                case "acu":
                    if (this.objectItems.has(fsp) === false) {
                        this.objectItems.set(fsp, this.newSourceItem("objects", base, file, 0,extLower));
                    }
                break;
            }
        }
    }

    private setTreeState(items: Map<string, SourceFolderItem>, item: SourceItem) {
        if (items.size !== 0) {
            item.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        } else {
            item.collapsibleState = vscode.TreeItemCollapsibleState.None;
        }
    }

    private refreshItems() {
        this.setTreeState(this.cobolItems, this.cobolItem);
        this.setTreeState(this.copyBookItems, this.copyBookItem);
        this.setTreeState(this.jclItems, this.jclItem);
        this.setTreeState(this.hlasmItems, this.hlasmItem);
        this.setTreeState(this.pliItems, this.pliItem);
        this.setTreeState(this.documentItems, this.documentItem);
        this.setTreeState(this.scriptItems, this.scriptItem);
        this.setTreeState(this.objectItems, this.objectItem);

        this.refreshAll();
    }

    public async checkFile(vuri: vscode.Uri): Promise<void> {
        try {
            const fileExtension = vuri.fsPath.split(".").pop();
            if (fileExtension !== undefined) {
                this.addExtension(fileExtension, vuri);
                this.refreshItems();
            }
        }
        catch (e) {
            VSLogger.logException("checkFile", e as Error);
        }
    }

    public clearFile(vuri: vscode.Uri): void {
        const f = vuri.fsPath;
        this.cobolItems.delete(f);
        this.copyBookItems.delete(f);
        this.jclItems.delete(f);
        this.hlasmItems.delete(f);
        this.pliItems.delete(f);
        this.documentItems.delete(f);
        this.scriptItems.delete(f);
        this.objectItems.delete(f);
        this.refreshItems();
    }

    private getItemsFromMap(itemMap: Map<string, SourceItem>): SourceItem[] {
        return [...itemMap.values()];
    }

    public getChildren(element?: SourceItem): Thenable<SourceItem[]> {
        if (element === undefined) {
            return new Promise(resolve => {
                resolve(this.topLevelItems);
            });
        }

        return new Promise(resolve => {
            let rtn: SourceItem[] = [];

            switch (element.label) {
                case "COBOL": rtn = this.getItemsFromMap(this.cobolItems); break;
                case "Copybooks": rtn = this.getItemsFromMap(this.copyBookItems); break;
                case "JCL": rtn = this.getItemsFromMap(this.jclItems); break;
                case "PL/I": rtn = this.getItemsFromMap(this.pliItems); break;
                case "HLASM": rtn = this.getItemsFromMap(this.hlasmItems); break;
                case "Documents": rtn = this.getItemsFromMap(this.documentItems); break;
                case "Scripts": rtn = this.getItemsFromMap(this.scriptItems); break;
                case "Objects": rtn = this.getItemsFromMap(this.objectItems); break;
            }


            resolve(rtn);
        });
    }

    getTreeItem(element: SourceItem): vscode.TreeItem {
        return element;
    }
}