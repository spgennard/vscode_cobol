import * as vscode from "vscode";

import { SourceOrFolderTreeItem } from "./sourceItem";
import { workspace } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSWorkspaceFolders } from "./cobolfolders";
import { VSLogger } from "./vslogger";
import { VSCOBOLSourceScannerTools } from "./vssourcescannerutils";
import { COBOLUtils } from "./cobolutils";

let sourceTreeView: SourceViewTree | undefined = undefined;
let sourceTreeWatcher: vscode.FileSystemWatcher | undefined = undefined;

export class VSSourceTreeViewHandler {
    static setupSourceViewTree(config: ICOBOLSettings, reinit: boolean): void {

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

    static actionSourceViewItemFunction(si: SourceOrFolderTreeItem, debug: boolean): void {
        let fsPath = "";
        if (si !== undefined && si.uri !== undefined) {
            fsPath = si.uri.path as string;
        }

        COBOLUtils.runOrDebug(fsPath, debug);
    }
}

export class SourceViewTree implements vscode.TreeDataProvider<SourceOrFolderTreeItem> {
    private cobolItem: SourceOrFolderTreeItem;
    private copyBookItem: SourceOrFolderTreeItem;
    private jclItem: SourceOrFolderTreeItem;
    private pliItem: SourceOrFolderTreeItem;
    private hlasmItem: SourceOrFolderTreeItem;
    private documentItem: SourceOrFolderTreeItem;
    private scriptItem: SourceOrFolderTreeItem;
    private objectItem: SourceOrFolderTreeItem;
    private testCaseItem: SourceOrFolderTreeItem;

    private topLevelItems: SourceOrFolderTreeItem[] = [];

    private cobolItems = new Map<string, SourceOrFolderTreeItem>();
    private copyBookItems = new Map<string, SourceOrFolderTreeItem>();
    private jclItems = new Map<string, SourceOrFolderTreeItem>();
    private pliItems = new Map<string, SourceOrFolderTreeItem>();
    private hlasmItems = new Map<string, SourceOrFolderTreeItem>();
    private documentItems = new Map<string, SourceOrFolderTreeItem>();
    private scriptItems = new Map<string, SourceOrFolderTreeItem>();
    private objectItems = new Map<string, SourceOrFolderTreeItem>();
    private testCaseItems = new Map<string, SourceOrFolderTreeItem>();

    private settings: ICOBOLSettings;

    constructor(config: ICOBOLSettings) {
        this.settings = config;

        this.cobolItem = new SourceOrFolderTreeItem(false,"COBOL");
        this.copyBookItem = new SourceOrFolderTreeItem(false,"Copybooks");
        this.jclItem = new SourceOrFolderTreeItem(false,"JCL");
        this.hlasmItem = new SourceOrFolderTreeItem(false,"HLASM");
        this.pliItem = new SourceOrFolderTreeItem(false,"PL/I");
        this.documentItem = new SourceOrFolderTreeItem(false,"Documents");
        this.scriptItem = new SourceOrFolderTreeItem(false,"Scripts");
        this.objectItem = new SourceOrFolderTreeItem(false,"Objects");
        this.testCaseItem = new SourceOrFolderTreeItem(false,"Tests");

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

        if (config.sourceview_include_test_files) {
            this.topLevelItems.push(this.testCaseItem);
        }

        let folders = VSWorkspaceFolders.get();
        if (folders) {
            for (const folder of folders) {
                this.addWorkspace(folder);
            }
        }

        folders = VSWorkspaceFolders.get("");
        if (folders) {
            for (const folder of folders) {
                this.addWorkspace(folder);
            }
        }
    }

    private async addWorkspace(standardFolder: vscode.WorkspaceFolder) {
        this.addFolder(standardFolder.uri);
    }

    private async addFolder(topLevelUri: vscode.Uri) {
        const entries = await workspace.fs.readDirectory(topLevelUri);

        for (const entry of entries) {
            switch (entry[1]) {
                case vscode.FileType.File: {
                    const filename = entry[0];
                    const lastDot = filename.lastIndexOf(".");
                    if (lastDot !== -1) {
                        const ext = filename.substring(1 + lastDot);
                        const subDir = topLevelUri.toString() + "/" + entry[0];
                        this.addExtension(ext, vscode.Uri.parse(subDir));
                    }
                }
                    break;

                case vscode.FileType.Directory: {
                    if (!VSCOBOLSourceScannerTools.ignoreDirectory(entry[0])) {
                        const subDir = vscode.Uri.parse(topLevelUri.toString() + "/" + entry[0]);
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
        if (ext === "acu" || ext === "int" || ext === "gnt" || ext === "so" || ext === "dll") {
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

    private newSourceItem(contextValue: string, label: string, file: vscode.Uri, lnum: number, ext: string): SourceOrFolderTreeItem {
        const item = new SourceOrFolderTreeItem(true,label, file, lnum);
        const newCommand = this.getCommand(file, ext);
        if (newCommand !== undefined) {
            item.command = newCommand;
        }
        item.contextValue = contextValue;
        item.tooltip = file.fsPath;
        return item;
    }

    private _onDidChangeTreeData: vscode.EventEmitter<SourceOrFolderTreeItem | undefined> = new vscode.EventEmitter<SourceOrFolderTreeItem | undefined>();
    readonly onDidChangeTreeData: vscode.Event<SourceOrFolderTreeItem | undefined> = this._onDidChangeTreeData.event;

    private refreshAll(): void {
        this._onDidChangeTreeData.fire(this.cobolItem);
        this._onDidChangeTreeData.fire(this.copyBookItem);
        this._onDidChangeTreeData.fire(this.jclItem);
        this._onDidChangeTreeData.fire(this.pliItem);
        this._onDidChangeTreeData.fire(this.hlasmItem);
        this._onDidChangeTreeData.fire(this.documentItem);
        this._onDidChangeTreeData.fire(this.scriptItem);
        this._onDidChangeTreeData.fire(this.objectItem);
        this._onDidChangeTreeData.fire(this.testCaseItem);
    }


    private addExtensionIfInList(ext: string, file: vscode.Uri, validExtensions: string[], sourceItem: string, base: string, items: Map<string, SourceOrFolderTreeItem>): boolean {
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

    private getBaseName(u: vscode.Uri) {
        let lastSlash = u.fsPath.lastIndexOf("/");
        if (lastSlash !== -1) {
            const after = u.fsPath.substring(1 + lastSlash);
            return after;
        }

        lastSlash = u.fsPath.lastIndexOf("\\");
        if (lastSlash !== -1) {
            const after = u.fsPath.substring(1 + lastSlash);
            return after;
        }
        return "";
    }

    private addExtension(ext: string, file: vscode.Uri) {
        const base = this.getBaseName(file);

        if (ext !== undefined) {
            this.addExtensionIfInList(ext.toLowerCase(), file, this.settings.program_extensions, this.cobolItem.contextValue, base, this.cobolItems);
            this.addExtensionIfInList(ext.toLowerCase(), file, this.settings.copybookexts, "copybook", base, this.copyBookItems);

            const fsp = file.fsPath;
            const extLower = ext.toLowerCase();

            if (base.startsWith("Test") || base.startsWith("MFUT")) {
                this.addExtensionIfInList(ext.toLowerCase(), file, this.settings.program_extensions, this.testCaseItem.contextValue, base, this.testCaseItems);
                this.addExtensionIfInList(ext.toLowerCase(), file, this.settings.copybookexts, this.testCaseItem.contextValue, base, this.testCaseItems);
            }

            switch (extLower) {
                case "jcl":
                case "job":
                case "cntl":
                case "prc":
                case "proc":
                    if (this.jclItems.has(fsp) === false) {
                        this.jclItems.set(fsp, this.newSourceItem("jcl", base, file, 0, extLower));
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
                        this.hlasmItems.set(fsp, this.newSourceItem("hlasm", base, file, 0, extLower));
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
                        this.pliItems.set(fsp, this.newSourceItem("pli", base, file, 0, extLower));
                    }
                    break;

                case "lst":
                case "md":
                case "txt":
                case "html":
                    if (this.documentItems.has(fsp) === false) {
                        this.documentItems.set(fsp, this.newSourceItem("document", base, file, 0, extLower));
                    }
                    break;
                case "sh":
                case "bat":
                    if (this.scriptItems.has(fsp) === false) {
                        this.scriptItems.set(fsp, this.newSourceItem("scripts", base, file, 0, extLower));
                    }
                    break;
                case "int":
                case "gnt":
                case "so":
                case "dll":
                case "acu":
                    if (this.objectItems.has(fsp) === false) {
                        this.objectItems.set(fsp, this.newSourceItem("objects", base, file, 0, extLower));
                    }
                    break;
            }
        }
    }

    private setTreeState(items: Map<string, SourceOrFolderTreeItem>, item: SourceOrFolderTreeItem) {
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
        this.setTreeState(this.testCaseItems, this.testCaseItem);
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
        this.testCaseItems.delete(f);
        this.refreshItems();
    }

    private sharedSubstring(string1: string, string2: string): string {
        let ret = "";
        let index = 1;
        while (string1.substring(0, index) == string2.substring(0, index)) {
            ret = string1.substring(0, index);
            index++;
        }

        return ret;
    }

    private getItemsFromMap(itemMap: Map<string, SourceOrFolderTreeItem>): SourceOrFolderTreeItem[] {
        if (itemMap.size <= 1) {
            return [...itemMap.values()];
        }
        const sortedKeys: string[] = [...itemMap.keys()].sort();
        const sharedPrefix = this.sharedSubstring(sortedKeys[0], sortedKeys[sortedKeys.length - 1]);

        const keyedMap = new Map<string, number>();
        for (const [, sitem] of itemMap) {
            if (sitem.label !== undefined) {
                if (keyedMap.has(sitem.label)) {
                    let labelCounter = keyedMap.get(sitem.label);
                    if (labelCounter !== undefined) {
                        labelCounter++;
                        keyedMap.delete(sitem.label);
                        keyedMap.set(sitem.label, labelCounter);
                    }
                } else {
                    keyedMap.set(sitem.label, 1);
                }
            }
        }

        const values: SourceOrFolderTreeItem[] = [];
        for (const [sfilename, sitem] of itemMap) {
            const labelCounter = keyedMap.get(sitem.label);
            if (labelCounter !== undefined && labelCounter >= 2) {
                const dupItem = new SourceOrFolderTreeItem(true,sfilename.substring(sharedPrefix.length, sfilename.length), sitem.uri, sitem.line);
                dupItem.tooltip = sitem.tooltip;
                if (sitem.command) {
                    dupItem.command = sitem.command;
                }
                dupItem.iconPath = sitem.iconPath;
                if (sitem.collapsibleState !== undefined) {
                    dupItem.collapsibleState = sitem.collapsibleState;
                }

                if (sitem.description !== undefined) {
                    dupItem.description = sitem.description;
                }
                if (sitem.accessibilityInformation !== undefined) {
                    dupItem.accessibilityInformation = sitem.accessibilityInformation;
                }
                values.push(dupItem);
            } else {
                values.push(sitem);
            }
        }
        return values;
    }

    public getChildren(element?: SourceOrFolderTreeItem): Thenable<SourceOrFolderTreeItem[]> {
        if (element === undefined) {
            return new Promise(resolve => {
                resolve(this.topLevelItems);
            });
        }

        return new Promise(resolve => {
            let rtn: SourceOrFolderTreeItem[] = [];

            switch (element.label) {
                case "COBOL": rtn = this.getItemsFromMap(this.cobolItems); break;
                case "Copybooks": rtn = this.getItemsFromMap(this.copyBookItems); break;
                case "JCL": rtn = this.getItemsFromMap(this.jclItems); break;
                case "PL/I": rtn = this.getItemsFromMap(this.pliItems); break;
                case "HLASM": rtn = this.getItemsFromMap(this.hlasmItems); break;
                case "Documents": rtn = this.getItemsFromMap(this.documentItems); break;
                case "Scripts": rtn = this.getItemsFromMap(this.scriptItems); break;
                case "Objects": rtn = this.getItemsFromMap(this.objectItems); break;
                case "Tests": rtn = this.getItemsFromMap(this.testCaseItems); break;
            }


            resolve(rtn);
        });
    }

    getTreeItem(element: SourceOrFolderTreeItem): vscode.TreeItem {
        return element;
    }
}