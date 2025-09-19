import * as vscode from "vscode";

import { SourceOrFolderTreeItem } from "./sourceItem";
import { workspace } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { VSLogger } from "./vslogger";
import { VSCOBOLSourceScannerTools } from "./vssourcescannerutils";
import { VSCOBOLUtils } from "./vscobolutils";

let sourceTreeView: SourceViewTree | undefined = undefined;
let sourceTreeWatcher: vscode.FileSystemWatcher | undefined = undefined;

export class VSSourceTreeViewHandler {
    static async setupSourceViewTree(config: ICOBOLSettings, reinit: boolean): Promise<void> {

        if ((config.sourceview === false || reinit) && sourceTreeView !== undefined) {
            sourceTreeWatcher?.dispose();
            sourceTreeView = undefined;
        }

        if (config.sourceview && sourceTreeView === undefined) {
            sourceTreeView = new SourceViewTree(config);
            await sourceTreeView.init(config);
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

        VSCOBOLUtils.runOrDebug(fsPath, debug);
    }
}

export class SourceViewTree implements vscode.TreeDataProvider<SourceOrFolderTreeItem> {
    private topLevelItems = new Map<string, SourceOrFolderTreeItem>();

    private cobolItems = new Map<string, SourceOrFolderTreeItem>();
    private copyBookItems = new Map<string, SourceOrFolderTreeItem>();
    private jclItems = new Map<string, SourceOrFolderTreeItem>();
    private pliItems = new Map<string, SourceOrFolderTreeItem>();
    private hlasmItems = new Map<string, SourceOrFolderTreeItem>();
    private documentItems = new Map<string, SourceOrFolderTreeItem>();
    private scriptItems = new Map<string, SourceOrFolderTreeItem>();
    private objectItems = new Map<string, SourceOrFolderTreeItem>();
    private testCaseItems = new Map<string, SourceOrFolderTreeItem>();

    private cobolItem: SourceOrFolderTreeItem;
    private copyBookItem: SourceOrFolderTreeItem;
    private jclItem: SourceOrFolderTreeItem;
    private pliItem: SourceOrFolderTreeItem;
    private hlasmItem: SourceOrFolderTreeItem;
    private documentItem: SourceOrFolderTreeItem;
    private scriptItem: SourceOrFolderTreeItem;
    private objectItem: SourceOrFolderTreeItem;
    private testCaseItem: SourceOrFolderTreeItem;

    private settings: ICOBOLSettings;

    private depth = 0;
    private maxDepth = 5;

    constructor(config: ICOBOLSettings) {
        this.settings = config;
        this.cobolItem = new SourceOrFolderTreeItem(false, "COBOL");
        this.copyBookItem = new SourceOrFolderTreeItem(false, "Copybooks");
        this.jclItem = new SourceOrFolderTreeItem(false, "JCL");
        this.hlasmItem = new SourceOrFolderTreeItem(false, "HLASM");
        this.pliItem = new SourceOrFolderTreeItem(false, "PL/I");
        this.documentItem = new SourceOrFolderTreeItem(false, "Documents");
        this.scriptItem = new SourceOrFolderTreeItem(false, "Scripts");
        this.objectItem = new SourceOrFolderTreeItem(false, "Objects");
        this.testCaseItem = new SourceOrFolderTreeItem(false, "Tests");

        this.cobolItems = this.cobolItem.children;
        this.copyBookItems = this.copyBookItem.children;
        this.jclItems = this.jclItem.children;
        this.pliItems = this.pliItem.children;
        this.hlasmItems = this.hlasmItem.children;
        this.documentItems = this.documentItem.children;
        this.scriptItems = this.scriptItem.children;
        this.objectItems = this.objectItem.children;
        this.testCaseItems = this.testCaseItem.children;

        this.topLevelItems.set(this.cobolItem.label, this.cobolItem);
        this.topLevelItems.set(this.copyBookItem.label, this.copyBookItem);

        if (config.sourceview_include_jcl_files) {
            this.topLevelItems.set(this.jclItem.label, this.jclItem);
        }

        if (config.sourceview_include_hlasm_files) {
            this.topLevelItems.set(this.hlasmItem.label, this.hlasmItem);
        }

        if (config.sourceview_include_pli_files) {
            this.topLevelItems.set(this.pliItem.label, this.pliItem);
        }

        if (config.sourceview_include_doc_files) {
            this.topLevelItems.set(this.documentItem.label, this.documentItem);
        }

        if (config.sourceview_include_script_files) {
            this.topLevelItems.set(this.scriptItem.label, this.scriptItem);
        }

        if (config.sourceview_include_object_files) {
            this.topLevelItems.set(this.objectItem.label, this.objectItem);
        }

        if (config.sourceview_include_test_files) {
            this.topLevelItems.set(this.testCaseItem.label, this.testCaseItem);
        }

        if (config.sourceview_include_jcl_files) {
            this.topLevelItems.set(this.jclItem.label, this.jclItem);
        }

        if (config.sourceview_include_hlasm_files) {
            this.topLevelItems.set(this.hlasmItem.label, this.hlasmItem);
        }

        if (config.sourceview_include_pli_files) {
            this.topLevelItems.set(this.pliItem.label, this.pliItem);
        }

        if (config.sourceview_include_doc_files) {
            this.topLevelItems.set(this.documentItem.label, this.documentItem);
        }

        if (config.sourceview_include_script_files) {
            this.topLevelItems.set(this.scriptItem.label, this.scriptItem);
        }

        if (config.sourceview_include_object_files) {
            this.topLevelItems.set(this.objectItem.label, this.objectItem);
        }

        if (config.sourceview_include_test_files) {
            this.topLevelItems.set(this.testCaseItem.label, this.testCaseItem);
        }
    }

    public async init(settings: ICOBOLSettings) {
        // file items
        let folders = VSWorkspaceFolders.get(settings);
        if (folders) {
            for (const folder of folders) {
                await this.addWorkspace(folder);
            }
        }

        // non file items
        folders = VSWorkspaceFolders.getFiltered("", settings);
        if (folders) {
            for (const folder of folders) {
                await this.addWorkspace(folder);
            }
        }
    }

    private async addWorkspace(standardFolder: vscode.WorkspaceFolder) {
        await this.addFolder(standardFolder.uri);
    }

    private async addFolder(topLevelUri: vscode.Uri) {
        if (this.depth >= this.maxDepth) {
            return;
        }

        this.depth++;
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
                    break;
                }

                case vscode.FileType.Directory: {
                    if (!VSCOBOLSourceScannerTools.ignoreDirectory(entry[0])) {
                        const subDir = vscode.Uri.parse(topLevelUri.toString() + "/" + entry[0]);
                        this.addFolder(subDir);
                    }
                    break;
                }
            }
        }
        this.depth--;
        this.refreshItems();
    }

    private getCommand(fileUri: vscode.Uri, ext: string): vscode.Command | undefined {
        const location = new vscode.Range(new vscode.Position(0, 0), new vscode.Position(0, 0));

        if (ext === "acu" || ext === "int" || ext === "gnt" || ext === "so" || ext === "dll") {
            return undefined;
        }
        const actionCommand = "vscode.open";
        return {
            arguments: [
                fileUri,
                { selection: location }
            ],
            command: actionCommand,
            title: "Open",
        };
    }

    private newSourceItem(contextValue: string, label: string, file: vscode.Uri, lnum: number, ext: string): SourceOrFolderTreeItem {
        const item = new SourceOrFolderTreeItem(true, label, file, lnum);
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
        for (const item of this.topLevelItems.values()) {
            this._onDidChangeTreeData.fire(item);
        }
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

            if (base.startsWith("Test") || base.startsWith("MFU")) {
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
        for (const [, item] of this.topLevelItems) {
            this.setTreeState(item.children, item);
        }
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
        for (const [, item] of this.topLevelItems) {
            item.children.delete(f);
        }
        this.refreshItems();
    }

    private sharedSubstring(string1: string, string2: string): string {
        let ret = "";
        let index = 1;
        while (string1.substring(0, index) === string2.substring(0, index)) {
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
                const dupItem = new SourceOrFolderTreeItem(true, sfilename.substring(sharedPrefix.length, sfilename.length), sitem.uri, sitem.line);
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

    private getTopLevelsFiltered(): SourceOrFolderTreeItem[] {
        const items: SourceOrFolderTreeItem[] = [];

        for (const item of this.topLevelItems.values()) {
            if (item.isFile === false) {
                items.push(item);
            }
        }
        return items;
    }

    public getChildren(element?: SourceOrFolderTreeItem): Thenable<SourceOrFolderTreeItem[]> {
        if (element === undefined) {
            return new Promise(resolve => {
                resolve(this.getTopLevelsFiltered());
            });
        }

        return new Promise(resolve => {
            const rtn = this.topLevelItems.get(element.label);

            if (rtn !== undefined) {
                resolve(this.getItemsFromMap(rtn.children));
            }

            resolve([]);
        });
    }

    getTreeItem(element: SourceOrFolderTreeItem): vscode.TreeItem {
        return element;
    }
}