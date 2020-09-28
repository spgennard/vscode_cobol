import * as vscode from 'vscode';
import * as path from 'path';

import { SourceItem, SourceFolderItem } from "./sourceItem";
import { workspace } from 'vscode';
import { ICOBOLSettings } from './iconfiguration';
import { getWorkspaceFolders } from './cobolfolders';
import { logException, logMessage } from './extension';
import VSCOBOLSourceScanner from './vscobolscanner';

export class SourceViewTree implements vscode.TreeDataProvider<SourceItem> {
    private cobolItem: SourceItem;
    private copyBook: SourceItem;
    private jclItem: SourceItem;
    private pliItem: SourceItem;
    private hlasmItem: SourceItem;
    private documentItem: SourceItem;
    private scriptItem: SourceItem;

    private topLevelItem: SourceItem[] = [];

    private cobolItems: SourceFolderItem[] = [];
    private copyBooks: SourceFolderItem[] = [];
    private jclItems: SourceFolderItem[] = [];
    private pliItems: SourceFolderItem[] = [];
    private hlasmItems: SourceFolderItem[] = [];
    private documentIems: SourceFolderItem[] = [];
    private scriptItems: SourceFolderItem[] = [];

    private settings: ICOBOLSettings;

    constructor(config: ICOBOLSettings) {
        this.settings = config;

        this.cobolItem = new SourceFolderItem("COBOL");
        this.copyBook = new SourceFolderItem("Copybooks");
        this.jclItem = new SourceFolderItem("JCL");
        this.hlasmItem = new SourceFolderItem("HLASM");
        this.pliItem = new SourceFolderItem("PL/I");
        this.documentItem = new SourceFolderItem("Documents");
        this.scriptItem = new SourceFolderItem("Scripts");

        this.topLevelItem.push(this.cobolItem);
        this.topLevelItem.push(this.copyBook);
        this.topLevelItem.push(this.jclItem);
        this.topLevelItem.push(this.hlasmItem);
        this.topLevelItem.push(this.pliItem);
        this.topLevelItem.push(this.documentItem);
        this.topLevelItem.push(this.scriptItem);

        const folders = getWorkspaceFolders();

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
                    if (!VSCOBOLSourceScanner.ignoreDirectory(entry[0])) {
                        const subDir = path.join(topLevel, entry[0]);
                        this.addFolder(subDir);
                    }
                }
                    break;
            }
        }
        this.refreshItems();
    }

    private getCommand(fileUri: vscode.Uri): vscode.Command | undefined {
        const location = new vscode.Range(new vscode.Position(0, 0), new vscode.Position(0, 0));

        return {
            arguments: [
                fileUri,
                { selection: location },
            ],
            command: "vscode.open",
            title: "Open",
        };
    }

    private newSourceItem(contextValue: string, label: string, file: vscode.Uri, lnum: number): SourceItem {
        const item = new SourceItem(label, file, lnum);
        item.command = this.getCommand(file);
        item.contextValue = contextValue;
        item.tooltip = file.fsPath;
        return item;
    }

    private _onDidChangeTreeData: vscode.EventEmitter<SourceItem | undefined> = new vscode.EventEmitter<SourceItem | undefined>();
    readonly onDidChangeTreeData: vscode.Event<SourceItem | undefined> = this._onDidChangeTreeData.event;

    private refreshAll(): void {
        this._onDidChangeTreeData.fire(this.cobolItem);
        this._onDidChangeTreeData.fire(this.copyBook);
        this._onDidChangeTreeData.fire(this.jclItem);
        this._onDidChangeTreeData.fire(this.pliItem);
        this._onDidChangeTreeData.fire(this.hlasmItem);
        this._onDidChangeTreeData.fire(this.documentItem);
        this._onDidChangeTreeData.fire(this.scriptItem);
    }

    private addExtensionIfInList(ext: string, file: vscode.Uri, validExtensions: string[], sourceItem: string, base: string, items: SourceFolderItem[]): boolean {
        let found = false;
        for (const validExtension of validExtensions) {
            if (validExtension.length > 0 && ext === validExtension) {
                if (items.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    items.push(this.newSourceItem(sourceItem, base, file, 0));
                    found = true;
                }

            }
        }

        return found;
    }

    private addExtension(ext: string, file: vscode.Uri) {
        const base = path.basename(file.fsPath);

        this.addExtensionIfInList(ext?.toLowerCase(), file, this.settings.program_extensions, "cobol", base, this.cobolItems);
        this.addExtensionIfInList(ext?.toLowerCase(), file, this.settings.copybookexts, "copybook", base, this.copyBooks);

        switch (ext?.toLowerCase()) {
            case "jcl":
            case "job":
            case "cntl":
            case "prc":
            case "proc":
                if (this.jclItems.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    this.jclItems.push(this.newSourceItem("jcl", base, file, 0));
                }
                break;
            case "hlasm":
            case "asm":
            case "s":
            case "asmpgm":
            case "mac":
            case "mlc":
            case "asmmac":
                if (this.hlasmItems.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    this.hlasmItems.push(this.newSourceItem("hlasm", base, file, 0));
                }
                break;

            case "pl1":
            case "pli":
            case "plinc":
            case "pc":
            case "pci":
            case "pcx":
            case "inc":
                if (this.pliItems.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    this.pliItems.push(this.newSourceItem("pli", base, file, 0));
                }
                break;

            case "md":
            case "txt":
            case "html":
                if (this.documentIems.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    this.documentIems.push(this.newSourceItem("document", base, file, 0));
                }
                break;
            case "sh":
            case "bat":
                if (this.scriptItems.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    this.scriptItems.push(this.newSourceItem("scripts", base, file, 0));
                }
                break;
        }
    }

    private setTreeState(items: SourceFolderItem[], item: SourceItem) {
        if (items.length !== 0) {
            item.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        } else {
            item.collapsibleState = vscode.TreeItemCollapsibleState.None;
        }
    }

    private refreshItems() {
        this.setTreeState(this.cobolItems, this.cobolItem);
        this.setTreeState(this.copyBooks, this.copyBook);
        this.setTreeState(this.jclItems, this.jclItem);
        this.setTreeState(this.hlasmItems, this.hlasmItem);
        this.setTreeState(this.pliItems, this.pliItem);
        this.setTreeState(this.documentIems, this.documentItem);
        this.setTreeState(this.scriptItems, this.scriptItem);

        this.refreshAll();
    }

    public async checkFile(vuri: vscode.Uri): Promise<void> {
        try {
            const fileExtension = vuri.fsPath.split('.').pop();
            if (fileExtension !== undefined) {
                this.addExtension(fileExtension, vuri);
                this.refreshItems();
            }
        }
        catch (e) {
            logException("checkFile", e);
        }
    }

    public clearFile(vuri: vscode.Uri): void {
        this.cobolItems = this.cobolItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.copyBooks = this.copyBooks.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.jclItems = this.jclItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.hlasmItems = this.hlasmItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.pliItems = this.pliItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.documentIems = this.documentIems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.scriptItems = this.scriptItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.refreshItems();
    }

    public getChildren(element?: SourceItem): Thenable<SourceItem[]> {
        if (element === undefined) {
            return new Promise(resolve => {
                resolve(this.topLevelItem);
            });
        }

        return new Promise(resolve => {
            let rtn: SourceItem[] = [];

            switch (element.label) {
                case "Copybooks": rtn = this.copyBooks; break;
                case "COBOL": rtn = this.cobolItems; break;
                case "JCL": rtn = this.jclItems; break;
                case "PL/I": rtn = this.pliItems; break;
                case "HLASM": rtn = this.hlasmItems; break;
                case "Documents": rtn = this.documentIems; break;
                case "Scripts": rtn = this.scriptItems; break;
            }

            resolve(rtn);
        });
    }

    getTreeItem(element: SourceItem): vscode.TreeItem {
        return element;
    }
}