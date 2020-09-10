import * as vscode from 'vscode';
import * as path from 'path';

import { SourceItem, SourceFolderItem } from "./sourceItem";
import { workspace } from 'vscode';
import { ICOBOLSettings } from './iconfiguration';
import { InMemoryGlobalFileCache } from './cobolsourcescanner';
import { isValidExtension } from './opencopybook';
import { getWorkspaceFolders } from './cobolfolders';
import { logException } from './extension';

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

    constructor(config: ICOBOLSettings) {
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

        if (getWorkspaceFolders()) {
            const standardFolders: string[] = ["*", "cbl/*", "jcl/*", "jclproc/*"];
            for (const standardFolder of standardFolders) {
                workspace.findFiles(standardFolder)
                    .then((allFiles) => {
                        allFiles.forEach((file) => {
                            const fileExtension = file.fsPath.split('.').pop();
                            if (fileExtension) {
                                this.addExtension(fileExtension, file);
                            }
                        });

                        this.refreshItems();
                    });
            }
        }
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

    private addExtension(ext: string, file: vscode.Uri) {
        const base = path.basename(file.fsPath);

        switch (ext?.toLowerCase()) {
            case "cobol":
            case "scbl":
            case "cob":
            case "pco":
            case "cbl":
                if (this.cobolItems.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    this.cobolItems.push(this.newSourceItem("cobol", base, file, 0));
                }
                break;
            case "ccp":
            case "dds":
            case "ss":
            case "wks":
            case "scr":
            case "cpy":
                if (this.copyBooks.find(e => e.uri?.fsPath === file.fsPath) === undefined) {
                    this.copyBooks.push(this.newSourceItem("copybook", base, file, 0));
                }
                break;
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
        if (InMemoryGlobalFileCache.copybookFileSymbols.size !== 0) {
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            for (const [i, tag] of InMemoryGlobalFileCache.copybookFileSymbols.entries()) {
                try {
                    const lastDot = i.lastIndexOf(".");

                    // if we have an extension, only process if we think it is valid
                    if (lastDot !== -1) {
                        if (isValidExtension(i)) {
                            const fileExtension = i.substr(1+lastDot);
                            if (fileExtension !== undefined) {
                                const vFile = vscode.Uri.file(i);
                                this.addExtension(fileExtension, vFile);
                            }
                        }
                    }
                }
                catch (e) {
                    logException("refreshItems", e);
                }
            }
        }

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