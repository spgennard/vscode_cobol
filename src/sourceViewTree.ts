import * as vscode from 'vscode';
import * as path from 'path';

import { SourceItem, SourceFolderItem } from "./sourceItem";
import { workspace } from 'vscode';

export class SourceViewTree implements vscode.TreeDataProvider<SourceItem> {
    private cobolItem: SourceItem;
    private copyBook: SourceItem;
    private jclItem: SourceItem;
    private pliItem: SourceItem;
    private hlasmItem: SourceItem;

    private topLevelItem: SourceItem[] = [];

    private cobolItems: SourceFolderItem[] = [];
    private copyBooks: SourceFolderItem[] = [];
    private jclItems: SourceFolderItem[] = [];
    private pliItems: SourceFolderItem[] = [];
    private hlasmItems: SourceFolderItem[] = [];

    constructor() {
        this.copyBook = new SourceFolderItem("Cobol");
        this.cobolItem = new SourceFolderItem("Copybooks");
        this.jclItem = new SourceFolderItem("JCL");
        this.hlasmItem = new SourceFolderItem("HLASM");
        this.pliItem = new SourceFolderItem("PL/I");


        if (workspace.workspaceFolders) {
            workspace.findFiles("*")
                .then((allFiles) => {
                    allFiles.forEach((file) => {
                        var fileExtension = file.fsPath.split('.').pop();
                        if (fileExtension) {
                            this.addExtension(fileExtension, file);
                        }
                    });

                    this.refreshItemFlags();

                });
        }

        this.topLevelItem.push(this.copyBook);
        this.topLevelItem.push(this.cobolItem);
        this.topLevelItem.push(this.jclItem);
        this.topLevelItem.push(this.pliItem);
        this.topLevelItem.push(this.hlasmItem);
    }

    private getCommand(fileUri: vscode.Uri): vscode.Command | undefined {
        var location = new vscode.Range(new vscode.Position(0, 0), new vscode.Position(0, 0));

        return {
            arguments: [
                fileUri,
                { selection: location },
            ],
            command: "vscode.open",
            title: "Jump to Target",
        };
    }


    private newSourceItem(label: string, file: vscode.Uri, lnum: number): SourceItem {
        var item = new SourceItem(label, file, lnum);
        item.command = this.getCommand(file);
        return item;
    }

    private _onDidChangeTreeData: vscode.EventEmitter<SourceItem | undefined> = new vscode.EventEmitter<SourceItem | undefined>();
    readonly onDidChangeTreeData: vscode.Event<SourceItem | undefined> = this._onDidChangeTreeData.event;


    public refresh(): void {
        this._onDidChangeTreeData.fire();
    }

    private addExtension(ext: string, file: vscode.Uri) {
        var base = path.basename(file.fsPath);

        switch (ext?.toLowerCase()) {
            case "scbl":
            case "cob":
            case "pco":
            case "cbl":
                if (this.cobolItems.find(e => e.uri === file) === undefined) {
                    this.cobolItems.push(this.newSourceItem(base, file, 0));
                }
                break;
            case "dds":
            case "ss":
            case "wks":
            case "scr":
            case "cpy":
                if (this.copyBooks.find(e => e.uri === file) === undefined) {
                    this.copyBooks.push(this.newSourceItem(base, file, 0));
                }
                break;
            case "jcl":
            case "job":
            case "cntl":
            case "prc":
            case "proc":
                if (this.jclItems.find(e => e.uri === file) === undefined) {
                    this.jclItems.push(this.newSourceItem(base, file, 0));
                }
                break;
            case "hlasm":
            case "asm":
            case "s":
            case "asmpgm":
            case "mac":
            case "asmmac":
                if (this.hlasmItems.find(e => e.uri === file) === undefined) {
                    this.hlasmItems.push(this.newSourceItem(base, file, 0));
                }
                break;

            case "pl1":
            case "pli":
            case "plinc":
            case "pc":
            case "pci":
            case "pcx":
            case "inc":
                if (this.pliItems.find(e => e.uri === file) === undefined) {
                    this.pliItems.push(this.newSourceItem(base, file, 0));
                }
                break;
        }
    }

    private refreshItemFlags() {
        if (this.cobolItems.length !== 0) {
            this.cobolItem.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        } else {
            this.cobolItem.collapsibleState = vscode.TreeItemCollapsibleState.None;
        }

        if (this.copyBooks.length !== 0) {
            this.copyBook.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        } else {
            this.copyBook.collapsibleState = vscode.TreeItemCollapsibleState.None;
        }

        if (this.jclItems.length !== 0) {
            this.jclItem.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        } else {
            this.jclItem.collapsibleState = vscode.TreeItemCollapsibleState.None;
        }

        if (this.hlasmItems.length !== 0) {
            this.hlasmItem.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        } else {
            this.hlasmItem.collapsibleState = vscode.TreeItemCollapsibleState.None;
        }

        if (this.pliItems.length !== 0) {
            this.pliItem.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        } else {
            this.pliItem.collapsibleState = vscode.TreeItemCollapsibleState.None;
        }

        this.refresh();

    }

    public async checkFile(vuri: vscode.Uri) {
        var fileExtension = vuri.fsPath.split('.').pop();
        if (fileExtension !== undefined) {
            this.addExtension(fileExtension, vuri);
            this.refreshItemFlags();
        }
    }

    public clearFile(vuri: vscode.Uri): void {
        this.cobolItems = this.cobolItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.copyBooks = this.copyBooks.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.jclItems = this.jclItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.hlasmItems = this.hlasmItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.pliItems = this.pliItems.filter(item => item.uri?.fsPath !== vuri.fsPath);
        this.refreshItemFlags();
        this.refresh();
    }

    public getChildren(element?: SourceItem): Thenable<SourceItem[]> {
        if (element === undefined) {
            return new Promise(resolve => {
                resolve(this.topLevelItem);
            });
        }

        return new Promise(resolve => {
            let rtn: SourceItem[] = [];

            if (element.label === "Copybooks") {
                rtn = this.copyBooks;
            }

            if (element.label === "Cobol") {
                rtn = this.cobolItems;
            }

            if (element.label === "JCL") {
                rtn = this.jclItems;
            }

            if (element.label === "PL/I") {
                rtn = this.pliItems;
            }

            if (element.label === "HLASM") {
                rtn = this.hlasmItems;
            }

            resolve(rtn);
        });
    }

    getTreeItem(element: SourceItem): vscode.TreeItem {
        return element;
    }
}