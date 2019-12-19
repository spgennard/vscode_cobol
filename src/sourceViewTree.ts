import * as vscode from 'vscode';
import * as path from 'path';

import { SourceItem, SourceFolderItem } from "./sourceItem";
import { workspace } from 'vscode';

export class SourceViewTree implements vscode.TreeDataProvider<SourceItem> {
    private cobolItem: SourceItem;
    private copyBook: SourceItem;

    private topLevelItem: SourceItem[] = [];

    private cobolItems: SourceFolderItem[] = [];
    private copyBooks: SourceFolderItem[] = [];

    constructor() {
        this.copyBook = new SourceFolderItem("Cobol");
        this.cobolItem = new SourceFolderItem("Copybooks");

        this.cobolItem.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
        this.copyBook.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;


        if (workspace.workspaceFolders) {
            workspace.findFiles("*")
                .then((allFiles) => {
                    allFiles.forEach((file) => {
                        var fileExtension = file.fsPath.split('.').pop();
                        var base = path.basename(file.fsPath);
                        switch (fileExtension?.toLowerCase()) {
                            case "scbl":
                            case "cob":
                            case "pco":
                            case "cbl": this.cobolItems.push(this.newSourceItem(base, file, 0));
                                break;
                            case "dds":
                            case "ss":
                            case "wks":
                            case "scr":
                            case "cpy": this.copyBooks.push(this.newSourceItem(base, file, 0));
                                break;
                        }
                    });
                });
        }

        this.topLevelItem.push(this.copyBook);
        this.topLevelItem.push(this.cobolItem);
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

    public async checkFile(vuri: vscode.Uri) {

        var fileExtension = vuri.fsPath.split('.').pop();

        const found: SourceItem[] = [];

        switch (fileExtension?.toLowerCase()) {
            case "cbl":
                if (this.cobolItems.find(e => e.uri === vuri) === undefined) {
                    this.cobolItems.push(new SourceItem("xx", vuri, 0));
                }
        }
        this.refresh();
    }

    public clearFile(vuri: vscode.Uri): void {
        this.cobolItems = this.cobolItems.filter(item => item.uri !== vuri);
        this.copyBooks = this.copyBooks.filter(item => item.uri !== vuri);
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

            resolve(rtn);
        });
    }

    getTreeItem(element: SourceItem): vscode.TreeItem {
        return element;
    }
}