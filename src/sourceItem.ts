import * as vscode from "vscode";
import * as path from "path";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const themedIconPath = (name: string): any => {
	return {
		light: path.join(__filename, "..", "..", "images", "light", name),
		dark: path.join(__filename, "..", "..", "images", "dark", name)
	};
};

export class SourceOrFolderTreeItem extends vscode.TreeItem {
    constructor(
        isFile: boolean,
        public readonly label: string,
        public readonly uri?: vscode.Uri,
        public readonly line?: number
    ) {
        super(label, vscode.TreeItemCollapsibleState.None);
        this.line = line;
        this.isFile = isFile;
        this.iconPath = isFile ? themedIconPath("sourceitem.svg") : themedIconPath("folder.svg");
        this.contextValue = isFile ? "Source" : "Source Folder";
        this.children = new Map<string, SourceOrFolderTreeItem>();
    }

    public isFile:boolean;
    public iconPath; 
    contextValue:string;

    public children:Map<string, SourceOrFolderTreeItem>;
}