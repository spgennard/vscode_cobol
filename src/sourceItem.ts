import * as vscode from 'vscode';
import * as path from 'path';

const themedIconPath = (name: string): any => {
	return {
		light: path.join(__filename, '..', '..', 'images', 'light', name),
		dark: path.join(__filename, '..', '..', 'images', 'dark', name)
	};
};

export class SourceFolderItem extends vscode.TreeItem {

    constructor(
        public readonly label: string,
        public readonly uri?: vscode.Uri,
        public readonly line?: number,
    ) {
        super(label, vscode.TreeItemCollapsibleState.None);
    }

    public iconPath = themedIconPath("folder.svg");

    contextValue = 'Source Folder';
}

export class SourceItem extends vscode.TreeItem {

    constructor(
        public readonly label: string,
        public readonly uri?: vscode.Uri,
        public readonly line?: number
    ) {
        super(label, vscode.TreeItemCollapsibleState.None);
    }

    public iconPath = themedIconPath("sourceitem.svg");

    contextValue = 'Source';
}