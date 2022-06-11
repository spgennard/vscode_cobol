

import * as vscode from "vscode";
import { ICOBOLSettings } from "./iconfiguration";

export class VSHelpAndFeedViewHandler {
    public static openUrl(url: string) {
        vscode.commands.executeCommand("vscode.open", url);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    static setupSourceViewTree(config: ICOBOLSettings, reinit: boolean): void {

        const helpviewProvider = vscode.window.createTreeView("help-and-feedback-view", {
            treeDataProvider: new HelpAndFeedbackTree(),
        });

        helpviewProvider.onDidChangeSelection((e) => {
            const item = e.selection[0];
            switch (item) {
                case FeedBackItem.getStarted:
                    VSHelpAndFeedViewHandler.openUrl("https://github.com/spgennard/vscode_cobol");
                    break;

                case FeedBackItem.readDocs:
                    VSHelpAndFeedViewHandler.openUrl("https://github.com/spgennard/vscode_cobol#readme");
                    break;

                case FeedBackItem.reviewIssues:
                    VSHelpAndFeedViewHandler.openUrl("https://github.com/spgennard/vscode_cobol/issues");
                    break;

                case FeedBackItem.reportIssue:
                    VSHelpAndFeedViewHandler.openUrl("https://github.com/spgennard/vscode_cobol/issues/new/choose");
                    break;

                case FeedBackItem.joinCommunity:
                    VSHelpAndFeedViewHandler.openUrl("https://community.microfocus.com/cobol/visualcobol/");
                    break;

                case FeedBackItem.followUs:
                    VSHelpAndFeedViewHandler.openUrl("https://twitter.com/spgennard");
                    break;
            }
        });
        return;
    }

}

enum FeedBackItem {
    getStarted = "Get Started",
    readDocs = "Read Documentation",
    reviewIssues = "Review Issues",
    reportIssue = "Report Issue",
    joinCommunity = "Join the 'Micro Focus' Community!",
    followUs = "Follow our Progress",
}

export class HelpAndFeedbackTree implements vscode.TreeDataProvider<FeedBackItem> {
    ALL_FEEDBACK_ITEMS = Object.values(FeedBackItem) as FeedBackItem[];

    constructor() {
        return;
    }

    private _onDidChangeTreeData: vscode.EventEmitter<FeedBackItem | undefined | null | void> = new vscode.EventEmitter<FeedBackItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<FeedBackItem | undefined | null | void> = this._onDidChangeTreeData.event;

    public getTreeItem(element: FeedBackItem): vscode.TreeItem {
        let iconPath: vscode.ThemeIcon;

        switch (element) {
            case FeedBackItem.getStarted:
                iconPath = new vscode.ThemeIcon("star");
                break;

            case FeedBackItem.readDocs:
                iconPath = new vscode.ThemeIcon("book");
                break;

            case FeedBackItem.reviewIssues:
                iconPath = new vscode.ThemeIcon("issues");
                break;

            case FeedBackItem.reportIssue:
                iconPath = new vscode.ThemeIcon("comment");
                break;

            case FeedBackItem.joinCommunity:
                iconPath = new vscode.ThemeIcon("organization");
                break;

            case FeedBackItem.followUs:
                iconPath = new vscode.ThemeIcon("twitter");
                break;
        }

        return {
            label: element.toString(),
            collapsibleState: vscode.TreeItemCollapsibleState.None,
            iconPath
        };
    }

    public getChildren(element?: FeedBackItem): vscode.ProviderResult<FeedBackItem[]> {
        if (element === undefined) {
            return this.ALL_FEEDBACK_ITEMS;
        }

        return [];
    }
}
