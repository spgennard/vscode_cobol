

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
                    VSHelpAndFeedViewHandler.openUrl("https://community.microfocus.com/cobol/");
                    break;

                case FeedBackItem.review:
                    VSHelpAndFeedViewHandler.openUrl("https://marketplace.visualstudio.com/items?itemName=bitlang.cobol&ssr=false#review-details");
                    break;

                case FeedBackItem.courses:
                    VSHelpAndFeedViewHandler.openUrl("https://www.rocketsoftware.com/learn-cobol");
                    break;

                case FeedBackItem.introToOO:
                    VSHelpAndFeedViewHandler.openUrl("https://supportline.microfocus.com/Documentation/books/VisualCOBOL/Intro_to_OO_Programming_for_COBOL_Developers.pdf");
                    break;
            }
            
        });
        return;
    }
}

enum FeedBackItem {
    getStarted = "Get Started",
    readDocs = "Read Documentation",
    review = "Review extension",
    reviewIssues = "Review Issues",
    reportIssue = "Report Issue",
    joinCommunity = "Join the 'Rocket Software' Community",
    courses = "'Rocket Software' On-Demand Courses",
    introToOO = "Introduction to OO Programming"
}

export class HelpAndFeedbackTree implements vscode.TreeDataProvider<FeedBackItem> {
    ALL_FEEDBACK_ITEMS = [
        FeedBackItem.getStarted,
        FeedBackItem.courses,
        FeedBackItem.introToOO,
        FeedBackItem.joinCommunity,
        FeedBackItem.review,
        FeedBackItem.reportIssue
    ];

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

            case FeedBackItem.review:
                iconPath = new vscode.ThemeIcon("book");
                break;

            case FeedBackItem.courses:
                iconPath = new vscode.ThemeIcon("play-circle");
                break;

            case FeedBackItem.introToOO:
                iconPath = new vscode.ThemeIcon("file-pdf");
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
