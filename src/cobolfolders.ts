import { workspace, WorkspaceFolder } from "vscode";
import { VSCOBOLConfiguration } from "./configuration";

export function getWorkspaceFolders(): ReadonlyArray<WorkspaceFolder> | undefined {
    let ws = workspace.workspaceFolders;
    if (ws === undefined) {
        return ws;
    }

    let folders_order = VSCOBOLConfiguration.get().copybookdirs_order;
    if (folders_order.length === 0) {
        return workspace.workspaceFolders;
    }

    // make a map of the folders
    let folderMap: Map<string, WorkspaceFolder> = new Map<string, WorkspaceFolder>();
    for (var folder of ws) {
        folderMap.set(folder.name, folder);
    }

    let rwFolder: WorkspaceFolder[] = [];
    for (var forder of folders_order) {
        let f = folderMap.get(forder);
        if (f !== undefined) {
            rwFolder.push(f);
            folderMap.delete(forder);
        }
    }

    for (let [k, v] of folderMap) {
        rwFolder.push(v);
    }

    let f: ReadonlyArray<WorkspaceFolder> = rwFolder;
    return f;
}