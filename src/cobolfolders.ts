import { workspace, WorkspaceFolder } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";

export function getVSWorkspaceFolders(): ReadonlyArray<WorkspaceFolder> | undefined {
    const ws = workspace.workspaceFolders;
    if (ws === undefined) {
        return ws;
    }

    const folders_order = VSCOBOLConfiguration.get().workspacefolders_order;

    // make a map of the folders
    const folderMap: Map<string, WorkspaceFolder> = new Map<string, WorkspaceFolder>();
    for (const folder of ws) {
        if (folder.uri.scheme === "file") {             // filter out another other than file related
            folderMap.set(folder.name, folder);
        }
    }

    const rwFolder: WorkspaceFolder[] = [];
    if (folders_order.length !== 0) {
        for (const folder of folders_order) {
            const f = folderMap.get(folder);
            if (f !== undefined) {
                rwFolder.push(f);
                folderMap.delete(folder);
            }
        }
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    for (const [k, v] of folderMap) {
        rwFolder.push(v);
    }

    const f: ReadonlyArray<WorkspaceFolder> = rwFolder;
    return f;
}