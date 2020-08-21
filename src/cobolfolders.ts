import { workspace, WorkspaceFolder } from "vscode";
import { VSCOBOLConfiguration } from "./configuration";

export function getWorkspaceFolders(): ReadonlyArray<WorkspaceFolder> | undefined {
    const ws = workspace.workspaceFolders;
    if (ws === undefined) {
        return ws;
    }

    const folders_order = VSCOBOLConfiguration.get().copybookdirs_order;
    if (folders_order.length === 0) {
        return workspace.workspaceFolders;
    }

    // make a map of the folders
    const folderMap: Map<string, WorkspaceFolder> = new Map<string, WorkspaceFolder>();
    for (const folder of ws) {
        folderMap.set(folder.name, folder);
    }

    const rwFolder: WorkspaceFolder[] = [];
    for (const forder of folders_order) {
        const f = folderMap.get(forder);
        if (f !== undefined) {
            rwFolder.push(f);
            folderMap.delete(forder);
        }
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    for (const [k, v] of folderMap) {
        rwFolder.push(v);
    }

    const f: ReadonlyArray<WorkspaceFolder> = rwFolder;
    return f;
}