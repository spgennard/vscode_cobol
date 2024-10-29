import { workspace, WorkspaceFolder } from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";

export class VSWorkspaceFolders {
    public static get(requiredSchema="file"): ReadonlyArray<WorkspaceFolder> | undefined {
        const ws = workspace.workspaceFolders;
        if (ws === undefined) {
            return ws;
        }

        const folders_order = VSCOBOLConfiguration.get_workspace_settings().workspacefolders_order;

        // make a map of the folders
        const folderMap: Map<string, WorkspaceFolder> = new Map<string, WorkspaceFolder>();
        for (const folder of ws) {
            if (requiredSchema.length === 0 || folder.uri.scheme === requiredSchema) {             // filter out another other than file related
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
        for (const [, v] of folderMap) {
            rwFolder.push(v);
        }

        const f: ReadonlyArray<WorkspaceFolder> = rwFolder;
        return f;
    }
}