import { workspace, WorkspaceFolder } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";

export class VSWorkspaceFolders {
    public static get(settings: ICOBOLSettings): ReadonlyArray<WorkspaceFolder> | undefined {

        const ws = workspace.workspaceFolders;
        if (ws === undefined) {
            return ws;
        }

        const requiredSchema = "file";
        const folders_order = settings.workspacefolders_order;

        // make a map of the folders
        const folderMap: Map<string, WorkspaceFolder> = new Map<string, WorkspaceFolder>();
        for (const folder of ws) {
            if (folder.uri.scheme === requiredSchema) {             // filter out another other than file related
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

    public static get_filtered(requiredSchema:string, settings: ICOBOLSettings): ReadonlyArray<WorkspaceFolder> | undefined {
        const ws = workspace.workspaceFolders;
        if (ws === undefined) {
            return ws;
        }

        const folders_order = settings.workspacefolders_order;

        // make a map of the folders
        const folderMap: Map<string, WorkspaceFolder> = new Map<string, WorkspaceFolder>();
        for (const folder of ws) {
            if (requiredSchema.length === 0 || folder.uri.scheme === requiredSchema) {
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