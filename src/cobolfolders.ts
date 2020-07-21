import { workspace, WorkspaceFolder } from "vscode";

export function getWorkspaceFolders(): ReadonlyArray<WorkspaceFolder> | undefined {
    return workspace.workspaceFolders;
}