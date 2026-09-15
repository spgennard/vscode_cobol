import { Uri } from "vscode";

export function pathToFileURL(filePath: string): URL {
    return new URL(Uri.file(filePath).toString());
}