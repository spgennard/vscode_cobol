import * as path from "path"
import * as vscode from "vscode"
import * as URI from "vscode-uri"

const copyBookExtensions = new Set<string>([
    ".cpy",
    ".cbl"
])

export class CopyBookDragDropProvider implements vscode.DocumentDropEditProvider {
    async provideDocumentDropEdits(document: vscode.TextDocument,
        _position: vscode.Position,
        dataTransfer: vscode.DataTransfer,
        token: vscode.CancellationToken): Promise<vscode.DocumentDropEdit | undefined> {

        // Return the text or snippet to insert at the drop location.
        const snippet = await tryGetUriListSnippet(document, dataTransfer, token)
        return snippet ? new vscode.DocumentDropEdit(snippet) : undefined
    }
}

async function tryGetUriListSnippet(document: vscode.TextDocument,
    dataTransfer: vscode.DataTransfer,
    token: vscode.CancellationToken): Promise<vscode.SnippetString | undefined> {
    // Get droped files uris
    const urlList = await dataTransfer.get("text/uri-list")?.asString()
    if (!urlList || token.isCancellationRequested) {
        return undefined
    }

    const uris: vscode.Uri[] = []
    for (const resource of urlList.split("\n")) {
        uris.push(vscode.Uri.parse(resource.replace("\r", "")))
    }

    if (!uris.length) {
        return
    }
    // Drop location uri
    const docUri = document.uri

    const snippet = new vscode.SnippetString()

    // Get uri for each uris list value
    uris.forEach((uri, i) => {
        const filePath = docUri.scheme === uri.scheme && docUri.authority === uri.authority
            ? encodeURI(path.relative(URI.Utils.dirname(docUri).fsPath, uri.fsPath).replace(/\\/g, "/"))
            : uri.toString(false)

        const ext = URI.Utils.extname(uri).toLowerCase()
        snippet.appendText(copyBookExtensions.has(ext) ? `copy "${filePath}"` : "")

        // Add a line break if multiple droped documents
        if (i <= uris.length - 1 && uris.length > 1) {
            snippet.appendText("\n")
        }
    })
    return snippet
}
