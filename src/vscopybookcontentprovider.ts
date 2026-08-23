import * as path from "path";
import * as vscode from "vscode";
import { CopybookExpansionBuilder, CopybookExpansionResult, CopybookOccurrence } from "./copybookexpansion";
import { COBOLCopybookToken } from "./cobolsourcescanner";
import { ExtensionDefaults } from "./extensionDefaults";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSExternalFeatures } from "./vsexternalfeatures";

const expandedCopybookScheme = "cobol-expanded";

interface CachedExpansion {
    result: CopybookExpansionResult;
    originalContent: string;
    originalUri: vscode.Uri;
}

type CopybookDocumentView = "expanded" | "original";

interface CopybookDocumentDescriptor {
    occurrence: CopybookOccurrence;
    view: CopybookDocumentView;
}

export class VSCopybookContentProvider implements vscode.TextDocumentContentProvider, vscode.DefinitionProvider, vscode.Disposable {
    private readonly changeEmitter = new vscode.EventEmitter<vscode.Uri>();
    private readonly cache = new Map<string, CachedExpansion>();
    private readonly disposables: vscode.Disposable[] = [];

    public readonly onDidChange = this.changeEmitter.event;

    public static register(context: vscode.ExtensionContext): VSCopybookContentProvider {
        const provider = new VSCopybookContentProvider();
        context.subscriptions.push(
            provider,
            vscode.workspace.registerTextDocumentContentProvider(expandedCopybookScheme, provider),
            vscode.languages.registerDefinitionProvider({ scheme: expandedCopybookScheme }, provider),
            vscode.commands.registerCommand("cobolplugin.openCopybook", occurrence => provider.openOriginal(occurrence)),
            vscode.commands.registerCommand("cobolplugin.openExpandedCopybook", occurrence => provider.openExpanded(occurrence)),
            vscode.commands.registerCommand("cobolplugin.compareExpandedCopybook", occurrence => provider.compareExpanded(occurrence)),
            vscode.commands.registerCommand("cobolplugin.refreshExpandedCopybook", resource => provider.refresh(resource))
        );
        return provider;
    }

    private constructor() {
        this.disposables.push(
            vscode.workspace.onDidSaveTextDocument(() => void this.invalidateAll()),
            vscode.workspace.onDidChangeConfiguration(event => {
                if (event.affectsConfiguration(ExtensionDefaults.defaultEditorConfig)) {
                    void this.invalidateAll();
                }
            })
        );
    }

    public async provideTextDocumentContent(uri: vscode.Uri, token: vscode.CancellationToken): Promise<string> {
        const descriptor = this.descriptorFromUri(uri);
        const expansion = await this.getExpansion(descriptor.occurrence, token);
        return descriptor.view === "original" ? expansion.originalContent : expansion.result.content;
    }

    public provideDefinition(
        document: vscode.TextDocument,
        position: vscode.Position
    ): vscode.ProviderResult<vscode.Definition> {
        const cached = this.cache.get(document.uri.toString());
        const origin = cached?.result.lines[position.line]?.origin;
        if (origin === undefined) {
            return undefined;
        }
        return new vscode.Location(vscode.Uri.parse(origin.sourceUri), new vscode.Position(origin.sourceLine, 0));
    }

    public dispose(): void {
        this.changeEmitter.dispose();
        for (const disposable of this.disposables) {
            disposable.dispose();
        }
    }

    private async openOriginal(requestedOccurrence?: CopybookOccurrence): Promise<void> {
        const occurrence = await this.resolveOccurrence(requestedOccurrence);
        if (occurrence === undefined) {
            return;
        }
        const expansion = await this.getExpansion(occurrence);
        await vscode.window.showTextDocument(expansion.originalUri, { preview: true });
    }

    private async openExpanded(requestedOccurrence?: CopybookOccurrence): Promise<void> {
        const occurrence = await this.resolveOccurrence(requestedOccurrence);
        if (occurrence === undefined) {
            return;
        }
        const uri = this.uriForOccurrence(occurrence);
        const document = await vscode.workspace.openTextDocument(uri);
        await vscode.languages.setTextDocumentLanguage(document, ExtensionDefaults.defaultCOBOLLanguage);
        await vscode.window.showTextDocument(document, { preview: true, viewColumn: vscode.ViewColumn.Beside });
    }

    private async compareExpanded(requestedOccurrence?: CopybookOccurrence): Promise<void> {
        const occurrence = await this.resolveOccurrence(requestedOccurrence);
        if (occurrence === undefined) {
            return;
        }
        const originalUri = this.uriForOccurrence(occurrence, "original");
        const expandedUri = this.uriForOccurrence(occurrence, "expanded");
        const expansion = await this.getExpansion(occurrence);
        await Promise.all([
            vscode.workspace.openTextDocument(originalUri),
            vscode.workspace.openTextDocument(expandedUri)
        ]);
        await vscode.commands.executeCommand(
            "vscode.diff",
            originalUri,
            expandedUri,
            `${path.basename(expansion.originalUri.path)} (Original vs Expanded)`
        );
    }

    private async refresh(requested?: CopybookOccurrence | vscode.Uri): Promise<void> {
        const activeUri = vscode.window.activeTextEditor?.document.uri;
        let resolvedOccurrence: CopybookOccurrence | undefined;
        if (this.isResourceUri(requested)) {
            resolvedOccurrence = requested.scheme === expandedCopybookScheme
                ? this.occurrenceFromUri(requested)
                : undefined;
        } else {
            resolvedOccurrence = requested ?? (activeUri?.scheme === expandedCopybookScheme
                ? this.occurrenceFromUri(activeUri)
                : undefined);
        }
        if (resolvedOccurrence === undefined) {
            return;
        }
        await this.invalidateScanner(resolvedOccurrence);
        this.cache.delete(this.uriForOccurrence(resolvedOccurrence).toString());
        for (const document of vscode.workspace.textDocuments) {
            if (document.uri.scheme === expandedCopybookScheme &&
                this.sameOccurrence(this.occurrenceFromUri(document.uri), resolvedOccurrence)) {
                this.changeEmitter.fire(document.uri);
            }
        }
    }

    private async invalidateAll(): Promise<void> {
        const openUris = vscode.workspace.textDocuments
            .filter(document => document.uri.scheme === expandedCopybookScheme)
            .map(document => document.uri);
        this.cache.clear();
        for (const uri of openUris) {
            await this.invalidateScanner(this.occurrenceFromUri(uri));
            this.changeEmitter.fire(uri);
        }
    }

    private async invalidateScanner(occurrence: CopybookOccurrence): Promise<void> {
        const document = await vscode.workspace.openTextDocument(vscode.Uri.parse(occurrence.sourceUri));
        const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
        VSCOBOLSourceScanner.removeCachedObject(document, settings);
    }

    private async resolveOccurrence(requested?: CopybookOccurrence): Promise<CopybookOccurrence | undefined> {
        if (requested !== undefined) {
            return requested;
        }
        const editor = vscode.window.activeTextEditor;
        if (editor === undefined) {
            return undefined;
        }
        if (editor.document.uri.scheme === expandedCopybookScheme) {
            return this.occurrenceFromUri(editor.document.uri);
        }

        const settings = VSCOBOLConfiguration.get_resource_settings(editor.document, VSExternalFeatures);
        const scanner = VSCOBOLSourceScanner.getCachedObject(editor.document, settings);
        if (scanner !== undefined) {
            const cursorLine = editor.selection.active.line;
            for (const entries of scanner.copyBooksUsed.values()) {
                for (const entry of entries) {
                    const statement = entry.statementInformation;
                    const occurrence = CopybookExpansionBuilder.occurrence(entry);
                    if (statement !== undefined && occurrence !== undefined &&
                        occurrence.sourceUri === editor.document.uri.toString() &&
                        cursorLine >= statement.startLineNumber && cursorLine <= statement.endLineNumber) {
                        return occurrence;
                    }
                }
            }
        }
        await vscode.window.showInformationMessage("Place the cursor on a resolved COPY statement and try again.");
        return undefined;
    }

    private async getExpansion(
        occurrence: CopybookOccurrence,
        cancellation?: vscode.CancellationToken
    ): Promise<CachedExpansion> {
        const expandedUri = this.uriForOccurrence(occurrence);
        const cacheKey = expandedUri.toString();
        const cached = this.cache.get(cacheKey);
        if (cached !== undefined) {
            return cached;
        }

        const sourceDocument = await vscode.workspace.openTextDocument(vscode.Uri.parse(occurrence.sourceUri));
        const settings = VSCOBOLConfiguration.get_resource_settings(sourceDocument, VSExternalFeatures);
        const scanner = VSCOBOLSourceScanner.getCachedObject(sourceDocument, settings);
        const copybook = scanner === undefined ? undefined : this.findOccurrence(scanner.copyBooksUsed, occurrence);
        const sourceHandler = copybook?.statementInformation?.sourceHandler;
        if (copybook === undefined || sourceHandler === undefined) {
            throw new Error("The selected copybook expansion is no longer available. Refresh the source document and try again.");
        }

        const expansion = {
            result: CopybookExpansionBuilder.build(copybook, cancellation),
            originalContent: CopybookExpansionBuilder.originalContent(copybook),
            originalUri: vscode.Uri.parse(sourceHandler.getUriAsString())
        };
        this.cache.set(cacheKey, expansion);
        return expansion;
    }

    private findOccurrence(
        copybooks: Map<string, COBOLCopybookToken[]>,
        occurrence: CopybookOccurrence
    ): COBOLCopybookToken | undefined {
        for (const entries of copybooks.values()) {
            for (const entry of entries) {
                const token = entry.token;
                if (token?.filenameAsURI === occurrence.sourceUri &&
                    token.startLine === occurrence.startLine &&
                    token.startColumn === occurrence.startColumn) {
                    return entry;
                }
            }
        }
        return undefined;
    }

    private sameOccurrence(left: CopybookOccurrence, right: CopybookOccurrence): boolean {
        return left.sourceUri === right.sourceUri &&
            left.startLine === right.startLine &&
            left.startColumn === right.startColumn;
    }

    private isResourceUri(value: CopybookOccurrence | vscode.Uri | undefined): value is vscode.Uri {
        return value !== undefined && "scheme" in value;
    }

    private uriForOccurrence(
        occurrence: CopybookOccurrence,
        view: CopybookDocumentView = "expanded"
    ): vscode.Uri {
        const sourceName = path.basename(vscode.Uri.parse(occurrence.sourceUri).path) || "copybook";
        return vscode.Uri.from({
            scheme: expandedCopybookScheme,
            path: `/${sourceName}.${occurrence.startLine + 1}.${view}.cbl`,
            query: encodeURIComponent(JSON.stringify({ occurrence, view } satisfies CopybookDocumentDescriptor))
        });
    }

    private occurrenceFromUri(uri: vscode.Uri): CopybookOccurrence {
        return this.descriptorFromUri(uri).occurrence;
    }

    private descriptorFromUri(uri: vscode.Uri): CopybookDocumentDescriptor {
        const parsed = JSON.parse(decodeURIComponent(uri.query)) as CopybookDocumentDescriptor | CopybookOccurrence;
        if ("occurrence" in parsed) {
            return parsed;
        }
        return { occurrence: parsed, view: "expanded" };
    }
}