import { Buffer } from "buffer";
import * as vscode from "vscode";
import { CobmdFormat, CobmdFragment } from "./cobmdformat";

export const cobmdNotebookType = "cobol-markdown-notebook";

interface CobmdCellMetadata {
    cobmd?: {
        language?: string;
        target?: string;
        originalValue?: string;
        originalSource?: string;
    };
}

interface CobmdNotebookMetadata {
    cobmd?: {
        originalSource?: string;
        cells?: CobmdOriginalCell[];
    };
}

interface CobmdOriginalCell {
    kind: vscode.NotebookCellKind;
    languageId: string;
    value: string;
    target?: string;
}

export class CobmdNotebookSerializer implements vscode.NotebookSerializer {
    public deserializeNotebook(content: Uint8Array): vscode.NotebookData {
        const source = Buffer.from(content).toString("utf8");
        const parsed = CobmdFormat.parse(source);
        const cells: vscode.NotebookCellData[] = [];
        let sourceOffset = 0;

        for (const fragment of parsed.fragments) {
            if (fragment.sourceStart > sourceOffset) {
                this.appendMarkupCell(cells, source.substring(sourceOffset, fragment.sourceStart));
            }
            cells.push(this.codeCell(fragment, source.substring(fragment.sourceStart, fragment.sourceEnd)));
            sourceOffset = fragment.sourceEnd;
        }
        if (sourceOffset < source.length || cells.length === 0) {
            this.appendMarkupCell(cells, source.substring(sourceOffset));
        }

        const notebook = new vscode.NotebookData(cells);
        notebook.metadata = {
            cobmd: {
                originalSource: source,
                cells: cells.map(cell => this.originalCell(cell))
            }
        } satisfies CobmdNotebookMetadata;
        return notebook;
    }

    public serializeNotebook(data: vscode.NotebookData): Uint8Array {
        const metadata = data.metadata as CobmdNotebookMetadata | undefined;
        if (metadata?.cobmd?.originalSource !== undefined && this.cellsAreUnchanged(data.cells, metadata.cobmd.cells)) {
            return Buffer.from(metadata.cobmd.originalSource, "utf8");
        }

        const output: string[] = [];
        for (const cell of data.cells) {
            if (cell.kind === vscode.NotebookCellKind.Markup) {
                output.push(cell.value);
                continue;
            }

            const cellMetadata = cell.metadata as CobmdCellMetadata | undefined;
            const language = cellMetadata?.cobmd?.language ?? "cobol";
            const target = cellMetadata?.cobmd?.target;
            const originalValue = cellMetadata?.cobmd?.originalValue;
            const originalSource = cellMetadata?.cobmd?.originalSource;
            if (originalSource !== undefined && originalValue === cell.value) {
                output.push(originalSource);
                continue;
            }

            const fileMetadata = target === undefined ? "" : ` file=${this.quoteTarget(target)}`;
            const value = cell.value.replace(/(?:\r\n|\r|\n)+$/, "");
            output.push(`\`\`\`${language}${fileMetadata}\n${value}\n\`\`\`\n`);
        }
        return Buffer.from(output.join(""), "utf8");
    }

    private appendMarkupCell(cells: vscode.NotebookCellData[], value: string): void {
        if (value.length !== 0) {
            cells.push(new vscode.NotebookCellData(vscode.NotebookCellKind.Markup, value, "markdown"));
        }
    }

    private codeCell(fragment: CobmdFragment, originalSource: string): vscode.NotebookCellData {
        const cell = new vscode.NotebookCellData(vscode.NotebookCellKind.Code, fragment.content, "COBOL");
        cell.metadata = {
            cobmd: {
                language: fragment.language,
                target: fragment.target,
                originalValue: fragment.content,
                originalSource
            }
        } satisfies CobmdCellMetadata;
        return cell;
    }

    private originalCell(cell: vscode.NotebookCellData): CobmdOriginalCell {
        const metadata = cell.metadata as CobmdCellMetadata | undefined;
        return {
            kind: cell.kind,
            languageId: cell.languageId,
            value: cell.value,
            target: metadata?.cobmd?.target
        };
    }

    private cellsAreUnchanged(cells: vscode.NotebookCellData[], originals: CobmdOriginalCell[] | undefined): boolean {
        return originals !== undefined && cells.length === originals.length && cells.every((cell, index) => {
            const metadata = cell.metadata as CobmdCellMetadata | undefined;
            const original = originals[index];
            return cell.kind === original.kind &&
                cell.languageId === original.languageId &&
                cell.value === original.value &&
                metadata?.cobmd?.target === original.target;
        });
    }

    private quoteTarget(target: string): string {
        return /\s/.test(target) ? `"${target.replace(/"/g, "\\\"")}"` : target;
    }
}

export function registerCobmdNotebook(context: vscode.ExtensionContext): void {
    context.subscriptions.push(
        vscode.workspace.registerNotebookSerializer(cobmdNotebookType, new CobmdNotebookSerializer(), {
            transientOutputs: true
        })
    );
}