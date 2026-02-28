import * as vscode from "vscode";
import { COBOLTokenStyle } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { ESourceFormat } from "./externalfeatures";

export class COBOLFoldingRangeProvider implements vscode.FoldingRangeProvider {

    private static readonly wordRegex = /[a-zA-Z][a-zA-Z0-9-]*/g;

    // Block verbs that commonly use END-xxx scope terminators in structured COBOL
    private static readonly primaryBlockOpeners = new Set(["if", "evaluate", "perform"]);

    // Additional verbs that have END-xxx forms but are only tracked when they are the
    // first significant word on the line (to avoid accumulating thousands of unmatched
    // entries for period-terminated usage such as "DISPLAY x." or "ADD 1 TO x.")
    private static readonly secondaryBlockOpeners = new Set([
        "read", "write", "start", "delete", "return", "search",
        "string", "unstring", "accept", "display", "multiply",
        "divide", "add", "subtract", "compute", "call", "invoke", "exec"
    ]);

    public async provideFoldingRanges(
        document: vscode.TextDocument,
        _context: vscode.FoldingContext,
        token: vscode.CancellationToken
    ): Promise<vscode.FoldingRange[]> {
        const ranges: vscode.FoldingRange[] = [];
        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

        const sf = VSCOBOLSourceScanner.getCachedObject(document, config);

        // Structural folding from parsed tokens (divisions, sections, paragraphs, etc.)
        if (sf !== undefined) {
            for (const t of sf.tokensInOrder) {
                if (token.isCancellationRequested) {
                    return ranges;
                }

                if (t.ignoreInOutlineView) {
                    continue;
                }

                // Only fold spans of 2+ lines
                if (t.rangeEndLine <= t.rangeStartLine) {
                    continue;
                }

                switch (t.tokenType) {
                    case COBOLTokenStyle.Division:
                    case COBOLTokenStyle.Section:
                    case COBOLTokenStyle.Paragraph:
                    case COBOLTokenStyle.ProgramId:
                    case COBOLTokenStyle.ClassId:
                    case COBOLTokenStyle.InterfaceId:
                    case COBOLTokenStyle.EnumId:
                    case COBOLTokenStyle.ValueTypeId:
                    case COBOLTokenStyle.MethodId:
                    case COBOLTokenStyle.FunctionId:
                    case COBOLTokenStyle.Constructor:
                    case COBOLTokenStyle.Property:
                    case COBOLTokenStyle.Declaratives:
                    case COBOLTokenStyle.Exec:
                        ranges.push(new vscode.FoldingRange(t.rangeStartLine, t.rangeEndLine));
                        break;

                    case COBOLTokenStyle.CopyBook:
                    case COBOLTokenStyle.CopyBookInOrOf:
                        ranges.push(new vscode.FoldingRange(t.rangeStartLine, t.rangeEndLine, vscode.FoldingRangeKind.Imports));
                        break;

                    case COBOLTokenStyle.Region:
                        ranges.push(new vscode.FoldingRange(t.rangeStartLine, t.rangeEndLine, vscode.FoldingRangeKind.Region));
                        break;
                }
            }
        }

        // Comment block folding and block-structure folding from a lightweight line scan
        this.addCommentAndBlockFolds(document, sf?.sourceFormat, ranges, token);

        return ranges;
    }

    /**
     * Lightweight line-based scan to detect:
     * - Consecutive comment-line blocks
     * - IF ... END-IF
     * - EVALUATE ... END-EVALUATE
     * - PERFORM ... END-PERFORM
     * - Other END-xxx block verbs (only when the opener is the first word on a line)
     */
    private addCommentAndBlockFolds(
        document: vscode.TextDocument,
        sourceFormat: ESourceFormat | undefined,
        ranges: vscode.FoldingRange[],
        token: vscode.CancellationToken
    ): void {
        const lineCount = document.lineCount;
        const isFree = sourceFormat === ESourceFormat.free;

        // ── Comment block detection ──────────────────────────────────────
        let commentBlockStart = -1;

        // ── Block-structure stack for IF / EVALUATE / PERFORM / etc ──────
        // Each entry: [keyword, line]
        const blockStack: [string, number][] = [];

        for (let i = 0; i < lineCount; i++) {
            if (token.isCancellationRequested) {
                return;
            }

            const lineText = document.lineAt(i).text;
            const isComment = this.isCommentLine(lineText, isFree);

            // ── Comment blocks ───────────────────────────────────────────
            if (isComment) {
                if (commentBlockStart === -1) {
                    commentBlockStart = i;
                }
            } else {
                if (commentBlockStart !== -1 && (i - 1) > commentBlockStart) {
                    ranges.push(new vscode.FoldingRange(commentBlockStart, i - 1, vscode.FoldingRangeKind.Comment));
                }
                commentBlockStart = -1;
            }

            // ── Block structure detection ────────────────────────────────
            const significant = this.getSignificantText(lineText, isFree);
            if (significant.length === 0) {
                continue;
            }

            const words = this.extractWords(significant);
            if (words.length === 0) {
                continue;
            }

            // Track whether the first word on this line is a block opener
            // (secondary verbs like READ, CALL, DISPLAY are only pushed when first)
            let firstWordHandled = false;

            for (let w = 0; w < words.length; w++) {
                const word = words[w];

                // ── Handle END-xxx closers ───────────────────────────────
                if (word.startsWith("end-")) {
                    const opener = word.substring(4); // "end-if" → "if"
                    const idx = this.findLastBlock(blockStack, opener);
                    if (idx !== -1) {
                        const entry = blockStack[idx];
                        blockStack.splice(idx, 1);
                        if (i > entry[1]) {
                            ranges.push(new vscode.FoldingRange(entry[1], i));
                        }
                    }
                    continue;
                }

                // ── Handle block openers ─────────────────────────────────
                if (COBOLFoldingRangeProvider.primaryBlockOpeners.has(word)) {
                    blockStack.push([word, i]);
                } else if (!firstWordHandled && w === 0 && COBOLFoldingRangeProvider.secondaryBlockOpeners.has(word)) {
                    // Only push secondary verbs when they are the first word on the line,
                    // which is the typical pattern for block-scoped usage
                    blockStack.push([word, i]);
                }

                firstWordHandled = true;
            }
        }

        // Close final comment block if file ends with comments
        if (commentBlockStart !== -1 && (lineCount - 1) > commentBlockStart) {
            ranges.push(new vscode.FoldingRange(commentBlockStart, lineCount - 1, vscode.FoldingRangeKind.Comment));
        }
    }

    /**
     * Find the last matching block entry on the stack (searching from the top).
     */
    private findLastBlock(stack: [string, number][], keyword: string): number {
        for (let i = stack.length - 1; i >= 0; i--) {
            if (stack[i][0] === keyword) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Determine whether a line is a comment line.
     * Fixed format: column 7 is '*' or '/'
     * Free format: line starts with '*>' after trimming
     */
    private isCommentLine(lineText: string, isFree: boolean): boolean {
        if (isFree) {
            const trimmed = lineText.trimStart();
            return trimmed.startsWith("*>");
        }

        // Fixed/variable format — column 7 (0-based index 6) is the indicator area
        if (lineText.length > 6) {
            const indicator = lineText[6];
            return indicator === "*" || indicator === "/" || indicator === "d" || indicator === "D";
        }

        // Blank or very short line — not a comment
        return false;
    }

    /**
     * Get the significant text from a line, skipping the sequence/indicator area in fixed format.
     */
    private getSignificantText(lineText: string, isFree: boolean): string {
        if (isFree) {
            return lineText.trimStart();
        }

        // Fixed format: columns 8-72 (0-based 7-71) are Area A + Area B
        if (lineText.length <= 7) {
            return "";
        }
        // Check indicator column
        if (lineText.length > 6) {
            const indicator = lineText[6];
            if (indicator === "*" || indicator === "/" || indicator === "d" || indicator === "D" || indicator === "-") {
                return "";
            }
        }
        const endCol = Math.min(lineText.length, 72);
        return lineText.substring(7, endCol).trimStart();
    }

    /**
     * Extract lowercased words from a text string, splitting on whitespace and common COBOL delimiters.
     */
    private extractWords(text: string): string[] {
        const words: string[] = [];
        // Match words including hyphenated COBOL keywords like END-IF, END-PERFORM
        const regex = COBOLFoldingRangeProvider.wordRegex;
        regex.lastIndex = 0;
        let match;
        while ((match = regex.exec(text)) !== null) {
            words.push(match[0].toLowerCase());
        }
        return words;
    }
}
