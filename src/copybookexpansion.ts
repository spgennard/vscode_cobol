import { COBOLCopybookToken } from "./cobolsourcescanner";

export interface CopybookOccurrence {
    sourceUri: string;
    startLine: number;
    startColumn: number;
}

export interface CopybookLineOrigin {
    sourceUri: string;
    sourceLine: number;
}

export interface CopybookExpansionLine {
    text: string;
    origin: CopybookLineOrigin | undefined;
}

export interface CopybookExpansionResult {
    content: string;
    lines: CopybookExpansionLine[];
}

export interface CopybookExpansionCancellation {
    readonly isCancellationRequested: boolean;
}

export class CopybookExpansionBuilder {
    public static occurrence(copybook: COBOLCopybookToken): CopybookOccurrence | undefined {
        const token = copybook.token;
        if (token === undefined) {
            return undefined;
        }
        return {
            sourceUri: token.filenameAsURI,
            startLine: token.startLine,
            startColumn: token.startColumn
        };
    }

    public static build(root: COBOLCopybookToken, cancellation?: CopybookExpansionCancellation): CopybookExpansionResult {
        const lines: CopybookExpansionLine[] = [];
        this.appendCopybook(root, lines, cancellation);
        return {
            content: lines.map(line => line.text).join("\n"),
            lines
        };
    }

    public static originalContent(root: COBOLCopybookToken): string {
        const source = root.statementInformation?.sourceHandler;
        if (source === undefined) {
            return "";
        }

        const lines: string[] = [];
        for (let sourceLine = 0; sourceLine < source.getLineCount(); sourceLine++) {
            lines.push(source.getLine(sourceLine, true) ?? "");
        }
        return lines.join("\n");
    }

    private static appendCopybook(
        copybook: COBOLCopybookToken,
        output: CopybookExpansionLine[],
        cancellation?: CopybookExpansionCancellation
    ): void {
        if (cancellation?.isCancellationRequested) {
            return;
        }

        const statement = copybook.statementInformation;
        const source = statement?.sourceHandler;
        if (statement === undefined || source === undefined) {
            this.appendUnavailable(copybook, output);
            return;
        }

        const children = [...copybook.children].sort((left, right) => {
            const leftStatement = left.statementInformation;
            const rightStatement = right.statementInformation;
            return (leftStatement?.startLineNumber ?? 0) - (rightStatement?.startLineNumber ?? 0) ||
                (leftStatement?.startCol ?? 0) - (rightStatement?.startCol ?? 0);
        });
        let childIndex = 0;

        for (let sourceLine = 0; sourceLine < source.getLineCount(); sourceLine++) {
            if (cancellation?.isCancellationRequested) {
                return;
            }

            const child = children[childIndex];
            const childStatement = child?.statementInformation;
            if (childStatement !== undefined && childStatement.startLineNumber === sourceLine) {
                if (childStatement.sourceHandler === undefined) {
                    const lastStatementLine = Math.max(sourceLine, childStatement.endLineNumber);
                    for (let statementLine = sourceLine; statementLine <= lastStatementLine; statementLine++) {
                        output.push({
                            text: source.getUpdatedLine(statementLine) ?? source.getLine(statementLine, true) ?? "",
                            origin: {
                                sourceUri: source.getUriAsString(),
                                sourceLine: statementLine
                            }
                        });
                    }
                    this.appendUnavailable(child, output);
                } else {
                    this.appendCopybook(child, output, cancellation);
                }
                sourceLine = Math.max(sourceLine, childStatement.endLineNumber);
                childIndex++;
                continue;
            }

            output.push({
                text: source.getUpdatedLine(sourceLine) ?? source.getLine(sourceLine, true) ?? "",
                origin: {
                    sourceUri: source.getUriAsString(),
                    sourceLine
                }
            });
        }
    }

    private static appendUnavailable(copybook: COBOLCopybookToken, output: CopybookExpansionLine[]): void {
        const tokenName = copybook.token?.tokenName ?? "unknown";
        output.push({
            text: `*> Unable to expand copybook ${tokenName}`,
            origin: undefined
        });
    }
}