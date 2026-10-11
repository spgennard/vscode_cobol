import path from "path";

export type CobmdDiagnosticSeverity = "error" | "warning";

export interface CobmdDiagnostic {
    code: string;
    message: string;
    line: number;
    severity: CobmdDiagnosticSeverity;
}

export interface CobmdFragment {
    language: string;
    target: string | undefined;
    sourceStart: number;
    sourceEnd: number;
    openingLine: number;
    startLine: number;
    endLine: number;
    content: string;
    sourceLines: string[];
}

export interface CobmdGeneratedLine {
    generatedLine: number;
    sourceLine: number;
}

export interface CobmdTarget {
    filename: string;
    content: string;
    fragments: CobmdFragment[];
    lines: CobmdGeneratedLine[];
}

export interface CobmdDocument {
    fragments: CobmdFragment[];
    targets: CobmdTarget[];
    diagnostics: CobmdDiagnostic[];
}

interface SourceLine {
    text: string;
    start: number;
    end: number;
}

interface OpenFence {
    marker: "`" | "~";
    length: number;
    language: string;
    metadata: string;
}

const cobolLanguages = new Set([
    "cobol",
    "bitlang-cobol",
    "acucobol",
    "rmcobol",
    "ilecobol",
    "cobolit"
]);

const cobolExtensions = new Set([
    ".bib", ".cbl", ".cblcpy", ".cblle", ".cblsrce", ".cob", ".cobcopy",
    ".cobol", ".copy", ".copybook", ".cpy", ".dds", ".def", ".eco", ".fd",
    ".pco", ".pdv", ".scb", ".scbl", ".sel", ".spb", ".sqlcblle", ".src",
    ".ss", ".wks"
]);

export class CobmdFormat {
    public static parse(source: string): CobmdDocument {
        const lines = this.sourceLines(source);
        const fragments: CobmdFragment[] = [];
        const diagnostics: CobmdDiagnostic[] = [];

        for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
            const fence = this.openFence(lines[lineIndex].text);
            if (fence === undefined) {
                continue;
            }

            const closingLine = this.findClosingFence(lines, lineIndex + 1, fence);
            if (closingLine === undefined) {
                if (cobolLanguages.has(fence.language)) {
                    diagnostics.push({
                        code: "unterminated-fence",
                        message: "COBOL code fence is not terminated.",
                        line: lineIndex,
                        severity: "error"
                    });
                }
                break;
            }

            if (cobolLanguages.has(fence.language)) {
                const target = this.targetFromMetadata(fence.metadata);
                if (target === undefined) {
                    diagnostics.push({
                        code: "missing-target",
                        message: "COBOL code fence requires file=<relative-filename>.",
                        line: lineIndex,
                        severity: "error"
                    });
                } else {
                    const targetError = this.validateTarget(target);
                    if (targetError !== undefined) {
                        diagnostics.push({
                            code: "invalid-target",
                            message: targetError,
                            line: lineIndex,
                            severity: "error"
                        });
                    }
                }

                const bodyLines = lines.slice(lineIndex + 1, closingLine);
                const contentStart = bodyLines[0]?.start ?? lines[closingLine].start;
                const contentEnd = bodyLines.length === 0 ? contentStart : bodyLines[bodyLines.length - 1].end;
                fragments.push({
                    language: fence.language,
                    target: target === undefined || this.validateTarget(target) !== undefined
                        ? undefined
                        : this.normalizeTarget(target),
                    sourceStart: lines[lineIndex].start,
                    sourceEnd: lines[closingLine].end,
                    openingLine: lineIndex,
                    startLine: lineIndex + 1,
                    endLine: closingLine - 1,
                    content: source.substring(contentStart, contentEnd).replace(/(?:\r\n|\r|\n)$/, ""),
                    sourceLines: bodyLines.map(line => line.text)
                });
            }

            lineIndex = closingLine;
        }

        return {
            fragments,
            targets: this.assemble(fragments, diagnostics),
            diagnostics
        };
    }

    private static assemble(fragments: CobmdFragment[], diagnostics: CobmdDiagnostic[]): CobmdTarget[] {
        const targets = new Map<string, CobmdTarget>();
        const targetSpellings = new Map<string, string>();

        for (const fragment of fragments) {
            if (fragment.target === undefined) {
                continue;
            }

            const key = fragment.target.toLocaleLowerCase();
            const previousSpelling = targetSpellings.get(key);
            if (previousSpelling !== undefined && previousSpelling !== fragment.target) {
                diagnostics.push({
                    code: "target-collision",
                    message: `Target '${fragment.target}' conflicts with '${previousSpelling}'.`,
                    line: fragment.openingLine,
                    severity: "error"
                });
                continue;
            }

            let target = targets.get(key);
            if (target === undefined) {
                target = {
                    filename: fragment.target,
                    content: "",
                    fragments: [],
                    lines: []
                };
                targets.set(key, target);
                targetSpellings.set(key, fragment.target);
            }

            target.fragments.push(fragment);
            for (let sourceOffset = 0; sourceOffset < fragment.sourceLines.length; sourceOffset++) {
                target.lines.push({
                    generatedLine: target.lines.length,
                    sourceLine: fragment.startLine + sourceOffset
                });
            }
        }

        for (const target of targets.values()) {
            const generatedLines = target.fragments.flatMap(fragment => fragment.sourceLines);
            target.content = generatedLines.length === 0 ? "" : `${generatedLines.join("\n")}\n`;
        }

        return [...targets.values()];
    }

    private static sourceLines(source: string): SourceLine[] {
        const lines: SourceLine[] = [];
        const lineExpression = /([^\r\n]*)(\r\n|\r|\n|$)/g;
        let match: RegExpExecArray | null;
        while ((match = lineExpression.exec(source)) !== null) {
            if (match[0].length === 0) {
                break;
            }
            lines.push({
                text: match[1],
                start: match.index,
                end: match.index + match[0].length
            });
        }
        return lines;
    }

    private static openFence(line: string): OpenFence | undefined {
        const match = /^ {0,3}(`{3,}|~{3,})\s*([^\s`~]+)?(?:\s+(.*?))?\s*$/.exec(line);
        if (match === null) {
            return undefined;
        }
        return {
            marker: match[1][0] as "`" | "~",
            length: match[1].length,
            language: (match[2] ?? "").toLocaleLowerCase(),
            metadata: match[3] ?? ""
        };
    }

    private static findClosingFence(lines: SourceLine[], startLine: number, fence: OpenFence): number | undefined {
        const closingExpression = new RegExp(`^ {0,3}${fence.marker === "`" ? "`" : "~"}{${fence.length},}\\s*$`);
        for (let lineIndex = startLine; lineIndex < lines.length; lineIndex++) {
            if (closingExpression.test(lines[lineIndex].text)) {
                return lineIndex;
            }
        }
        return undefined;
    }

    private static targetFromMetadata(metadata: string): string | undefined {
        const match = /(?:^|\s)file\s*=\s*(?:"([^"]+)"|'([^']+)'|([^\s]+))/i.exec(metadata);
        const target = match?.[1] ?? match?.[2] ?? match?.[3];
        return target?.trim() || undefined;
    }

    private static normalizeTarget(target: string): string {
        return target.replace(/\\/g, "/").replace(/^\.\//, "");
    }

    private static validateTarget(target: string): string | undefined {
        const normalized = this.normalizeTarget(target);
        if (normalized.startsWith("/") || /^[a-zA-Z]:\//.test(normalized) || /^[a-zA-Z][a-zA-Z0-9+.-]*:/.test(normalized)) {
            return "COBOL notebook target must be a relative path.";
        }
        if (normalized.split("/").some(segment => segment === ".." || segment.length === 0)) {
            return "COBOL notebook target cannot contain empty or parent path segments.";
        }
        if (!cobolExtensions.has(path.posix.extname(normalized).toLocaleLowerCase())) {
            return "COBOL notebook target must use a supported COBOL source extension.";
        }
        return undefined;
    }
}