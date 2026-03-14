import * as vscode from "vscode";
import { ESourceFormat } from "./externalfeatures";
import { SourceFormat } from "./sourceformat";
import { VSCodeSourceHandlerLite } from "./vscodesourcehandler";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSExtensionUtils } from "./vsextutis";
import { VSLogger } from "./vslogger";

/**
 * Represents a parsed COBOL line with its structural components.
 * Fixed/variable format lines have:
 *   columns 1-6:  sequence number area
 *   column 7:     indicator area (* = comment, - = continuation, / = page eject, D = debug)
 *   columns 8-11: Area A
 *   columns 12-72: Area B
 *   columns 73-80: identification area (optional)
 *
 * Free format lines have no fixed column structure.
 */
interface COBOLParsedLine {
    /** Original line text */
    original: string;
    /** Sequence number area (cols 1-6 in fixed/variable) */
    sequenceArea: string;
    /** Indicator character (col 7 in fixed/variable) */
    indicator: string;
    /** Whether this is a comment line */
    isComment: boolean;
    /** Whether this is a continuation line */
    isContinuation: boolean;
    /** Whether this is a debug line */
    isDebug: boolean;
    /** Whether this is a blank/empty line */
    isBlank: boolean;
    /** Whether this line contains a source format directive */
    isSourceFormatDirective: boolean;
    /** The content part of the line (Area A + Area B in fixed format) */
    content: string;
    /** Identification area (cols 73-80 in fixed format) */
    identificationArea: string;
    /** Whether this line is a page eject (/ in indicator) */
    isPageEject: boolean;
}

/**
 * Returns the user-friendly label for a source format.
 */
function formatLabel(format: ESourceFormat): string {
    switch (format) {
        case ESourceFormat.fixed: return "Fixed";
        case ESourceFormat.free: return "Free";
        case ESourceFormat.variable: return "Variable";
        default: return format.toString();
    }
}

// Regex patterns for embedded source format directives
const sourceFormatDirectivePatterns = [
    />>SOURCE\s+FORMAT\s+(IS\s+)?(FREE|FIXED|VARIABLE)/i,
    /\$SET\s+SOURCEFORMAT\s*\(\s*(FREE|FIXED|VARIABLE)\s*\)/i,
    /SOURCEFORMAT\s*\(\s*(FREE|FIXED|VARIABLE)\s*\)/i,
];

/** Fixed-format right margin (last column of Area B). */
const FIXED_RIGHT_MARGIN = 72;
/** Fixed-format content width: cols 8-72 = 65 characters. */
const FIXED_CONTENT_WIDTH = 65;
/** Sequence number area width: cols 1-6 = 6 characters. */
const SEQUENCE_AREA_WIDTH = 6;
/** The first content column (col 8) index into the raw line (0-based). */
const CONTENT_START_INDEX = 7;
/** Blank sequence number area (6 spaces). */
const BLANK_SEQUENCE_AREA = " ".repeat(SEQUENCE_AREA_WIDTH);

/**
 * Detect whether a line contains an embedded source format directive.
 */
function isSourceFormatDirectiveLine(line: string): boolean {
    const trimmed = line.trim();
    for (const pattern of sourceFormatDirectivePatterns) {
        if (pattern.test(trimmed)) {
            return true;
        }
    }
    return false;
}

/**
 * Create a COBOLParsedLine with default values.
 */
function createDefaultParsedLine(line: string): COBOLParsedLine {
    return {
        original: line,
        sequenceArea: "",
        indicator: " ",
        isComment: false,
        isContinuation: false,
        isDebug: false,
        isBlank: false,
        isSourceFormatDirective: false,
        content: "",
        identificationArea: "",
        isPageEject: false,
    };
}

/**
 * Apply indicator-area flags to a parsed line based on the indicator character.
 */
function applyIndicatorFlags(result: COBOLParsedLine): void {
    switch (result.indicator) {
        case "*":
            result.isComment = true;
            break;
        case "/":
            result.isComment = true;
            result.isPageEject = true;
            break;
        case "-":
            result.isContinuation = true;
            break;
        case "D":
        case "d":
            result.isDebug = true;
            break;
    }
}

/**
 * Shared parsing logic for fixed and variable format lines.
 * Both formats share cols 1-6 (sequence area) and col 7 (indicator).
 * The `extractContent` callback controls how the content area is extracted
 * (fixed format truncates at col 72; variable format reads to end of line).
 */
function parseColumnarLine(
    line: string,
    extractContent: (line: string) => { content: string; identificationArea: string },
): COBOLParsedLine {
    const result = createDefaultParsedLine(line);

    if (line.trimEnd().length === 0) {
        result.isBlank = true;
        return result;
    }

    if (line.length >= SEQUENCE_AREA_WIDTH) {
        result.sequenceArea = line.substring(0, SEQUENCE_AREA_WIDTH);
    } else {
        // Short line — malformed source. Preserve entire line as content
        // to avoid silent data loss.
        result.content = line;
        return result;
    }

    if (line.length >= CONTENT_START_INDEX) {
        result.indicator = line[SEQUENCE_AREA_WIDTH];
    }

    applyIndicatorFlags(result);

    if (line.length > CONTENT_START_INDEX) {
        const extracted = extractContent(line);
        result.content = extracted.content;
        result.identificationArea = extracted.identificationArea;
    }

    result.isSourceFormatDirective = isSourceFormatDirectiveLine(line);

    return result;
}

/**
 * Parse a single line from Fixed format COBOL source.
 */
function parseFixedLine(line: string): COBOLParsedLine {
    return parseColumnarLine(line, (l) => {
        const endCol = Math.min(l.length, FIXED_RIGHT_MARGIN);
        return {
            content: l.substring(CONTENT_START_INDEX, endCol),
            identificationArea: l.length > FIXED_RIGHT_MARGIN ? l.substring(FIXED_RIGHT_MARGIN) : "",
        };
    });
}

/**
 * Parse a single line from Variable format COBOL source.
 * Variable format is like fixed but without the 72-column right margin restriction,
 * meaning columns 1-6 are sequence area, column 7 is indicator, and content extends
 * beyond column 72 with no identification area.
 */
function parseVariableLine(line: string): COBOLParsedLine {
    return parseColumnarLine(line, (l) => ({
        content: l.substring(CONTENT_START_INDEX),
        identificationArea: "",
    }));
}

/**
 * Parse a single line from Free format COBOL source.
 */
function parseFreeLine(line: string): COBOLParsedLine {
    const result = createDefaultParsedLine(line);

    if (line.trimEnd().length === 0) {
        result.isBlank = true;
        return result;
    }

    const trimmed = line.trimStart();

    // Free format comments start with *>
    if (trimmed.startsWith("*>")) {
        result.isComment = true;
        result.content = line;
        result.isSourceFormatDirective = isSourceFormatDirectiveLine(line);
        return result;
    }

    // In free format, & at the end of a line is ALWAYS a continuation marker,
    // regardless of whether the code is currently inside a string literal.
    // (The & itself is never part of the source content — it is always stripped.)
    if (trimmed.endsWith("&")) {
        result.isContinuation = true;
    }

    // Free format debug lines start with D or >>D
    if (trimmed.toUpperCase().startsWith(">>D")) {
        result.isDebug = true;
    }

    result.content = line;
    result.isSourceFormatDirective = isSourceFormatDirectiveLine(line);

    return result;
}

/**
 * Merge fixed/variable format continuation lines.
 *
 * In fixed and variable formats, a '-' in column 7 means this line continues
 * the previous line. The content from the continuation line (starting from the
 * first non-space character after column 7) is appended to the previous line's
 * content.
 *
 * Returns an array of "logical lines" where continuations have been merged,
 * along with mappings back to original line numbers.
 */
interface MergedLine {
    /** The merged content after joining all continuations */
    content: string;
    /** The leading whitespace / indentation from the first line */
    leadingIndent: string;
    /** The original line indices that were merged */
    sourceLines: number[];
    /** Whether the first line was a comment */
    isComment: boolean;
    /** Whether the first line was a debug line */
    isDebug: boolean;
    /** Whether this was blank */
    isBlank: boolean;
    /** Whether this is a source format directive */
    isSourceFormatDirective: boolean;
    /** Whether the first line was a page eject */
    isPageEject: boolean;
    /** Original comment text (for comment lines) */
    commentText: string;
}

/**
 * Merge continuation lines from fixed or variable format parsed lines.
 */
function mergeContinuationLines(parsedLines: COBOLParsedLine[]): MergedLine[] {
    const merged: MergedLine[] = [];

    let i = 0;
    while (i < parsedLines.length) {
        const pl = parsedLines[i];

        if (pl.isBlank) {
            merged.push({
                content: "",
                leadingIndent: "",
                sourceLines: [i],
                isComment: false,
                isDebug: false,
                isBlank: true,
                isSourceFormatDirective: false,
                isPageEject: false,
                commentText: "",
            });
            i++;
            continue;
        }

        if (pl.isComment) {
            merged.push({
                content: pl.content,
                leadingIndent: "",
                sourceLines: [i],
                isComment: true,
                isDebug: false,
                isBlank: false,
                isSourceFormatDirective: pl.isSourceFormatDirective,
                isPageEject: pl.isPageEject,
                commentText: pl.content,
            });
            i++;
            continue;
        }

        // Start of a regular (possibly continued) line
        const sourceLines = [i];
        let contentStr = pl.content;
        const indent = getLeadingWhitespace(pl.content);

        // Look ahead for continuation lines
        let j = i + 1;
        while (j < parsedLines.length && parsedLines[j].isContinuation) {
            sourceLines.push(j);
            const contLine = parsedLines[j];
            const contContent = contLine.content;

            // Determine if we're currently inside an open string literal
            // by checking for unmatched quotes in the accumulated content
            if (analyzeStringLiteralState(contentStr).isOpen) {
                // COBOL continuation rule for string literals:
                // - The previous line's content is kept as-is (trailing spaces are significant)
                // - The continuation line's first non-blank character is a quote delimiter
                //   that must be consumed (it's a continuation marker, not part of the value)
                const trimmedCont = contContent.trimStart();
                if (trimmedCont.length > 0 && (trimmedCont[0] === '"' || trimmedCont[0] === "'")) {
                    // Skip the continuation quote delimiter
                    contentStr += trimmedCont.substring(1);
                } else {
                    // No quote found — unusual, append as-is to avoid data loss
                    contentStr += contContent;
                }
            } else {
                // Non-literal continuation: trim whitespace at the join boundary
                contentStr = contentStr.trimEnd();
                contentStr += contContent.trimStart();
            }
            j++;
        }

        merged.push({
            content: contentStr,
            leadingIndent: indent,
            sourceLines: sourceLines,
            isComment: false,
            isDebug: pl.isDebug,
            isBlank: false,
            isSourceFormatDirective: pl.isSourceFormatDirective,
            isPageEject: false,
            commentText: "",
        });

        i = j;
    }

    return merged;
}

function getLeadingWhitespace(text: string): string {
    const match = text.match(/^(\s*)/);
    return match ? match[1] : "";
}

/**
 * Result of analyzing text for unclosed string literals.
 */
interface StringLiteralState {
    /** Whether the text ends inside an unclosed string literal */
    isOpen: boolean;
    /** The quote character that opened the unclosed literal ('"' or "'"); only meaningful when isOpen is true */
    openQuote: string;
}

/**
 * Analyze the given text for unclosed string literals, tracking double and
 * single quotes independently and accounting for COBOL doubled-quote
 * escaping (e.g., "" inside a double-quoted string).
 */
function analyzeStringLiteralState(text: string): StringLiteralState {
    let inDouble = false;
    let inSingle = false;
    for (let i = 0; i < text.length; i++) {
        const ch = text[i];
        if (inDouble) {
            if (ch === '"') {
                if (i + 1 < text.length && text[i + 1] === '"') {
                    i++; // skip doubled-quote escape
                } else {
                    inDouble = false;
                }
            }
        } else if (inSingle) {
            if (ch === "'") {
                if (i + 1 < text.length && text[i + 1] === "'") {
                    i++;
                } else {
                    inSingle = false;
                }
            }
        } else {
            if (ch === '"') {
                inDouble = true;
            } else if (ch === "'") {
                inSingle = true;
            }
        }
    }
    return {
        isOpen: inDouble || inSingle,
        openQuote: inSingle ? "'" : '"',
    };
}

/**
 * Convert a comment from fixed/variable format to free format.
 * In fixed format comments, column 7 is * or / and the comment text is in cols 8-72.
 * In free format, comments use *> prefix.
 */
function fixedCommentToFree(parsed: COBOLParsedLine): string {
    // Extract the comment text — everything after the indicator
    let commentText = parsed.content;

    if (parsed.isPageEject) {
        // Page eject (/) — preserve as a free format comment with a
        // unique marker that won't collide with regular comments
        return "*>! PAGE EJECT" + (commentText.trim().length > 0 ? " " + commentText.trim() : "");
    }

    // If the comment text already starts with *>, it's a new-style inline comment
    // embedded in fixed format; just return it
    if (commentText.trimStart().startsWith("*>")) {
        return commentText.trimStart();
    }

    const trimmedComment = commentText.trimEnd();
    if (trimmedComment.length === 0) {
        return "*>";
    }

    // Preserve relative indentation from Area A/B
    // In fixed format, Area A starts at col 8 (index 0 in content = col 8)
    // We'll keep the leading spaces as-is for indentation
    return "*> " + commentText.trimEnd();
}

/**
 * Extract the comment body from a free-format comment line.
 * Removes the *> prefix and exactly one separator space, preserving any
 * additional leading whitespace (which represents indentation in the original source).
 */
function extractFreeCommentBody(trimmedLine: string): string {
    if (trimmedLine.startsWith("*>")) {
        let body = trimmedLine.substring(2);
        // Remove exactly one separator space after *> (added during fixed→free conversion)
        if (body.startsWith(" ")) {
            body = body.substring(1);
        }
        return body.trimEnd();
    }
    return trimmedLine;
}

/**
 * Convert a comment from free format to fixed format.
 * Returns an array of lines because long comments must be split to fit
 * within the 72-column fixed format limit.
 */
function freeCommentToFixed(line: string): string[] {
    const trimmed = line.trimStart();

    // Detect page eject marker first (uses "!" sentinel to distinguish from
    // regular comments that happen to contain "PAGE EJECT" text)
    if (trimmed.startsWith("*>!")) {
        const commentBody = trimmed.substring(3).trim();
        if (/^PAGE\s+EJECT$/i.test(commentBody)) {
            return [buildFixedLine("/", "")];
        }
        if (/^PAGE\s+EJECT\s+/i.test(commentBody)) {
            const ejectText = commentBody.replace(/^PAGE\s+EJECT\s+/i, "");
            return splitTextForFixedLines("/", " " + ejectText);
        }
    }

    const commentBody = extractFreeCommentBody(trimmed);

    if (commentBody.length === 0) {
        return [buildFixedLine("*", "")];
    }

    return splitTextForFixedLines("*", " " + commentBody);
}

/**
 * Split long text across multiple fixed-format lines with word-boundary-aware
 * splitting. Uses the given indicator for the first line and '*' for overflow lines.
 * The text parameter should include any leading space for readability (e.g. " commentBody").
 */
function splitTextForFixedLines(indicator: string, text: string, maxWidth: number = FIXED_CONTENT_WIDTH): string[] {
    if (text.length === 0) {
        return [buildFixedLine(indicator, "")];
    }
    if (text.length <= maxWidth) {
        return [buildFixedLine(indicator, text)];
    }
    const lines: string[] = [];
    let remaining = text;
    let isFirst = true;
    while (remaining.length > 0) {
        const ind = isFirst ? indicator : "*";
        if (remaining.length <= maxWidth) {
            lines.push(buildFixedLine(ind, remaining));
            break;
        }
        let splitPos = maxWidth;
        for (let s = maxWidth; s >= Math.max(0, maxWidth - 20); s--) {
            if (remaining[s] === " ") {
                splitPos = s;
                break;
            }
        }
        lines.push(buildFixedLine(ind, remaining.substring(0, splitPos)));
        remaining = remaining.substring(splitPos).trimStart();
        isFirst = false;
    }
    return lines;
}

/**
 * Convert a comment from free format to variable format.
 * Variable format has no 72-column limit so no splitting is needed.
 */
function freeCommentToVariable(line: string): string {
    const trimmed = line.trimStart();

    // Detect page eject marker first (uses "!" sentinel)
    if (trimmed.startsWith("*>!")) {
        const ejectBody = trimmed.substring(3).trim();
        if (/^PAGE\s+EJECT$/i.test(ejectBody)) {
            return buildVariableLine("/", "");
        }
        if (/^PAGE\s+EJECT\s+/i.test(ejectBody)) {
            const ejectText = ejectBody.replace(/^PAGE\s+EJECT\s+/i, "");
            return buildVariableLine("/", " " + ejectText);
        }
    }

    const commentBody = extractFreeCommentBody(trimmed);

    if (commentBody.length === 0) {
        return buildVariableLine("*", "");
    }

    return buildVariableLine("*", " " + commentBody);
}

/**
 * Build a fixed-format line from components.
 * Callers must ensure content fits within FIXED_CONTENT_WIDTH characters (cols 8-72).
 * Use splitForFixedFormat() for content that may exceed this limit.
 */
function buildFixedLine(indicator: string, content: string): string {
    return BLANK_SEQUENCE_AREA + indicator + content;
}

/**
 * Build a variable-format line from components.
 * Same structure as fixed but with no right-margin restriction.
 */
function buildVariableLine(indicator: string, content: string): string {
    return BLANK_SEQUENCE_AREA + indicator + content;
}

/**
 * Split a long content line into multiple fixed-format lines with appropriate
 * continuation. This handles the case where free/variable format content that
 * extends beyond column 72 must be split.
 *
 * For non-string content, we split at word boundaries where possible.
 * For string literals, we split the literal with proper continuation characters.
 */
function splitForFixedFormat(indicator: string, content: string): string[] {
    const lines: string[] = [];

    // If the content fits, just return it
    const fullLine = BLANK_SEQUENCE_AREA + indicator + content;
    if (fullLine.length <= FIXED_RIGHT_MARGIN) {
        lines.push(fullLine);
        return lines;
    }

    // We need to split the line.
    // Strategy: split at word boundaries when possible
    let remaining = content;
    let isFirstLine = true;

    while (remaining.length > 0) {
        const ind = isFirstLine ? indicator : "-";
        
        if (remaining.length <= FIXED_CONTENT_WIDTH) {
            lines.push(buildFixedLine(ind, remaining));
            break;
        }

        // Find a good split point — look for a space near the end of the available width
        let splitPos = FIXED_CONTENT_WIDTH;
        const searchStart = Math.max(0, FIXED_CONTENT_WIDTH - 20);

        // Look for a space to split at
        let bestSplit = -1;
        for (let s = FIXED_CONTENT_WIDTH; s >= searchStart; s--) {
            if (remaining[s] === " ") {
                bestSplit = s;
                break;
            }
        }

        if (bestSplit > 0) {
            splitPos = bestSplit;
        }

        // Check if we're in the middle of a string literal
        const stringState = analyzeStringLiteralState(remaining.substring(0, splitPos));
        if (stringState.isOpen) {
            // We're inside a string literal — we must split the string properly.
            // In COBOL fixed format continuation of string literals:
            // - The current line ends at the split point (truncated at col 72)
            // - The continuation line has '-' in col 7, and the literal resumes
            //   with the same quote delimiter in Area B (col 12)
            const openQuote = stringState.openQuote;
            const chunk = remaining.substring(0, splitPos);
            lines.push(buildFixedLine(ind, chunk));
            // Continuation line: 4 spaces indent (to col 12) + opening quote + rest of literal
            remaining = "    " + openQuote + remaining.substring(splitPos);
        } else {
            const chunk = remaining.substring(0, splitPos);
            lines.push(buildFixedLine(ind, chunk));
            // Skip past any spaces at the split point
            remaining = "    " + remaining.substring(splitPos).trimStart();
        }

        isFirstLine = false;
    }

    return lines;
}

/**
 * Update embedded source format directives in a line to specify the target format.
 */
function updateSourceFormatDirective(line: string, targetFormat: ESourceFormat): string {
    const targetName = targetFormat === ESourceFormat.fixed ? "FIXED"
        : targetFormat === ESourceFormat.free ? "FREE"
            : "VARIABLE";

    // Try each directive pattern and replace the format specifier
    let updated = line.replace(
        />>SOURCE\s+FORMAT\s+(IS\s+)?(FREE|FIXED|VARIABLE)/i,
        `>>SOURCE FORMAT IS ${targetName}`
    );
    if (updated !== line) return updated;

    updated = line.replace(
        /\$SET\s+SOURCEFORMAT\s*\(\s*(FREE|FIXED|VARIABLE)\s*\)/i,
        `$SET SOURCEFORMAT(${targetName})`
    );
    if (updated !== line) return updated;

    updated = line.replace(
        /SOURCEFORMAT\s*\(\s*(FREE|FIXED|VARIABLE)\s*\)/i,
        `SOURCEFORMAT(${targetName})`
    );
    if (updated !== line) return updated;

    return line;
}

/**
 * Merge free-format continuation lines (joined with &) starting from the given index.
 * In free-format COBOL, `&` at the end of a line is ALWAYS a continuation marker
 * and must ALWAYS be stripped — regardless of whether it appears inside or outside
 * a string literal.  The string literal state only affects how the join is formed
 * (leading whitespace on the next line is preserved for open literals but trimmed
 * for non-literal context).
 *
 * Returns the merged content and the last consumed index (inclusive).
 */
function mergeFreeContinuations(parsedLines: COBOLParsedLine[], startIndex: number): { content: string; lastIndex: number } {
    let content = parsedLines[startIndex].content.trimEnd();

    // Always strip the trailing & — it is the continuation marker, never content
    if (content.endsWith("&")) {
        content = content.substring(0, content.length - 1);
    }

    let j = startIndex + 1;
    while (j < parsedLines.length) {
        const nextPl = parsedLines[j];
        if (nextPl.isBlank || nextPl.isComment) break;

        const nextContent = nextPl.content;
        const nextTrimmed = nextContent.trimEnd();
        const insideString = analyzeStringLiteralState(content).isOpen;

        let joinPart: string;
        if (insideString) {
            // COBOL continuation rule for string literals (same as fixed/variable format):
            // - Trim leading whitespace (it's just indentation)
            // - The first non-blank character should be a quote delimiter that must be
            //   consumed (it's a continuation marker, not part of the value)
            const trimmedCont = nextTrimmed.trimStart();
            if (trimmedCont.length > 0 && (trimmedCont[0] === '"' || trimmedCont[0] === "'")) {
                joinPart = trimmedCont.substring(1);
            } else {
                // No quote found — unusual, append as-is to avoid data loss
                joinPart = trimmedCont;
            }
        } else {
            // Non-literal continuation: trim leading whitespace
            joinPart = nextTrimmed.trimStart();
        }

        if (nextTrimmed.endsWith("&")) {
            // Another continuation — strip the & from the join part
            content += joinPart.endsWith("&") ? joinPart.substring(0, joinPart.length - 1) : joinPart;
            j++;
        } else {
            // Last line in the chain — append and stop
            content += joinPart;
            j++;
            break;
        }
    }
    return { content, lastIndex: j - 1 };
}

// ============================================================================
// Main conversion functions
// ============================================================================

/**
 * Convert from fixed format to free format.
 */
function convertFixedToFree(lines: string[]): string[] {
    const parsedLines = lines.map(l => parseFixedLine(l));
    const mergedLines = mergeContinuationLines(parsedLines);
    const result: string[] = [];

    for (const ml of mergedLines) {
        if (ml.isBlank) {
            result.push("");
            continue;
        }

        if (ml.isComment) {
            const parsed = parsedLines[ml.sourceLines[0]];
            result.push(fixedCommentToFree(parsed));
            continue;
        }

        if (ml.isSourceFormatDirective) {
            // Use the merged content (not the original line) in case the directive
            // spans continuation lines — using the raw original would lose data.
            result.push(updateSourceFormatDirective(ml.content.trimStart(), ESourceFormat.free));
            continue;
        }

        if (ml.isDebug) {
            // Debug lines: convert D indicator to >>D prefix in free format.
            // Preserve Area A/B indentation so that round-tripping retains
            // the original column placement (e.g. Area B items keep their
            // 4-space indent).
            const content = ml.content.trimEnd();
            result.push(">>D " + content);
            continue;
        }

        // Regular code line:
        // Remove the fixed-format structure, preserving indentation from Area A/B.
        // In fixed format, Area A is cols 8-11 (content chars 0-3) and Area B is cols 12-72 (content chars 4-64).
        // In free format, we preserve the relative indentation.
        const content = ml.content;
        const trimmedContent = content.trimEnd();

        if (trimmedContent.length === 0) {
            result.push("");
            continue;
        }

        // Preserve indentation — Area A items have 0-3 leading spaces in content,
        // Area B items have 4+ leading spaces in content.
        // In free format, we use the same relative indentation starting from column 1.
        result.push(trimmedContent);
    }

    return result;
}

/**
 * Convert from fixed format to variable format.
 */
function convertFixedToVariable(lines: string[]): string[] {
    const result: string[] = [];

    for (const line of lines) {
        const parsed = parseFixedLine(line);

        if (parsed.isBlank) {
            result.push("");
            continue;
        }

        if (parsed.isSourceFormatDirective && !parsed.isComment) {
            const content = line.length > 7 ? line.substring(7) : line;
            result.push(buildVariableLine(" ", updateSourceFormatDirective(content.trimStart(), ESourceFormat.variable)));
            continue;
        }

        // Variable format keeps sequence area and indicator but drops the identification area.
        // Content extends beyond column 72.
        const newContent = parsed.content;

        // Remove the identification area (it was already separated in parsing)
        result.push(buildVariableLine(parsed.indicator, newContent.trimEnd()));
    }

    return result;
}

/**
 * Convert from variable format to free format.
 */
function convertVariableToFree(lines: string[]): string[] {
    const parsedLines = lines.map(l => parseVariableLine(l));
    const mergedLines = mergeContinuationLines(parsedLines);
    const result: string[] = [];

    for (const ml of mergedLines) {
        if (ml.isBlank) {
            result.push("");
            continue;
        }

        if (ml.isComment) {
            const parsed = parsedLines[ml.sourceLines[0]];
            // Same conversion as fixed-to-free for comments
            result.push(fixedCommentToFree(parsed));
            continue;
        }

        if (ml.isSourceFormatDirective) {
            // Use the merged content (not the original line) to handle continuations correctly.
            result.push(updateSourceFormatDirective(ml.content.trimStart(), ESourceFormat.free));
            continue;
        }

        if (ml.isDebug) {
            // Preserve Area A/B indentation for round-trip fidelity
            const content = ml.content.trimEnd();
            result.push(">>D " + content);
            continue;
        }

        const trimmedContent = ml.content.trimEnd();
        if (trimmedContent.length === 0) {
            result.push("");
            continue;
        }

        result.push(trimmedContent);
    }

    return result;
}

/**
 * Convert from variable format to fixed format.
 */
function convertVariableToFixed(lines: string[]): string[] {
    const result: string[] = [];

    for (const line of lines) {
        const parsed = parseVariableLine(line);

        if (parsed.isBlank) {
            result.push("");
            continue;
        }

        if (parsed.isComment) {
            // Comment in variable format uses same indicator as fixed.
            // Split long comments across multiple lines instead of truncating.
            // Only the first line preserves the original indicator (e.g. / for
            // page eject); continuation lines always use *.
            const commentText = parsed.content.trimEnd();
            result.push(...splitTextForFixedLines(parsed.indicator, commentText));
            continue;
        }

        if (parsed.isSourceFormatDirective) {
            const content = line.length > 7 ? line.substring(7) : line;
            const updatedDirective = updateSourceFormatDirective(content.trimStart(), ESourceFormat.fixed);
            const directiveLines = splitForFixedFormat(" ", updatedDirective);
            result.push(...directiveLines);
            continue;
        }

        // For other lines, the content may exceed 72 columns and need continuation
        const contentLines = splitForFixedFormat(parsed.indicator, parsed.content.trimEnd());
        result.push(...contentLines);
    }

    return result;
}

/**
 * Find the index of the **FREE directive, skipping any leading blank lines.
 * Returns the index of the **FREE line, or -1 if not found.
 */
function findFreeDirectiveIndex(lines: string[]): number {
    for (let i = 0; i < lines.length; i++) {
        const trimmed = lines[i].trimStart();
        if (trimmed.length === 0) continue;  // skip blank lines
        if (trimmed.toUpperCase().startsWith("**FREE")) {
            return i;
        }
        break;  // first non-blank line is not **FREE
    }
    return -1;
}

/**
 * Convert from free format to fixed format.
 */
function convertFreeToFixed(lines: string[]): string[] {
    const parsedLines = lines.map(l => parseFreeLine(l));
    const result: string[] = [];

    // Skip **FREE directive if present (may be preceded by blank lines)
    const freeDirectiveIdx = findFreeDirectiveIndex(lines);
    let startIndex = 0;
    if (freeDirectiveIdx >= 0) {
        // Emit blank lines before the directive
        for (let b = 0; b < freeDirectiveIdx; b++) {
            result.push("");
        }
        startIndex = freeDirectiveIdx + 1;
    }

    for (let i = startIndex; i < parsedLines.length; i++) {
        const pl = parsedLines[i];

        if (pl.isBlank) {
            result.push("");
            continue;
        }

        if (pl.isComment) {
            result.push(...freeCommentToFixed(pl.content));
            continue;
        }

        if (pl.isSourceFormatDirective) {
            const updatedDirective = updateSourceFormatDirective(pl.content.trimStart(), ESourceFormat.fixed);
            const directiveLines = splitForFixedFormat(" ", updatedDirective);
            result.push(...directiveLines);
            continue;
        }

        if (pl.isDebug) {
            // >>D prefix -> D indicator in fixed format.
            // Strip only the >>D marker and one separator space, preserving
            // any remaining indentation so Area A/B placement round-trips.
            let debugContent = pl.content.trimStart();
            if (debugContent.toUpperCase().startsWith(">>D")) {
                debugContent = debugContent.substring(3);
                // Strip exactly one conventional separator space after >>D
                if (debugContent.startsWith(" ")) {
                    debugContent = debugContent.substring(1);
                }
            }
            const debugLines = splitForFixedFormat("D", debugContent);
            result.push(...debugLines);
            continue;
        }

        // Regular code line — merge any & continuations before emitting
        let content = pl.content;
        if (pl.isContinuation) {
            const merged = mergeFreeContinuations(parsedLines, i);
            content = merged.content;
            i = merged.lastIndex; // outer loop will i++
        }

        const trimmedContent = content.trimEnd();

        if (trimmedContent.length === 0) {
            result.push("");
            continue;
        }

        // In free format, the indentation is from column 1.
        // In fixed format, the content goes into Area A (cols 8-11) and Area B (cols 12-72).
        // We place the content starting at col 8, preserving relative indentation.
        const contentLines = splitForFixedFormat(" ", trimmedContent);
        result.push(...contentLines);
    }

    return result;
}

/**
 * Convert from free format to variable format.
 */
function convertFreeToVariable(lines: string[]): string[] {
    const parsedLines = lines.map(l => parseFreeLine(l));
    const result: string[] = [];

    // Skip **FREE directive if present (may be preceded by blank lines)
    const freeDirectiveIdx = findFreeDirectiveIndex(lines);
    let startIndex = 0;
    if (freeDirectiveIdx >= 0) {
        for (let b = 0; b < freeDirectiveIdx; b++) {
            result.push("");
        }
        startIndex = freeDirectiveIdx + 1;
    }

    for (let i = startIndex; i < parsedLines.length; i++) {
        const pl = parsedLines[i];

        if (pl.isBlank) {
            result.push("");
            continue;
        }

        if (pl.isComment) {
            result.push(freeCommentToVariable(pl.content));
            continue;
        }

        if (pl.isSourceFormatDirective) {
            const updatedDirective = updateSourceFormatDirective(pl.content.trimStart(), ESourceFormat.variable);
            result.push(buildVariableLine(" ", updatedDirective));
            continue;
        }

        if (pl.isDebug) {
            // Strip only the >>D marker and one separator space, preserving
            // any remaining indentation for Area A/B round-trip fidelity.
            let debugContent = pl.content.trimStart();
            if (debugContent.toUpperCase().startsWith(">>D")) {
                debugContent = debugContent.substring(3);
                if (debugContent.startsWith(" ")) {
                    debugContent = debugContent.substring(1);
                }
            }
            result.push(buildVariableLine("D", debugContent));
            continue;
        }

        // Regular code line — merge any & continuations before emitting
        let content = pl.content;
        if (pl.isContinuation) {
            const merged = mergeFreeContinuations(parsedLines, i);
            content = merged.content;
            i = merged.lastIndex; // outer loop will i++
        }

        const trimmedContent = content.trimEnd();
        if (trimmedContent.length === 0) {
            result.push("");
            continue;
        }

        // Place content after the 6-blank sequence area + space indicator
        result.push(buildVariableLine(" ", trimmedContent));
    }

    return result;
}

// ============================================================================
// Core conversion dispatch
// ============================================================================

/**
 * Check whether any lines in fixed format have non-trivial identification area
 * content (columns 73-80) that will be lost during conversion.
 */
function hasIdentificationAreaContent(lines: string[]): boolean {
    for (const line of lines) {
        if (line.length > FIXED_RIGHT_MARGIN) {
            const idArea = line.substring(FIXED_RIGHT_MARGIN).trim();
            if (idArea.length > 0) {
                return true;
            }
        }
    }
    return false;
}

/**
 * Perform the format conversion from one source format to another.
 * Returns the new file content as an array of lines, or undefined if the
 * conversion is not supported or unnecessary.
 */
function convertSource(lines: string[], fromFormat: ESourceFormat, toFormat: ESourceFormat): string[] | undefined {
    if (fromFormat === toFormat) {
        return undefined;
    }

    switch (fromFormat) {
        case ESourceFormat.fixed:
            switch (toFormat) {
                case ESourceFormat.free: return convertFixedToFree(lines);
                case ESourceFormat.variable: return convertFixedToVariable(lines);
            }
            break;
        case ESourceFormat.variable:
            switch (toFormat) {
                case ESourceFormat.free: return convertVariableToFree(lines);
                case ESourceFormat.fixed: return convertVariableToFixed(lines);
            }
            break;
        case ESourceFormat.free:
            switch (toFormat) {
                case ESourceFormat.fixed: return convertFreeToFixed(lines);
                case ESourceFormat.variable: return convertFreeToVariable(lines);
            }
            break;
    }

    return undefined;
}

// ============================================================================
// VS Code command entry point
// ============================================================================

/**
 * Execute the source format conversion command.
 * Detects the current format, prompts the user for the target format,
 * and applies the conversion to the active editor.
 */
export async function convertSourceFormat(): Promise<void> {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage("No active editor");
        return;
    }

    const document = editor.document;
    const settings = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);
    const langid = document.languageId;

    if (!VSExtensionUtils.isKnownCOBOLLanguageId(settings, langid)) {
        vscode.window.showErrorMessage("Active file is not a recognized COBOL source file");
        return;
    }

    // Detect the current source format
    const sourceHandlerLite = new VSCodeSourceHandlerLite(document);
    const currentFormat = SourceFormat.get(sourceHandlerLite, settings);

    if (currentFormat === ESourceFormat.unknown || currentFormat === ESourceFormat.jcl) {
        vscode.window.showErrorMessage(`Unable to determine the source format of this file (detected: ${currentFormat})`);
        return;
    }

    // Build the list of target formats (exclude current format from choices)
    const targetFormats: { label: string; format: ESourceFormat; description: string }[] = [];

    if (currentFormat !== ESourceFormat.fixed) {
        targetFormats.push({
            label: "Fixed",
            format: ESourceFormat.fixed,
            description: "Columns 1-6: sequence, Col 7: indicator, Cols 8-72: code, Cols 73-80: identification",
        });
    }

    if (currentFormat !== ESourceFormat.free) {
        targetFormats.push({
            label: "Free",
            format: ESourceFormat.free,
            description: "No column restrictions, *> for comments",
        });
    }

    if (currentFormat !== ESourceFormat.variable) {
        targetFormats.push({
            label: "Variable",
            format: ESourceFormat.variable,
            description: "Like fixed but no right margin (col 73+) restriction",
        });
    }

    const currentLabel = formatLabel(currentFormat);

    const pick = await vscode.window.showQuickPick(targetFormats, {
        placeHolder: `Current format: ${currentLabel} — select target format`,
        title: "Convert COBOL Source Format",
    });

    if (!pick) {
        return; // User cancelled
    }

    const targetFormat = pick.format;

    // Read all lines from the document
    const lines: string[] = [];
    for (let i = 0; i < document.lineCount; i++) {
        lines.push(document.lineAt(i).text);
    }

    // Warn if converting from fixed format and identification area content will be lost
    if (currentFormat === ESourceFormat.fixed && targetFormat !== ESourceFormat.fixed) {
        if (hasIdentificationAreaContent(lines)) {
            const proceed = await vscode.window.showWarningMessage(
                "This file has content in the identification area (columns 73-80) which will be discarded during conversion. Continue?",
                { modal: true },
                "Continue"
            );
            if (proceed !== "Continue") {
                return;
            }
        }
    }

    // Perform the conversion
    const convertedLines = convertSource(lines, currentFormat, targetFormat);
    if (!convertedLines) {
        vscode.window.showInformationMessage("No conversion needed — source is already in the target format");
        return;
    }

    // Apply the conversion as a single edit replacing the entire document
    const fullRange = new vscode.Range(
        new vscode.Position(0, 0),
        new vscode.Position(document.lineCount - 1, document.lineAt(document.lineCount - 1).text.length)
    );

    // Detect the document's line ending style
    const eol = document.eol === vscode.EndOfLine.CRLF ? "\r\n" : "\n";

    const newContent = convertedLines.join(eol);

    const edit = new vscode.WorkspaceEdit();
    edit.replace(document.uri, fullRange, newContent);
    const success = await vscode.workspace.applyEdit(edit);

    if (success) {
        const fromLabel = formatLabel(currentFormat);
        const toLabel = formatLabel(targetFormat);
        VSLogger.logMessage(`Source format converted from ${fromLabel} to ${toLabel}`);
        vscode.window.showInformationMessage(
            `COBOL source format converted: ${fromLabel} → ${toLabel}`
        );
    } else {
        vscode.window.showErrorMessage("Failed to apply source format conversion");
    }
}
