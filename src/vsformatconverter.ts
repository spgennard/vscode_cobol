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
 * Parse a single line from Fixed format COBOL source.
 */
function parseFixedLine(line: string): COBOLParsedLine {
    const result: COBOLParsedLine = {
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

    if (line.trimEnd().length === 0) {
        result.isBlank = true;
        return result;
    }

    // Sequence number area: columns 1-6 (index 0-5)
    if (line.length >= 6) {
        result.sequenceArea = line.substring(0, 6);
    } else {
        // Short line — treat the whole thing as sequence area + blank
        result.sequenceArea = line.padEnd(6);
        result.isBlank = true;
        return result;
    }

    // Indicator area: column 7 (index 6)
    if (line.length >= 7) {
        result.indicator = line[6];
    }

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

    // Content area: columns 8-72 (index 7-71)
    if (line.length > 7) {
        const endCol = Math.min(line.length, 72);
        result.content = line.substring(7, endCol);
    }

    // Identification area: columns 73+ (index 72+)
    if (line.length > 72) {
        result.identificationArea = line.substring(72);
    }

    result.isSourceFormatDirective = isSourceFormatDirectiveLine(line);

    return result;
}

/**
 * Parse a single line from Variable format COBOL source.
 * Variable format is like fixed but without the 72-column right margin restriction,
 * meaning columns 1-6 are sequence area, column 7 is indicator, and content extends
 * beyond column 72 with no identification area.
 */
function parseVariableLine(line: string): COBOLParsedLine {
    const result: COBOLParsedLine = {
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

    if (line.trimEnd().length === 0) {
        result.isBlank = true;
        return result;
    }

    if (line.length >= 6) {
        result.sequenceArea = line.substring(0, 6);
    } else {
        result.sequenceArea = line.padEnd(6);
        result.isBlank = true;
        return result;
    }

    if (line.length >= 7) {
        result.indicator = line[6];
    }

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

    // Variable format: content extends to end of line (no identification area)
    if (line.length > 7) {
        result.content = line.substring(7);
    }

    result.isSourceFormatDirective = isSourceFormatDirectiveLine(line);

    return result;
}

/**
 * Parse a single line from Free format COBOL source.
 */
function parseFreeLine(line: string): COBOLParsedLine {
    const result: COBOLParsedLine = {
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

    // Free format has no continuation character — continuation is implicit
    // via incomplete statements (no period at end). We track literal continuations
    // by checking for the & character at the end of a line.
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
            if (isInsideStringLiteral(contentStr)) {
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
 * Determine if the given text ends inside an unclosed string literal.
 * Tracks double and single quotes independently, accounting for COBOL
 * doubled-quote escaping (e.g., "" inside a double-quoted string).
 */
function isInsideStringLiteral(text: string): boolean {
    let inDouble = false;
    let inSingle = false;
    for (let i = 0; i < text.length; i++) {
        const ch = text[i];
        if (inDouble) {
            if (ch === '"') {
                // Check for doubled quote (escape)
                if (i + 1 < text.length && text[i + 1] === '"') {
                    i++; // skip the escape
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
    return inDouble || inSingle;
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
        // Page eject (/) — preserve as a free format comment
        return "*> PAGE EJECT" + (commentText.trim().length > 0 ? " " + commentText.trim() : "");
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
 * Convert a comment from free format to fixed format.
 * Returns an array of lines because long comments must be split to fit
 * within the 72-column fixed format limit.
 */
function freeCommentToFixed(line: string): string[] {
    const trimmed = line.trimStart();
    let commentBody = "";

    if (trimmed.startsWith("*>")) {
        commentBody = trimmed.substring(2).trim();
    } else {
        commentBody = trimmed;
    }

    // Maximum comment text width: cols 8-72 = 65 chars, minus 1 for the space after * = 64
    const maxCommentWidth = 64;

    if (commentBody.length === 0) {
        return ["      *"];
    }

    if (commentBody.length <= maxCommentWidth) {
        return ["      * " + commentBody];
    }

    // Split long comment across multiple fixed-format comment lines
    const commentLines: string[] = [];
    let remaining = commentBody;
    while (remaining.length > 0) {
        if (remaining.length <= maxCommentWidth) {
            commentLines.push("      * " + remaining);
            break;
        }

        // Find a word boundary to split at
        let splitPos = maxCommentWidth;
        for (let s = maxCommentWidth; s >= Math.max(0, maxCommentWidth - 20); s--) {
            if (remaining[s] === " ") {
                splitPos = s;
                break;
            }
        }

        commentLines.push("      * " + remaining.substring(0, splitPos));
        remaining = remaining.substring(splitPos).trimStart();
    }

    return commentLines;
}

/**
 * Convert a comment from free format to variable format.
 * Variable format has no 72-column limit so no splitting is needed.
 */
function freeCommentToVariable(line: string): string {
    const trimmed = line.trimStart();
    let commentBody = "";

    if (trimmed.startsWith("*>")) {
        commentBody = trimmed.substring(2).trim();
    } else {
        commentBody = trimmed;
    }

    if (commentBody.length === 0) {
        return "      *";
    }

    return "      * " + commentBody;
}

/**
 * Build a fixed-format line from components.
 * Callers must ensure content fits within 65 characters (cols 8-72).
 * Use splitForFixedFormat() for content that may exceed this limit.
 */
function buildFixedLine(indicator: string, content: string): string {
    // Cols 1-6 are blank (no sequence numbers), col 7 is indicator, cols 8+ are content
    return "      " + indicator + content;
}

/**
 * Build a variable-format line from components.
 */
function buildVariableLine(indicator: string, content: string): string {
    // Cols 1-6 are blank, col 7 is indicator, rest is content (no right margin limit)
    return "      " + indicator + content;
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
    const maxContentWidth = 65; // columns 8-72 = 65 characters
    const lines: string[] = [];

    // If the content fits, just return it
    const fullLine = "      " + indicator + content;
    if (fullLine.length <= 72) {
        lines.push(fullLine);
        return lines;
    }

    // We need to split the line.
    // Strategy: split at word boundaries when possible
    let remaining = content;
    let isFirstLine = true;

    while (remaining.length > 0) {
        const ind = isFirstLine ? indicator : "-";
        const availWidth = isFirstLine ? maxContentWidth : maxContentWidth;
        
        if (remaining.length <= availWidth) {
            lines.push(buildFixedLine(ind, remaining));
            break;
        }

        // Find a good split point — look for a space near the end of the available width
        let splitPos = availWidth;
        const searchStart = Math.max(0, availWidth - 20);

        // Look for a space to split at
        let bestSplit = -1;
        for (let s = availWidth; s >= searchStart; s--) {
            if (remaining[s] === " ") {
                bestSplit = s;
                break;
            }
        }

        if (bestSplit > 0) {
            splitPos = bestSplit;
        }

        // Check if we're in the middle of a string literal
        const quoteCount = countUnescapedQuotes(remaining.substring(0, splitPos));
        if (quoteCount % 2 !== 0) {
            // We're inside a string literal — we must split the string properly.
            // In fixed format continuation of string literals:
            // - The current line is truncated at col 72
            // - The continuation line has '-' in col 7 and the string continues
            //   starting with a quote character in Area B
            const chunk = remaining.substring(0, splitPos);
            lines.push(buildFixedLine(ind, chunk));
            // For string continuation, the next line starts with the remaining string
            // The continuation indicator is '-' and the literal continues from col 12
            remaining = "    " + remaining.substring(splitPos);
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

function countUnescapedQuotes(text: string): number {
    let count = 0;
    for (let i = 0; i < text.length; i++) {
        if (text[i] === '"' || text[i] === "'") {
            count++;
        }
    }
    return count;
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

        if (ml.isSourceFormatDirective) {
            // Update the directive to say FREE
            const origLine = lines[ml.sourceLines[0]];
            const content = origLine.length > 7 ? origLine.substring(7) : origLine;
            result.push(updateSourceFormatDirective(content.trimStart(), ESourceFormat.free));
            continue;
        }

        if (ml.isComment) {
            const parsed = parsedLines[ml.sourceLines[0]];
            result.push(fixedCommentToFree(parsed));
            continue;
        }

        if (ml.isDebug) {
            // Debug lines: convert D indicator to >>D prefix in free format
            const content = ml.content.trimEnd();
            result.push(">>D " + content.trimStart());
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

        if (parsed.isSourceFormatDirective) {
            const content = line.length > 7 ? line.substring(7) : line;
            result.push(buildVariableLine(" ", updateSourceFormatDirective(content.trimStart(), ESourceFormat.variable).trimStart()));
            continue;
        }

        // Variable format keeps sequence area and indicator but drops the identification area.
        // Content extends beyond column 72.
        let newContent = parsed.content;

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

        if (ml.isSourceFormatDirective) {
            const origLine = lines[ml.sourceLines[0]];
            const content = origLine.length > 7 ? origLine.substring(7) : origLine;
            result.push(updateSourceFormatDirective(content.trimStart(), ESourceFormat.free));
            continue;
        }

        if (ml.isComment) {
            const parsed = parsedLines[ml.sourceLines[0]];
            // Same conversion as fixed-to-free for comments
            result.push(fixedCommentToFree(parsed));
            continue;
        }

        if (ml.isDebug) {
            const content = ml.content.trimEnd();
            result.push(">>D " + content.trimStart());
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

        if (parsed.isSourceFormatDirective) {
            const content = line.length > 7 ? line.substring(7) : line;
            const updatedDirective = updateSourceFormatDirective(content.trimStart(), ESourceFormat.fixed);
            const directiveLines = splitForFixedFormat(" ", updatedDirective);
            result.push(...directiveLines);
            continue;
        }

        if (parsed.isComment) {
            // Comment in variable format uses same indicator as fixed
            // Split long comments across multiple lines instead of truncating
            const commentText = parsed.content.trimEnd();
            const maxCommentWidth = 65; // columns 8-72

            if (commentText.length <= maxCommentWidth) {
                result.push("      " + parsed.indicator + commentText);
            } else {
                let remaining = commentText;
                while (remaining.length > 0) {
                    if (remaining.length <= maxCommentWidth) {
                        result.push("      " + parsed.indicator + remaining);
                        break;
                    }

                    let splitPos = maxCommentWidth;
                    for (let s = maxCommentWidth; s >= Math.max(0, maxCommentWidth - 20); s--) {
                        if (remaining[s] === " ") {
                            splitPos = s;
                            break;
                        }
                    }

                    result.push("      " + parsed.indicator + remaining.substring(0, splitPos));
                    remaining = remaining.substring(splitPos).trimStart();
                }
            }
            continue;
        }

        // For other lines, the content may exceed 72 columns and need continuation
        const contentLines = splitForFixedFormat(parsed.indicator, parsed.content.trimEnd());
        result.push(...contentLines);
    }

    return result;
}

/**
 * Convert from free format to fixed format.
 */
function convertFreeToFixed(lines: string[]): string[] {
    const parsedLines = lines.map(l => parseFreeLine(l));
    const result: string[] = [];

    // Skip **FREE directive if present on first line
    let startIndex = 0;
    if (lines.length > 0 && lines[0].trimStart().toUpperCase().startsWith("**FREE")) {
        startIndex = 1;
    }

    for (let i = startIndex; i < parsedLines.length; i++) {
        const pl = parsedLines[i];

        if (pl.isBlank) {
            result.push("");
            continue;
        }

        if (pl.isSourceFormatDirective) {
            const updatedDirective = updateSourceFormatDirective(pl.content.trimStart(), ESourceFormat.fixed);
            const directiveLines = splitForFixedFormat(" ", updatedDirective);
            result.push(...directiveLines);
            continue;
        }

        if (pl.isComment) {
            result.push(...freeCommentToFixed(pl.content));
            continue;
        }

        if (pl.isDebug) {
            // >>D prefix -> D indicator in fixed format
            let debugContent = pl.content.trimStart();
            if (debugContent.toUpperCase().startsWith(">>D")) {
                debugContent = debugContent.substring(3).trimStart();
            }
            const debugLines = splitForFixedFormat("D", debugContent);
            result.push(...debugLines);
            continue;
        }

        // Regular code line
        const content = pl.content;
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

    // Skip **FREE directive if present on first line
    let startIndex = 0;
    if (lines.length > 0 && lines[0].trimStart().toUpperCase().startsWith("**FREE")) {
        startIndex = 1;
    }

    for (let i = startIndex; i < parsedLines.length; i++) {
        const pl = parsedLines[i];

        if (pl.isBlank) {
            result.push("");
            continue;
        }

        if (pl.isSourceFormatDirective) {
            const updatedDirective = updateSourceFormatDirective(pl.content.trimStart(), ESourceFormat.variable);
            result.push(buildVariableLine(" ", updatedDirective));
            continue;
        }

        if (pl.isComment) {
            result.push(freeCommentToVariable(pl.content));
            continue;
        }

        if (pl.isDebug) {
            let debugContent = pl.content.trimStart();
            if (debugContent.toUpperCase().startsWith(">>D")) {
                debugContent = debugContent.substring(3).trimStart();
            }
            result.push(buildVariableLine("D", debugContent));
            continue;
        }

        const trimmedContent = pl.content.trimEnd();
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
