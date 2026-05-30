
export class SplitTokenizer {

    private static readonly wordSeperator = "~!@$%^&*()=+[{]}\\|;,<>/?";

    // Lookup tables: 1 = plain word char (continue), 0 = stop char (handled by an earlier branch).
    // Chars >= 128 default to "word char" (Unicode-safe).
    private static readonly wordCharTable: Uint8Array = SplitTokenizer.buildWordCharTable();
    // 1 = char appears in `wordSeperator`. Used to replace `wordSeperator.indexOf(c)`.
    private static readonly wordSepTable: Uint8Array = SplitTokenizer.buildWordSepTable();

    private static buildWordCharTable(): Uint8Array {
        const t = new Uint8Array(128);
        for (let i = 0; i < 128; i++) t[i] = 1;
        // Any char handled by a branch BEFORE the fallthrough word-accumulation must stop the fast path.
        const stops = SplitTokenizer.wordSeperator + "'\":= \t";
        for (let k = 0; k < stops.length; k++) {
            t[stops.charCodeAt(k)] = 0;
        }
        return t;
    }

    private static buildWordSepTable(): Uint8Array {
        const t = new Uint8Array(128);
        const s = SplitTokenizer.wordSeperator;
        for (let k = 0; k < s.length; k++) {
            t[s.charCodeAt(k)] = 1;
        }
        return t;
    }

    public static splitArgument(input: string, ret: string[]): void {
        let inQuote = false;
        let inQuoteSingle = false;
        let inDoubleEquals = false;
        const lineLength = input.length;
        let currentArgument = "";
        const wordCharTable = SplitTokenizer.wordCharTable;
        const wordSepTable = SplitTokenizer.wordSepTable;

        for (let i = 0; i < lineLength; i++) {
            // Fast path: contiguous run of plain word chars when no state is active and no partial argument.
            if (currentArgument.length === 0 && !inQuote && !inQuoteSingle && !inDoubleEquals) {
                const code0 = input.charCodeAt(i);
                if (code0 >= 128 || wordCharTable[code0] === 1) {
                    const wordStart = i;
                    i++;
                    while (i < lineLength) {
                        const code = input.charCodeAt(i);
                        if (code < 128 && wordCharTable[code] === 0) break;
                        i++;
                    }
                    currentArgument = input.substring(wordStart, i);
                    i--; // for-loop's i++ will land on the stop char
                    continue;
                }
            }

            let c = input.charAt(i);

            /* handle quotes */
            if (c === "'" && !inQuote) {
                inQuoteSingle = !inQuoteSingle;
                currentArgument += c;
                if (inQuoteSingle === false) {
                    ret.push(currentArgument);
                    currentArgument = "";
                }
                continue;
            }

            if (c === "\"" && !inQuoteSingle) {
                inQuote = !inQuote;
                currentArgument += c;
                if (inQuote === false) {
                    ret.push(currentArgument);
                    currentArgument = "";
                }
                continue;
            }

            if (c == '=') {
                // double ==
                if (1 + i < lineLength && input.charAt(1 + i) === "=") {
                    currentArgument += "==";
                    inDoubleEquals = !inDoubleEquals;
                    i++;
                    continue;
                }
                if (currentArgument.length !== 0) {
                    ret.push(currentArgument);
                    currentArgument = "";
                }
                // single =
                ret.push(c);
                currentArgument = "";
                continue;             
            }

            // include all items in string
            if (inQuote || inQuoteSingle || inDoubleEquals) {
                currentArgument += c;
                continue;
            }

            /* skip white space */
            if ((c === " ") || (c === "\t")) {
                if (currentArgument.length !== 0) {
                    ret.push(currentArgument);
                    currentArgument = "";
                }
                while ((c === " ") || (c === "\t")) {
                    i++;
                    c = input.charAt(i);
                }
                i--;
                continue;
            }


            // handle : or ::
            if (c === ":") {
                if (currentArgument.length !== 0) {
                    ret.push(currentArgument);
                    currentArgument = "";
                }

                // double ::
                if (1 + i < lineLength && input.charAt(1 + i) === ":") {
                    ret.push("::");
                    i++;
                    continue;
                }

                // single :
                ret.push(c);
                currentArgument = "";
                continue;
            }

            const code = input.charCodeAt(i);
            if (code < 128 && wordSepTable[code] === 1) {
                if (currentArgument.length !== 0) {
                    ret.push(currentArgument);
                }
                ret.push(c);
                currentArgument = "";
                continue;
            }

            currentArgument += c;
        }

        if (currentArgument.length !== 0) {
            ret.push(currentArgument);
        }
    }
}