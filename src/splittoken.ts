
export class SplitTokenizer {

    private static wordSeperator = "~!@$%^&*()=+[{]}\\|;,<>/?";

    public static splitArgument(input: string, ret: string[]): void {
        let inQuote = false;
        let inQuoteSingle = false;
        const lineLength = input.length;
        let currentArgument = "";

        for (let i = 0; i < lineLength; i++) {
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

            // include all items in string
            if (inQuote || inQuoteSingle) {
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
                ret.push("" + c);
                currentArgument = "";
                continue;
            }

            const ind = SplitTokenizer.wordSeperator.indexOf(c);
            if (ind !== -1) {
                if (currentArgument.length !== 0) {
                    ret.push(currentArgument);
                }
                ret.push("" + c);
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