import { ESourceFormat } from "./externalfeatures";
import { ICOBOLSettings, IEditorMarginFiles } from "./iconfiguration";
import { ISourceHandlerLite } from "./isourcehandler";
import minimatch from "minimatch";


function isNumber(value: string | number): boolean {
    if (value.toString().length === 0) {
        return false;
    }
    return !isNaN(Number(value.toString()));
}


const inline_sourceformat: string[] = ["sourceformat", ">>source format"];

export class VSSourceFormat {

    private static isValidFixedLine(line: string): boolean {
        if (line.length >= 7) {
            switch (line[6]) {
                case "*": return true;
                case "D": return true;
                case "/": return true;
                case " ": return true;
                case "-": return true;
            }
        }
    
        return false;
    }

    public static get(doc: ISourceHandlerLite, config: ICOBOLSettings): ESourceFormat {
        const langid = doc.getLanguageId();

        if (config.fileformat_strategy === "always_fixed") {
            return ESourceFormat.fixed;
        }
        
        let linesWithJustNumbers = 0;
        let linesWithIdenticalAreaB = 0;
        const maxLines = doc.getLineCount() > config.pre_scan_line_limit ? config.pre_scan_line_limit : doc.getLineCount();
        let defFormat = ESourceFormat.unknown;

        const checkForTerminalFormat: boolean = langid.toLocaleLowerCase() === "acucobol" ? true : false;
        let prevRightMargin = "";
        let validFixedLines = 0;
        let skippedLines = 0;
        let linesGT80 = 0;

        for (let i = 0; i < maxLines; i++) {

            let lineText = doc.getLineTabExpanded(i);
            if (lineText === undefined) {
                break;
            }

            lineText = lineText.trimEnd();
            if (lineText.length === 0) {
                skippedLines++;
                continue;
            }

            const line = lineText.toLowerCase();
            const validFixedLine = VSSourceFormat.isValidFixedLine(line);
            if (validFixedLine) {
                validFixedLines++;
            }

            // acu
            if (defFormat === ESourceFormat.unknown && checkForTerminalFormat) {
                if (line.startsWith("*") || line.startsWith("|") || line.startsWith("\\D")) {
                    defFormat = ESourceFormat.terminal;
                }
            }

            // non-acu
            if (defFormat === ESourceFormat.unknown && !checkForTerminalFormat) {
                const newcommentPos = line.indexOf("*>");
                if (newcommentPos !== -1 && defFormat === ESourceFormat.unknown) {
                    defFormat = ESourceFormat.variable;
                }
            }

            let pos4sourceformat_after = 0;
            for (let isf = 0; isf < inline_sourceformat.length; isf++) {
                const pos4sourceformat = line.indexOf(inline_sourceformat[isf]);
                if (pos4sourceformat !== -1) {
                    pos4sourceformat_after = pos4sourceformat + inline_sourceformat[isf].length + 1;
                    break;
                }
            }

            // does it contain a inline comments? no
            if (pos4sourceformat_after === 0) {
                if (line.length > 80) {
                    defFormat = ESourceFormat.variable;
                    linesGT80++;
                    continue;
                } else {
                    if (VSSourceFormat.isValidFixedLine(line)) {
                        if (line.length > 72) {
                            const rightMargin = line.substring(72).trim();

                            if (prevRightMargin === rightMargin) {
                                linesWithIdenticalAreaB++;
                            } else {
                                if (isNumber(rightMargin)) {
                                    linesWithJustNumbers++;
                                }
                            }

                            prevRightMargin = rightMargin;
                        }
                    } else {
                        // if we cannot be sure, then let the default be variable
                        if (defFormat === ESourceFormat.unknown) {
                            defFormat = ESourceFormat.variable;
                        }
                    }
                }
                continue;
            } else {
                // got a inline comment,yes
                const line2right = line.substring(pos4sourceformat_after);

                if (line2right.indexOf("fixed") !== -1) {
                    return ESourceFormat.fixed;
                }
                if (line2right.indexOf("variable") !== -1) {
                    return ESourceFormat.variable;
                }
                if (line2right.indexOf("free") !== -1) {
                    return ESourceFormat.free;
                }
            }
        }

        if (linesGT80 === 0 && (validFixedLines + skippedLines === maxLines)) {
            return ESourceFormat.fixed;
        }

        //it might well be...
        if (linesWithJustNumbers > 7 || linesWithIdenticalAreaB > 7) {
            return ESourceFormat.fixed;
        }

        const filesFilter = config.editor_margin_files;
        if (filesFilter.length >= 1) {
            const docFilename: string = doc.getFilename();
            for (let i = 0; i < filesFilter.length; i++) {
                const filter: IEditorMarginFiles = filesFilter[i];

                if (minimatch(docFilename, filter.pattern, { nocase: true })) {
                    return ESourceFormat[filter.sourceformat];
                }
            }
        }

        return defFormat;
    }

}