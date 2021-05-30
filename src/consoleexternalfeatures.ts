import util from 'util';
import path from 'path';
import fs from 'fs';

/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { ESourceFormat, IExternalFeatures } from "./externalfeatures";
import ISourceHandler from "./isourcehandler";
import { ICOBOLSettings } from "./iconfiguration";

const inline_sourceformat: string[] = ['sourceformat', '>>source format'];

function isNumber(value: string | number): boolean {
    if (value.toString().length === 0) {
        return false;
    }
    return !isNaN(Number(value.toString()));
}

function isValidFixedLine(line: string): boolean {
    if (line.length > 7) {
        switch (line[6]) {
            case '*': return true;
            case 'D': return true;
            case '/': return true;
            case ' ': return true;
            case '-': return true;
        }
    }

    return false;
}


export function getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings, checkForTerminalFormat: boolean): ESourceFormat {

    if (config.fileformat_strategy === "always_fixed") {
        return ESourceFormat.fixed;
    }

    let linesWithJustNumbers = 0;
    let linesWithIdenticalAreaB = 0;
    const maxLines = doc.getLineCount() > 10 ? 10 : doc.getLineCount();
    let defFormat = ESourceFormat.unknown;

    let prevRightMargin = "";
    let validFixedLines = 0;
    for (let i = 0; i < maxLines; i++) {

        const lineText = doc.getLine(i, true);
        if (lineText === undefined) {
            break;
        }

        const line = lineText.toLowerCase();
        const validFixedLine = isValidFixedLine(line);
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
                continue;
            } else {
                if (isValidFixedLine(line)) {
                    if (line.length > 72) {
                        const rightMargin = line.substr(72).trim();

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
                    defFormat = ESourceFormat.variable;
                }
            }
            continue;
        } else {
            // got a inline comment,yes
            const line2right = line.substr(pos4sourceformat_after);

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

    if (validFixedLines == maxLines) {
        return ESourceFormat.fixed;
    }

    //it might well be...
    if (linesWithJustNumbers > 7 || linesWithIdenticalAreaB > 7) {
        return ESourceFormat.fixed;
    }

    return defFormat;
}

export class ConsoleExternalFeatures implements IExternalFeatures {
    public static readonly Default = new ConsoleExternalFeatures();

    public workspaceFolders: string[] = [];

    private static isFile(sdir: string): boolean {
        try {
            if (fs.existsSync(sdir)) {
                return true;
            }
        }
        catch {
            return false;
        }
        return false;
    }

    public logMessage(message: string): void {
        if (process.send) {
            process.send(message);
        } else {
            console.log(message);
        }

        return;
    }

    public logException(message: string, ex: Error): void {
        this.logMessage(ex.name + ": " + message);
        if (ex !== undefined && ex.stack !== undefined) {
            this.logMessage(ex.stack);
        } 
        return;
    }

    readonly logTimeThreshold = 500;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        const fixedTimeTaken = " (" + timeTaken.toFixed(2) + "ms)";

        if (timeTaken < this.logTimeThreshold) {
            return false;
        }

        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            const m: string = util.format(message, parameters);
            this.logMessage(m.padEnd(60) + fixedTimeTaken);
        } else {
            this.logMessage(message.padEnd(60) + fixedTimeTaken);
        }

        return true;

    }

    public performance_now(): number {
        return Date.now();
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        return "";
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat {
        return getCOBOLSourceFormat(doc,config,false);
    }

    public setWorkspaceFolders(folders: string[]) {
        this.workspaceFolders = folders;
    }

    public getWorkspaceFolders(): string[] {
        return this.workspaceFolders;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        if (this.workspaceFolders.length === 0) {
            this.logMessage("getFullWorkspaceFilename: workspaceFolders.length === 0");
        }
        for (const folder of this.workspaceFolders) {
            const possibleFile = path.join(folder, sdir);
            if (ConsoleExternalFeatures.isFile(possibleFile)) {
                const stat4src = fs.statSync(possibleFile, { bigint: true });
                if (sdirMs === stat4src.mtimeMs) {
                    return possibleFile;
                } 
                this.logMessage(`getFullWorkspaceFilename: found ${possibleFile} ${sdirMs} !== ${stat4src.mtimeMs}`);
            }
        }

        return undefined;
    }
}
