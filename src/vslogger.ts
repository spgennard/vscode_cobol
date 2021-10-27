import { OutputChannel, window } from "vscode";
import util from "util";

export const COBOLOutputChannel: OutputChannel = window.createOutputChannel("COBOL");

export class VSLogger {
    public static readonly logTimeThreshold = 500;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        const fixedTimeTaken = " (" + timeTaken.toFixed(2) + "ms)";

        if (timeTaken < VSLogger.logTimeThreshold) {
            return false;
        }

        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            const m: string = util.format(message, parameters);
            COBOLOutputChannel.appendLine(m.padEnd(60) + fixedTimeTaken);
        } else {
            COBOLOutputChannel.appendLine(message.padEnd(60) + fixedTimeTaken);
        }

        return true;
    }

    public static logChannelSetPreserveFocus(preserveFocus: boolean): void {
        COBOLOutputChannel.show(preserveFocus);
    }


    public static logChannelHide(): void {
        COBOLOutputChannel.hide();
    }


    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logWarningMessage(message: string, ...parameters: any[]): void {
        const trimmedLeftCount = message.length - message.trimLeft().length;
        const spacesToLeft = " ".repeat(trimmedLeftCount);

        // TODO: Could this be colorized?
        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            COBOLOutputChannel.appendLine(`${spacesToLeft}WARNING: ${util.format(message, parameters)}`);
        } else {
            COBOLOutputChannel.appendLine(`${spacesToLeft}WARNING: ${message}`);
        }
    }


    public static logException(message: string, ex: Error): void {
        VSLogger.logMessage(ex.name + ": " + message);
        if (ex !== undefined && ex.stack !== undefined) {
            VSLogger.logMessage(ex.stack);
        }
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static logMessage(message: string, ...parameters: any[]): void {
        if ((parameters !== undefined || parameters !== null) && parameters.length !== 0) {
            COBOLOutputChannel.appendLine(util.format(message, parameters));
        } else {
            COBOLOutputChannel.appendLine(message);
        }
    }
}






