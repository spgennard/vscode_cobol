"user strict";
import { window } from "vscode";

const outputChannel = window.createOutputChannel("COBOL");

export class OutputChannel {
    static appendLineError(value: any): any {
        outputChannel.show(true);
        outputChannel.appendLine(value);
    }
    public static appendLine(value: string) {
        outputChannel.show(true);
        outputChannel.appendLine(value);
    }

}
