import * as vscode from "vscode";
import * as cp from "child_process";
import { ExtensionDefaults } from "./extensionDefaults";
import { VSExternalFeatures } from "./vsexternalfeatures";
import path from "path";

/**
 * Execute async shell command.
 *
 * @param cmd the command the execute.
 * @param timeout optional timeout in ms for the promise to resolve: defaults to 1000ms.
 * @returns A promise<string> with stdout if any, or stderr if error.
 */
export function runAsync(cmd: string, timeout = 1000): Promise<string> {
    return new Promise((resolve, reject) => {
        let timer: NodeJS.Timeout;
        let wasKilled = false;

        const childProcess = cp.exec(cmd, (err, stdout, stderr) => {
            clearTimeout(timer);

            if (wasKilled) {
                return reject(new Error("killed"));
            }

            if (stdout) {
                return resolve(stdout);
            }

            if (stderr) {
                return reject(stderr);
            }

            if (err) {
                return reject(err.message);
            }
        });

        timer = setTimeout(() => {
            wasKilled = true;
            childProcess.kill();
            reject("timeout");
        }, timeout);
    });
}

/**
 * Execute sync shell command.
 *
 * If command fails, will show a popup error message.
 *
 * @param cmd the command the execute.
 * @param suppressError if true, will not show error message. Defaults to false.
 * @returns A string with stdout if any, or null if error.
 */
export function runSync(cmd: string, suppressError = false, processEncoding: "utf-8"): string | null {
    let result = "";
    try {
        result = cp.execSync(cmd, { encoding: processEncoding });
    } catch (error) {
        if (suppressError) {
            return null;
        }
        let msg = "";

        if (error instanceof Error) {
            msg = error.message;
        } else {
            msg = `Some unknown error has occurred when running: ${cmd}`;
        }
        vscode.window.showErrorMessage(msg);
        return null;
    }
    return result;
}

function getDOTFILE_RC(currentContext: vscode.ExtensionContext): string {
    const thisExtension = vscode.extensions.getExtension(ExtensionDefaults.thisExtensionName);
    if (thisExtension !== undefined) {
        const extPath = `${thisExtension.extensionPath}`;
        const rcFile = path.join(extPath, "dotfiles","scan4install.rc");
        if (VSExternalFeatures.isFile(rcFile)) {
            return rcFile;
        }
    }

    return "";
}

export class VSTerminal implements vscode.TerminalProfileProvider {
    
    private context:vscode.ExtensionContext;

    constructor(context:vscode.ExtensionContext) {
        this.context = context;
    }

    provideTerminalProfile(token: vscode.CancellationToken): vscode.ProviderResult<vscode.TerminalProfile> {
        const rcFile = getDOTFILE_RC(this.context);
            let args: string[] = [
                    "--rcfile",
                    rcFile
            ];
            return {
                options:
                {
                    name: 'COBOL Terminal',
                    shellPath: "/bin/bash",
                    cwd: "/tmp",
                    shellArgs: args
                }
            };
    }
}