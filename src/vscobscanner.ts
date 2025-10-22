/* eslint-disable @typescript-eslint/naming-convention */
import path from "path";
import { extensions, Uri, workspace, WorkspaceFolder } from "vscode";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { COBSCANNER_ADDFILE, COBSCANNER_END_OF_FILE, COBSCANNER_KNOWNCOPYBOOK, COBSCANNER_SENDCLASS, COBSCANNER_SENDENUM, COBSCANNER_SENDEP, COBSCANNER_SENDINTERFACE, COBSCANNER_SENDPRGID, COBSCANNER_START_OF_FILE, COBSCANNER_STATUS, ScanData, ScanDataHelper } from "./cobscannerdata";
import { progressStatusBarItem } from "./extension";
import { VSLogger } from "./vslogger";
import { ICOBOLSettings } from "./iconfiguration";
import { fork, ForkOptions } from "child_process";
import { COBOLWorkspaceSymbolCacheHelper, TypeCategory } from "./cobolworkspacecache";
import { VSCOBOLUtils } from "./vscobolutils";

import { InMemoryGlobalCacheHelper, InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { COBOLWorkspaceFile } from "./cobolglobalcache";
import { VSCOBOLFileUtils } from "./vsfileutils";
import { ExtensionDefaults } from "./extensionDefaults";
import { MakeDep } from "./makedeps";
import { IExternalFeatures } from "./externalfeatures";
import { COBOLCopyBookProvider } from "./opencopybook";

class FileScanStats {
    directoriesScanned = 0;
    maxDirectoryDepth = 0;
    fileCount = 0;
    showMessage = false;
    directoriesScannedMap: Map<string, Uri> = new Map<string, Uri>();
}

export class VSCobScanner {
    public static readonly scannerBinDir = VSCobScanner.getCobScannerDirectory();

    private static getCobScannerDirectory(): string {
        const thisExtension = extensions.getExtension(ExtensionDefaults.thisExtensionName);
        if (thisExtension !== undefined) {
            const extPath = `${thisExtension.extensionPath}`;
            return path.join(extPath, "dist");
        }
        return "";
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private static async forkScanner(externalFeatures: IExternalFeatures, settings: ICOBOLSettings, sf: ScanData, reason: string, updateNow: boolean, useThreaded: boolean, threadCount: number): Promise<void> {
        const jcobscannerJS = path.join(VSCobScanner.scannerBinDir, "cobscanner.js");

        const options: ForkOptions = {
            stdio: [0, 1, 2, "ipc"],
            cwd: VSCobScanner.scannerBinDir,
            env: {
                SCANDATA: ScanDataHelper.getScanData(sf),
                SCANDATA_TCOUNT: `${threadCount}`
            }
        };
        const scannerStyle = useThreaded ? "useenv_threaded" : "useenv";
        const child = fork(jcobscannerJS, [scannerStyle], options);

        // const child = ;
        if (child === undefined) {
            return;
        }

        const timer = setTimeout(function () {
            try {
                child.kill();
            } catch (err) {
                VSLogger.logException(`Timeout, ${reason}`, err as Error);
            }
        }, settings.cache_metadata_inactivity_timeout);

        child.on("error", err => {
            VSLogger.logException(`Fork caused ${reason}`, err);
        });

        child.on("exit", code => {
            clearTimeout(timer);

            if (code !== 0) {
                if (sf.cache_metadata_verbose_messages) {
                    VSLogger.logMessage(`External scan completed (${child.pid}) [Exit Code=${code}}]`);
                }
            } else {
                progressStatusBarItem.hide();
            }
            if (updateNow) {
                VSCOBOLUtils.saveGlobalCacheToWorkspace(settings);
            }
        });

        let percent = 0;
        let currentFile = "";
        let programId = "";
        let copyBooksNames: Array<string> = [];
        let processUnUsedCopyBooks: Array<string> = [];
        child.on("message", async (msg) => {
            timer.refresh();        // restart timer
            const message = msg as string;
            if (message.startsWith("@@")) {
                InMemoryGlobalSymbolCache.isDirty = true;
                if (message.startsWith(COBSCANNER_STATUS)) {
                    const args = message.split(" ");
                    progressStatusBarItem.show();
                    percent += Number.parseInt(args[1], 10);
                    progressStatusBarItem.text = `Processing metadata: ${percent}%`;
                }
                else if (message.startsWith(COBSCANNER_SENDEP)) {
                    const args = message.split(",");
                    const tokenName = args[1];
                    const tokenLine = Number.parseInt(args[2], 10);
                    const tokenFilename = args[3];
                    COBOLWorkspaceSymbolCacheHelper.addEntryPoint(tokenFilename, tokenName, tokenLine);
                }
                else if (message.startsWith(COBSCANNER_SENDPRGID)) {
                    const args = message.split(",");
                    const tokenName = args[1];
                    const tokenLine = Number.parseInt(args[2], 10);
                    const tokenFilename = args[3];
                    programId = tokenName;
                    COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(tokenFilename);
                    COBOLWorkspaceSymbolCacheHelper.removeAllTypes(tokenFilename);
                    COBOLWorkspaceSymbolCacheHelper.addCalableSymbol(tokenFilename, tokenName, tokenLine);
                }
                else if (message.startsWith(COBSCANNER_SENDCLASS)) {
                    const args = message.split(",");
                    const tokenName = args[1];
                    const tokenLine = Number.parseInt(args[2], 10);
                    const tokenFilename = args[3];
                    COBOLWorkspaceSymbolCacheHelper.addClass(tokenFilename, tokenName, tokenLine, TypeCategory.ClassId);
                }
                else if (message.startsWith(COBSCANNER_SENDINTERFACE)) {
                    const args = message.split(",");
                    const tokenName = args[1];
                    const tokenLine = Number.parseInt(args[2], 10);
                    const tokenFilename = args[3];
                    COBOLWorkspaceSymbolCacheHelper.addClass(tokenFilename, tokenName, tokenLine, TypeCategory.InterfaceId);
                }
                else if (message.startsWith(COBSCANNER_SENDENUM)) {
                    const args = message.split(",");
                    const tokenName = args[1];
                    const tokenLine = Number.parseInt(args[2], 10);
                    const tokenFilename = args[3];
                    COBOLWorkspaceSymbolCacheHelper.addClass(tokenFilename, tokenName, tokenLine, TypeCategory.EnumId);
                } else if (message.startsWith(COBSCANNER_ADDFILE)) {
                    const args = message.split(",");
                    const ms = BigInt(args[1]);
                    const fullFilename = args[2];
                    const fsUri = Uri.file(fullFilename);
                    const shortFilename = VSCOBOLFileUtils.getShortWorkspaceFilename(fsUri.scheme, fullFilename, settings);
                    if (shortFilename !== undefined) {
                        const cws = new COBOLWorkspaceFile(ms, shortFilename);
                        COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(shortFilename);
                        InMemoryGlobalCacheHelper.addFilename(fullFilename, cws);
                    } else {
                        VSLogger.logMessage(`Unable to getShortWorkspaceFilename for ${fullFilename}`);
                    }
                }
                else if (message.startsWith(COBSCANNER_KNOWNCOPYBOOK)) {
                    const args = message.split(",");
                    const enKey = args[1];
                    const inFilename = args[2];
                    const inOf = args[3];
                    COBOLWorkspaceSymbolCacheHelper.addReferencedCopybook(enKey, inFilename, inOf);

                    const fileName = COBOLCopyBookProvider.expandLogicalCopyBookOrEmpty(enKey, inOf, settings, inFilename, externalFeatures);
                    // VSLogger.logMessage(`Referenced copybook ${enKey} in ${inFilename} resolved to ${fileName}`);
                    if (fileName.length !== 0) {
                        if (copyBooksNames.includes(fileName) === false) {
                            copyBooksNames.push(fileName);
                        }
                    } else {
                        if (processUnUsedCopyBooks.includes(enKey) === false) {
                            processUnUsedCopyBooks.push(enKey);
                        }
                    }
                } else if (message.startsWith(COBSCANNER_START_OF_FILE)) {
                    const args = message.split(",");
                    currentFile = args[1];
                    // VSLogger.logMessage(`Starting scan of ${currentFile}`);
                } else if (message.startsWith(COBSCANNER_END_OF_FILE)) {
                    const args = message.split(",");
                    currentFile = args[1];
                    if (programId.length !== 0) {
                        const makeDepLines = MakeDep.CreateDependencyFile(settings, currentFile, copyBooksNames, processUnUsedCopyBooks, currentFile);
                        if (makeDepLines.length !== 0) {
                            const lastDot = currentFile.lastIndexOf(".");
                            if (lastDot !== -1) {
                                currentFile = currentFile.substring(0, lastDot);
                            }
                            const dirName = path.dirname(currentFile);
                            const dirFilename = path.basename(currentFile);
                            const newFilename = path.join(dirName, `${settings.makefile_dependency_prefix}${dirFilename}.d`);
                            await workspace.fs.writeFile(Uri.file(newFilename), Buffer.from(makeDepLines.join("\n"), "utf8"));
                        }
                    }

                    // VSLogger.logMessage(`Ending scan of ${currentFile}`);
                    programId = "";
                    copyBooksNames = [];
                    processUnUsedCopyBooks = [];
                }
            } else {
                VSLogger.logMessage(msg as string);
            }
        });

        if (child.stdout !== null) {
            for await (const data of child.stdout) {
                // compress the output
                const lines: string = data.toString();
                for (const line of lines.split("\n")) {
                    const lineTrimmed = line.trim();
                    if (lineTrimmed.length !== 0) {
                        VSLogger.logMessage(` ${line}`);
                    }
                }
            }
        }

        if (child.stderr !== null) {
            for await (const data of child.stderr) {
                // compress the output
                const lines: string = data.toString();
                for (const line of lines.split("\n")) {
                    const lineTrimmed = line.trim();
                    if (lineTrimmed.length !== 0) {
                        VSLogger.logMessage(` [${line}]`);
                    }
                }
            }
        }
    }


    private static getScanData(settings: ICOBOLSettings, ws: ReadonlyArray<WorkspaceFolder>, stats: FileScanStats, files: string[]): ScanData {
        const sf = new ScanData();
        sf.scannerBinDir = VSCobScanner.scannerBinDir;
        sf.directoriesScanned = stats.directoriesScanned;
        sf.maxDirectoryDepth = stats.maxDirectoryDepth;
        sf.fileCount = stats.fileCount;

        sf.parse_copybooks_for_references = settings.parse_copybooks_for_references;
        sf.Files = files;
        sf.cache_metadata_verbose_messages = settings.cache_metadata_verbose_messages;
        sf.md_symbols = settings.metadata_symbols;
        sf.md_entrypoints = settings.metadata_entrypoints;
        sf.md_metadata_files = settings.metadata_files;
        sf.md_metadata_knowncopybooks = settings.metadata_knowncopybooks;
        sf.md_copybookdirs = settings.copybookdirs;
        sf.md_copybookexts = settings.copybookexts;
        sf.md_file_search_directory = settings.file_search_directory;
        sf.md_makefile_dependancy_fullpath = settings.makefile_dependancy_fullpath;

        if (ws !== undefined) {
            for (const f of ws) {
                if (f !== undefined && f.uri.scheme === "file") {
                    sf.workspaceFolders.push(f.uri.fsPath);
                }
            }
        }

        return sf;
    }

    public static async processAllFilesInWorkspaceOutOfProcess(externalFeatures: IExternalFeatures, settings: ICOBOLSettings, viaCommand: boolean, useThreaded: boolean, threadCount: number): Promise<void> {

        const msgViaCommand = "(" + (viaCommand ? "on demand" : "startup") + ")";

        const ws = VSWorkspaceFolders.get(settings);
        const stats = new FileScanStats();
        const files: string[] = [];

        if (ws === undefined) {
            VSLogger.logMessage(`No workspace folders available ${msgViaCommand}`);
            return;
        }

        if (!viaCommand) {
            VSLogger.logChannelHide();
        } else {
            VSLogger.logChannelSetPreserveFocus(!viaCommand);
        }
        VSLogger.logMessage("");
        VSLogger.logMessage(`Starting to process metadata from workspace folders ${msgViaCommand}`);

        await VSCOBOLUtils.populateDefaultCallableSymbols(settings, false);
        for (const [, b] of InMemoryGlobalSymbolCache.defaultCallableSymbols) {
            files.push(b);
        }

        VSCOBOLUtils.saveGlobalCacheToWorkspace(settings, false);

        const sf = this.getScanData(settings, ws, stats, files);
        await VSCobScanner.forkScanner(externalFeatures, settings, sf, msgViaCommand, true, useThreaded, threadCount);
        VSCOBOLUtils.saveGlobalCacheToWorkspace(settings, true);
    }
}
