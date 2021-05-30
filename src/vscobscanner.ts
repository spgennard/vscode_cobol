import path from "path";

import { extensions, FileType, Uri, workspace, WorkspaceFolder } from "vscode";
import { getWorkspaceFolders } from "./cobolfolders";
import { COBSCANNER_ADDFILE, COBSCANNER_KNOWNCOPYBOOK, COBSCANNER_SENDCLASS, COBSCANNER_SENDENUM, COBSCANNER_SENDEP, COBSCANNER_SENDINTERFACE, COBSCANNER_SENDPRGID, COBSCANNER_STATUS, ScanData, ScanDataHelper } from "./cobscannerdata";
import { VSCOBOLConfiguration } from "./configuration";
import { COBOLStatUtils, logChannelHide, logChannelSetPreserveFocus, logException, logMessage, progressStatusBarItem } from "./extension";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLFileUtils } from "./opencopybook";
import VSCOBOLSourceScanner from "./vscobolscanner";
import { ChildProcess, fork, ForkOptions } from 'child_process';
import { COBOLWorkspaceSymbolCacheHelper, TypeCategory } from "./cobolworkspacecache";
import { COBOLUtils } from "./cobolutils";
import tempDirectory from 'temp-dir';
import { InMemoryGlobalCacheHelper, InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { COBOLWorkspaceFile } from "./cobolglobalcache";
import { COBOLSourceScannerUtils } from "./cobolsourcescannerutils";
import { VSCobScanner_depreciated } from "./vscobscanner_depreciated";


class FileScanStats {
    parentPid = 0;
    filesIgnored = 0;
    directoriesScanned = 0;
    directoryDepth = 0;
    maxDirectoryDepth = 0;
    fileCount = 0;
    showMessage = false;
    directoriesScannedMap: Map<string, Uri> = new Map<string, Uri>();
}

export class VSCobScanner {
    public static readonly scannerBinDir = VSCobScanner.getCobScannerDirectory();

    public static async processSavedFile(fsPath: string, settings: ICOBOLSettings): Promise<void> {
        if (VSCOBOLConfiguration.isOnDiskCachingEnabled() === false) {
            return;
        }

        // handle when parsed
        if (settings.parse_copybooks_for_references) {
            return;
        }

        // cleanup old sym files
        const cacheDirectory = VSCOBOLSourceScanner.getDeprecatedCacheDirectory();
        if (cacheDirectory !== undefined) {
            COBOLSourceScannerUtils.cleanUpOldMetadataFiles(settings, cacheDirectory);
        }

        if (COBOLFileUtils.isValidCopybookExtension(fsPath, settings) || COBOLFileUtils.isValidProgramExtension(fsPath, settings)) {
            COBOLUtils.saveGlobalCacheToWorkspace(settings, false);
            const sf = new ScanData();
            sf.scannerBinDir = VSCobScanner.scannerBinDir;
            sf.showStats = false;
            sf.Files.push(fsPath);
            sf.fileCount = sf.Files.length;
            sf.parse_copybooks_for_references = settings.parse_copybooks_for_references;
            sf.cache_metadata_show_progress_messages = settings.cache_metadata_show_progress_messages;
            sf.md_symbols = settings.metadata_symbols;
            sf.md_entrypoints = settings.metadata_entrypoints;
            sf.md_types = settings.metadata_types;
            sf.md_metadata_files = settings.metadata_files;
            sf.md_metadata_knowncopybooks = settings.metadata_knowncopybooks;
            await this.forkScanner(settings, sf, "OnSave", true, true, false, -1);
        }
    }

    private static getCobScannerDirectory(): string {
        const thisExtension = extensions.getExtension("bitlang.cobol");
        if (thisExtension !== undefined) {
            const extPath = `${thisExtension.extensionPath}`;
            return path.join(extPath, "cobscanner");
        }
        return "";
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public static async forkScanner(settings: ICOBOLSettings, sf: ScanData, reason: string, deprecatedMode: boolean, updateNow: boolean, useThreaded: boolean, threadCount: number): Promise<void> {
        const jcobscanner_js = path.join(VSCobScanner.scannerBinDir, "cobscanner.js");
        let child: ChildProcess;

        if (deprecatedMode) {
            ScanDataHelper.save(tempDirectory, sf);
            const jsonFile = path.join(tempDirectory, ScanDataHelper.scanFilename);
            const options: ForkOptions = {
                stdio: [0, 1, 2, "ipc"],
                cwd: VSCobScanner.scannerBinDir
            };

            child = fork(jcobscanner_js, [jsonFile], options);
        }
        else {
            const options: ForkOptions = {
                stdio: [0, 1, 2, "ipc"],
                cwd: VSCobScanner.scannerBinDir,
                env: {
                    SCANDATA: ScanDataHelper.getScanData(sf),
                    SCANDATA_TCOUNT: `${threadCount}`
                }
            };
            const scannerStyle = useThreaded ? "useenv_threaded" : "useenv";

            child = fork(jcobscanner_js, [scannerStyle], options);
        }

        // const child = ;
        if (child == undefined) {
            return;
        }

        if (deprecatedMode) {
            VSCobScanner_depreciated.deprecatedActivePid = child.pid;
        }

        const timer = setTimeout(function () {
            try {
                child.kill()
            } catch (err) {
                logException(`Timeout, ${reason}`, err);
            }
        }, settings.cache_metadata_inactivity_timeout);

        child.on('error', err => {
            if (tempDirectory !== undefined && deprecatedMode) {
                VSCobScanner_depreciated.removeScannerFile(tempDirectory);
            }
            logException(`Fork caused ${reason}`, err);
        });

        child.on('exit', code => {
            if (deprecatedMode) {
                VSCobScanner_depreciated.deprecatedActivePid = 0;
            }
            clearTimeout(timer);

            if (code !== 0) {
                // if (sf.cache_metadata_show_progress_messages) {
                logMessage(`External scan completed (${child.pid}) [Exit Code=${code}}]`);
                // }
            } else {
                progressStatusBarItem.hide();
            }
            if (updateNow) {
                COBOLUtils.saveGlobalCacheToWorkspace(settings);
            }
        });

        let percent = 0;
        child.on('message', (msg) => {
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
                    const shortFilename = COBOLStatUtils.getShortWorkspaceFilename(fullFilename);
                    if (shortFilename !== undefined) {
                        const cws = new COBOLWorkspaceFile(ms, shortFilename);
                        COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(shortFilename);
                        if (fullFilename !== undefined) {
                            InMemoryGlobalCacheHelper.addFilename(fullFilename, cws);
                        }
                    }
                } else if (message.startsWith(COBSCANNER_KNOWNCOPYBOOK)) {
                    const args = message.split(",");
                    const enKey = args[1];
                    const inFilename = args[2];
                    COBOLWorkspaceSymbolCacheHelper.addReferencedCopybook(enKey, inFilename);
                }
            } else {
                logMessage(msg as string);
            }
        });

        if (child.stdout !== null) {
            for await (const data of child.stdout) {
                // compress the output
                const lines: string = data.toString();
                for (const line of lines.split("\n")) {
                    const lineTrimmed = line.trim();
                    if (lineTrimmed.length !== 0) {
                        logMessage(` ${line}`);
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
                        logMessage(` [${line}]`);
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
        sf.cache_metadata_show_progress_messages = settings.cache_metadata_show_progress_messages;
        sf.md_symbols = settings.metadata_symbols;
        sf.md_entrypoints = settings.metadata_entrypoints;
        sf.md_metadata_files = settings.metadata_files;
        sf.md_metadata_knowncopybooks = settings.metadata_knowncopybooks;
        for (const [, uri] of stats.directoriesScannedMap) {
            sf.Directories.push(uri.fsPath);
        }

        if (ws !== undefined) {
            for (const f of ws) {
                if (f !== undefined && f.uri.scheme === 'file') {
                    sf.workspaceFolders.push(f.uri.fsPath);
                }
            }
        }

        return sf;
    }

    public static async processAllFilesInWorkspaceOutOfProcess(viaCommand: boolean, useThreaded: boolean, threadCount:number): Promise<void> {

        const msgViaCommand = "(" + (viaCommand ? "on demand" : "startup") + ")";
        const settings = VSCOBOLConfiguration.get();

        const ws = getWorkspaceFolders();
        const stats = new FileScanStats();
        const files: string[] = [];

        if (ws === undefined) {
            logMessage(`No workspace folders available ${msgViaCommand}`);
            return;
        }

        if (!viaCommand) {
            logChannelHide();
        } else {
            logChannelSetPreserveFocus(!viaCommand);
        }
        logMessage("");
        logMessage(`Starting to process metadata from workspace folders ${msgViaCommand}`);

        if (ws !== undefined) {
            for (const folder of ws) {
                try {
                    await VSCobScanner.generateCOBScannerData(settings, folder.uri, stats, files);
                } catch {
                    continue;
                }
            }

        }

        COBOLUtils.saveGlobalCacheToWorkspace(settings, false);
 
        const sf = this.getScanData(settings, ws, stats, files);
        await VSCobScanner.forkScanner(settings, sf, msgViaCommand, false, true, useThreaded,threadCount);
    }

    private static async generateCOBScannerData(settings: ICOBOLSettings, folder: Uri, stats: FileScanStats, files2scan: string[]): Promise<boolean> {
        const entries = await workspace.fs.readDirectory(folder);
        stats.directoriesScanned++;
        if (stats.directoriesScannedMap.has(folder.fsPath)) {
            return true;
        }

        if (stats.showMessage) {
            const spaces = " ".repeat(stats.directoryDepth);
            logMessage(` ${spaces}Directory : ${folder.fsPath}`);
        }
        stats.directoriesScannedMap.set(folder.fsPath, folder);

        const dir2scan: Uri[] = [];

        for (const [entry, fileType] of entries) {
            switch (fileType) {
                case FileType.File | FileType.SymbolicLink:
                    {
                        const spaces4file = " ".repeat(1 + stats.directoryDepth);
                        logMessage(`${spaces4file} File : ${entry} in ${folder.fsPath} is a symbolic link which may cause duplicate data to be cached`);
                    }
                // eslint-disable-next-line no-fallthrough
                case FileType.File:
                    {
                        const fullPath = path.join(folder.fsPath, entry);
                        if (COBOLFileUtils.isValidProgramExtension(fullPath, settings) || COBOLFileUtils.isValidCopybookExtension(fullPath, settings)) {
                            files2scan.push(fullPath);
                            stats.fileCount++;
                        } else {
                            stats.filesIgnored++;
                        }
                    }
                    break;
                case FileType.Directory | FileType.SymbolicLink:
                    {
                        const spaces4dir = " ".repeat(1 + stats.directoryDepth);
                        logMessage(`${spaces4dir} Directory : ${entry} in ${folder.fsPath} is a symbolic link which may cause duplicate data to be cached`);
                    }
                // eslint-disable-next-line no-fallthrough
                case FileType.Directory:
                    if (!VSCOBOLSourceScanner.ignoreDirectory(entry)) {
                        const fullDirectory = path.join(folder.fsPath, entry);
                        if (!VSCOBOLSourceScanner.ignoreDirectory(entry)) {
                            try {
                                dir2scan.push(Uri.file(fullDirectory));
                            } catch (ex) {
                                logMessage(` Uri.file failed with ${fullDirectory} from ${folder.fsPath} + ${entry}`);
                                if (ex instanceof Error) {
                                    logException("Unexpected abort during Uri Parse", ex as Error);
                                } else {
                                    logMessage(ex);
                                }
                            }
                        }
                    }
                    break;
            }
        }

        if (dir2scan.length !== 0) {
            if (1 + stats.directoryDepth <= settings.cache_metadata_max_directory_scan_depth) {
                stats.directoryDepth++;
                for (const directoryUri of dir2scan) {
                    try {
                        await VSCobScanner.generateCOBScannerData(settings, directoryUri, stats, files2scan);
                    } catch {
                        continue;       // file not found
                    }
                }
                if (stats.directoryDepth > stats.maxDirectoryDepth) {
                    stats.maxDirectoryDepth = stats.directoryDepth;
                }
                stats.directoryDepth--;
            } else {
                logMessage(` Directories below : ${folder.fsPath} has not been scanned (depth limit is ${settings.cache_metadata_max_directory_scan_depth})`);
            }

        }

        return true;
    }


}
