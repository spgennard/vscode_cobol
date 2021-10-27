/* eslint-disable @typescript-eslint/naming-convention */
import path from "path";
import fs from "fs";
import tempDirectory from "temp-dir";
import { VSCobScanner } from "./vscobscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { clearCOBOLCache } from "./vscobolscanner";
import { progressStatusBarItem } from "./extension";
import { VSLogger } from "./vslogger";
import { ICOBOLSettings, ICOBOLSettings_depreciated } from "./iconfiguration";
import { FileType, Uri, window, workspace } from "vscode";
import { COBOLFileUtils } from "./fileutils";
import { COBOLSourceScannerUtils } from "./cobolsourcescannerutils";
import { getVSWorkspaceFolders } from "./cobolfolders";
import { fork, ForkOptions } from "child_process";
import { InMemoryGlobalSymbolCache } from "./globalcachehelper";
import { COBSCANNER_KNOWNCOPYBOOK, COBSCANNER_SENDEP, COBSCANNER_SENDPRGID, COBSCANNER_STATUS, ScanData, ScanDataHelper } from "./cobscannerdata";
import { COBOLWorkspaceSymbolCacheHelper } from "./cobolworkspacecache";
import { CacheDirectoryStrategy } from "./externalfeatures";
import { VSCOBOLSourceScannerTools } from "./vssourcescannerutils";

class FileScanStats {
    filesIgnored = 0;
    directoriesScanned = 0;
    directoryDepth = 0;
    maxDirectoryDepth = 0;
    fileCount = 0;
    showMessage = false;
    directoriesScannedMap: Map<string, Uri> = new Map<string, Uri>();
}

                // const cacheDirectory: string | undefined = VSCobScanner_depreciated.getDeprecatedCacheDirectory();

export class VSCobScanner_depreciated implements ICOBOLSettings_depreciated {

    public static deprecatedActivePid: number|undefined = undefined;

    private static isDeprecatedAlive(pid: number): boolean {
        if (this.deprecatedActivePid === 0) {
            return false;
        }

        try {
            return process.kill(pid, 0);
        }
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        catch (e:any) {
            return e.code === "EPERM";
        }
    }

    public static isDeprecatedScannerActive(cacheDirectory: string): boolean {

        const jsonFile = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        const jsonFileExists = COBOLFileUtils.isFile(jsonFile);

        if (VSCobScanner_depreciated.deprecatedActivePid === 0) {
            return jsonFileExists;
        }

        // if the file exists.. then leave early
        if (jsonFileExists) {
            return jsonFileExists;
        }

        let validPid  = 0;
        if (VSCobScanner_depreciated.deprecatedActivePid !== undefined) {
            validPid = VSCobScanner_depreciated.deprecatedActivePid;
        }
        return this.isDeprecatedAlive(validPid);
    }

    private static depreciatedRemoveScannerFile(cacheDirectory: string): void {
        const jsonFile = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        try {
            fs.unlinkSync(jsonFile);
        } catch {
            //continue
        }
    }

    private static async depreciatedGenerateCOBScannerData(settings: ICOBOLSettings, folder: Uri, stats: FileScanStats, files2scan: string[]): Promise<boolean> {
        const entries = await workspace.fs.readDirectory(folder);
        stats.directoriesScanned++;
        if (stats.directoriesScannedMap.has(folder.fsPath)) {
            return true;
        }

        if (stats.showMessage) {
            const spaces = " ".repeat(stats.directoryDepth);
            VSLogger.logMessage(` ${spaces}Directory : ${folder.fsPath}`);
        }
        stats.directoriesScannedMap.set(folder.fsPath, folder);

        const dir2scan: Uri[] = [];

        for (const [entry, fileType] of entries) {
            switch (fileType) {
                case FileType.File | FileType.SymbolicLink:
                    {
                        const spaces4file = " ".repeat(1 + stats.directoryDepth);
                        VSLogger.logMessage(`${spaces4file} File : ${entry} in ${folder.fsPath} is a symbolic link which may cause duplicate data to be cached`);
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
                        VSLogger.logMessage(`${spaces4dir} Directory : ${entry} in ${folder.fsPath} is a symbolic link which may cause duplicate data to be cached`);
                    }
                // eslint-disable-next-line no-fallthrough
                case FileType.Directory:
                    if (!VSCOBOLSourceScannerTools.ignoreDirectory(entry)) {
                        const fullDirectory = path.join(folder.fsPath, entry);
                        if (!VSCOBOLSourceScannerTools.ignoreDirectory(entry)) {
                            try {
                                dir2scan.push(Uri.file(fullDirectory));
                            } catch (ex) {
                                VSLogger.logMessage(` Uri.file failed with ${fullDirectory} from ${folder.fsPath} + ${entry}`);
                                if (ex instanceof Error) {
                                    VSLogger.logException("Unexpected abort during Uri Parse", ex as Error);
                                } else {
                                    VSLogger.logMessage((ex as Error).message);
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
                        await VSCobScanner_depreciated.depreciatedGenerateCOBScannerData(settings, directoryUri, stats, files2scan);
                    } catch {
                        continue;       // file not found
                    }
                }
                if (stats.directoryDepth > stats.maxDirectoryDepth) {
                    stats.maxDirectoryDepth = stats.directoryDepth;
                }
                stats.directoryDepth--;
            } else {
                VSLogger.logMessage(` Directories below : ${folder.fsPath} has not been scanned (depth limit is ${settings.cache_metadata_max_directory_scan_depth})`);
            }

        }

        return true;
    }

    public static async deprecated_processAllFilesInWorkspaceOutOfProcess(viaCommand: boolean): Promise<void> {

        const msgViaCommand = "(" + (viaCommand ? "on demand" : "startup") + ")";
        const settings = VSCOBOLConfiguration.get();

        if (VSCOBOLConfiguration.isDepreciatedDiskCachingEnabled() === false) {
            VSLogger.logMessage(`Metadata cache is off, no action taken ${msgViaCommand}`);
            return;
        }

        const cacheDirectory = settings.get_depreciated_cache_directory();
        if (cacheDirectory !== undefined && VSCobScanner_depreciated.isDeprecatedScannerActive(cacheDirectory)) {
            COBOLSourceScannerUtils.cleanUpOldMetadataFiles(settings, cacheDirectory);
            if (!viaCommand) {
                VSLogger.logMessage(`Source scanner lock file is present on startup ${msgViaCommand}`);
                VSCobScanner_depreciated.depreciatedRemoveScannerFile(cacheDirectory);
                VSLogger.logMessage(" - lock file released");
            } else {
                VSLogger.logMessage(`Source scanner already active, no action taken ${msgViaCommand}`);
                return;
            }
        }


        const ws = getVSWorkspaceFolders();
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

        if (ws !== undefined) {
            for (const folder of ws) {
                try {
                    await VSCobScanner_depreciated.depreciatedGenerateCOBScannerData(settings, folder.uri, stats, files);
                } catch {
                    continue;
                }
            }

        }

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

        if (ws !== undefined) {
            for (const f of ws) {
                if (f !== undefined && f.uri.scheme === "file") {
                    sf.workspaceFolders.push(f.uri.fsPath);
                }
            }
        }

        await VSCobScanner_depreciated.depreciatedForkScanner(settings, sf, msgViaCommand, true, false, -1);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private static async depreciatedForkScanner(settings: ICOBOLSettings, sf: ScanData, reason: string, updateNow: boolean, useThreaded: boolean, threadCount: number): Promise<void> {
        const jcobscanner_js = path.join(VSCobScanner.scannerBinDir, "cobscanner.js");

        ScanDataHelper.save(tempDirectory, sf);
        const jsonFile = path.join(tempDirectory, ScanDataHelper.scanFilename);
        const options: ForkOptions = {
            stdio: [0, 1, 2, "ipc"],
            cwd: VSCobScanner.scannerBinDir
        };

        const child = fork(jcobscanner_js, [jsonFile], options);

        // const child = ;
        if (child === undefined) {
            return;
        }

        VSCobScanner_depreciated.deprecatedActivePid = child.pid;

        const timer = setTimeout(function () {
            try {
                child.kill();
            } catch (err) {
                VSLogger.logException(`Timeout, ${reason}`, err as Error);
            }
        }, settings.cache_metadata_inactivity_timeout);

        child.on("error", err => {
            if (tempDirectory !== undefined) {
                VSCobScanner_depreciated.depreciatedRemoveScannerFile(tempDirectory);
            }
            VSLogger.logException(`Fork caused ${reason}`, err);
        });

        child.on("exit", code => {
            VSCobScanner_depreciated.deprecatedActivePid = 0;
            clearTimeout(timer);

            if (code !== 0) {
                // if (sf.cache_metadata_show_progress_messages) {
                VSLogger.logMessage(`External scan completed (${child.pid}) [Exit Code=${code}}]`);
                // }
            } else {
                progressStatusBarItem.hide();
            }
        });

        let percent = 0;
        child.on("message", (msg) => {
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
                } else if (message.startsWith(COBSCANNER_KNOWNCOPYBOOK)) {
                    const args = message.split(",");
                    const enKey = args[1];
                    const inFilename = args[2];
                    COBOLWorkspaceSymbolCacheHelper.addReferencedCopybook(enKey, inFilename);
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


    public static async depreciatedProcessSavedFile(fsPath: string, settings: ICOBOLSettings): Promise<void> {
        if (VSCOBOLConfiguration.isDepreciatedDiskCachingEnabled() === false) {
            return;
        }

        // handle when parsed
        if (settings.parse_copybooks_for_references) {
            return;
        }

        // cleanup old sym files
        const cacheDirectory = settings.get_depreciated_cache_directory();
        if (cacheDirectory !== undefined) {
            COBOLSourceScannerUtils.cleanUpOldMetadataFiles(settings, cacheDirectory);
        }

        if (COBOLFileUtils.isValidCopybookExtension(fsPath, settings) || COBOLFileUtils.isValidProgramExtension(fsPath, settings)) {
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
            await VSCobScanner_depreciated.depreciatedForkScanner(settings, sf, "OnSave", true, false, -1);
        }
    }


    public static deprecatedClearMetaData(settings: ICOBOLSettings, cacheDirectory: string): void {
        if (VSCobScanner_depreciated.isDeprecatedScannerActive(cacheDirectory)) {
            window.showInformationMessage(" Unable to clear metadata while caching is already in progress");
            return;
        }

        window.showQuickPick(["Yes", "No"], { placeHolder: "Are you sure you want to clear the metadata?" }).then(function (data) {
            if (data === "Yes") {
                VSCobScanner_depreciated.wipeCacheDirectory(cacheDirectory);
                VSLogger.logMessage("Metadata cache cleared");
            }
        });
    }


    private static wipeCacheDirectory(cacheDirectory: string) {
        clearCOBOLCache();


        for (const file of fs.readdirSync(cacheDirectory)) {
            if (file.endsWith(".sym")) {
                const fileName = path.join(cacheDirectory, file);
                try {
                    fs.unlinkSync(fileName);
                }
                catch {
                    //continue
                }
            }
        }
        const jsonFile = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        try {
            fs.unlinkSync(jsonFile);
        } catch {
            //continue
        }
    }

    public get_depreciated_cache_directory(): string | undefined {

        const settings = VSCOBOLConfiguration.get();

        // turned off via the strategy
        if (settings.cache_metadata === CacheDirectoryStrategy.Off) {
            return undefined;
        }

        if (getVSWorkspaceFolders()) {

            if (settings.cache_metadata === CacheDirectoryStrategy.UserDefinedDirectory) {
                const storageDirectory: string | undefined = VSCOBOLSourceScannerTools.getUserStorageDirectory(settings);

                /* no storage directory */
                if (storageDirectory === undefined) {
                    return undefined;
                }

                /* not a directory, so ignore */
                if (!COBOLFileUtils.isDirectory(storageDirectory)) {
                    return undefined;
                }

                return storageDirectory;
            }

            let firstCacheDir = "";

            const ws = getVSWorkspaceFolders();
            if (ws !== undefined) {
                for (const folder of ws) {
                    const cacheDir2: string = path.join(folder.uri.fsPath, ".vscode_cobol");
                    if (COBOLFileUtils.isDirectory(cacheDir2)) {
                        return cacheDir2;
                    }
                    if (firstCacheDir === "") {
                        firstCacheDir = cacheDir2;
                    }
                }
            }

            if (firstCacheDir.length === 0) {
                return undefined;
            }

            if (COBOLFileUtils.isDirectory(firstCacheDir) === false) {
                try {
                    fs.mkdirSync(firstCacheDir);
                }
                catch {
                    return undefined;
                }
            }

            return firstCacheDir;
        }

        return undefined;
    }


}