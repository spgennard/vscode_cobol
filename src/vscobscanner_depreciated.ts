import path from "path";
import fs from 'fs';
import { ScanData, ScanDataHelper } from "./cobscannerdata";
import { VSCobScanner } from "./vscobscanner";
import { VSCOBOLConfiguration } from "./configuration";
import VSCOBOLSourceScanner from "./vscobolscanner";
import { logChannelHide, logChannelSetPreserveFocus, logException, logMessage } from "./extension";
import { ICOBOLSettings } from "./iconfiguration";
import { FileType, Uri, workspace } from "vscode";
import { COBOLFileUtils } from "./fileutils";
import { COBOLSourceScannerUtils } from "./cobolsourcescannerutils";
import { getWorkspaceFolders } from "./cobolfolders";

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
export class VSCobScanner_depreciated {

    public static deprecatedActivePid = 0;


    public static isDeprecatedAlive(pid: number): boolean {
        if (this.deprecatedActivePid === 0) {
            return false;
        }

        try {
            return process.kill(pid, 0);
        }
        catch (e) {
            return e.code === 'EPERM';
        }
    }

    public static IsDeprecatedScannerActive(cacheDirectory: string): boolean {

        const jsonFile = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        const jsonFileExists = fs.existsSync(jsonFile);

        if (VSCobScanner_depreciated.deprecatedActivePid === 0) {
            return jsonFileExists;
        }

        // if the file exists.. then leave early
        if (jsonFileExists) {
            return jsonFileExists;
        }

        return this.isDeprecatedAlive(VSCobScanner_depreciated.deprecatedActivePid);
    }

    public static removeScannerFile(cacheDirectory: string): void {
        const jsonFile = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        try {
            fs.unlinkSync(jsonFile);
        } catch {
            //continue
        }
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
                        await VSCobScanner_depreciated.generateCOBScannerData(settings, directoryUri, stats, files2scan);
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

    public static async deprecated_processAllFilesInWorkspaceOutOfProcess(viaCommand: boolean): Promise<void> {

        const msgViaCommand = "(" + (viaCommand ? "on demand" : "startup") + ")";
        const settings = VSCOBOLConfiguration.get();

        if (VSCOBOLConfiguration.isOnDiskCachingEnabled() === false) {
            logMessage(`Metadata cache is off, no action taken ${msgViaCommand}`);
            return;
        }

        const cacheDirectory = VSCOBOLSourceScanner.getDeprecatedCacheDirectory();
        if (cacheDirectory !== undefined && VSCobScanner_depreciated.IsDeprecatedScannerActive(cacheDirectory)) {
            COBOLSourceScannerUtils.cleanUpOldMetadataFiles(settings, cacheDirectory);
            if (!viaCommand) {
                logMessage(`Source scanner lock file is present on startup ${msgViaCommand}`);
                VSCobScanner_depreciated.removeScannerFile(cacheDirectory);
                logMessage(` - lock file released`);
            } else {
                logMessage(`Source scanner already active, no action taken ${msgViaCommand}`);
                return;
            }
        }


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
                    await VSCobScanner_depreciated.generateCOBScannerData(settings, folder.uri, stats, files);
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
                if (f !== undefined && f.uri.scheme === 'file') {
                    sf.workspaceFolders.push(f.uri.fsPath);
                }
            }
        }

        await VSCobScanner.forkScanner(settings, sf, msgViaCommand, true, true, false, -1);
    }

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
            await VSCobScanner.forkScanner(settings, sf, "OnSave", true, true, false, -1);
        }
    }

}