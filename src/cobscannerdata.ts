/* eslint-disable @typescript-eslint/no-explicit-any */
import path from "path";
import fs from "fs";

export class ScanData {
    public directoriesScanned = 0;
    public maxDirectoryDepth = 0;
    public fileCount = 0;
    public parse_copybooks_for_references = false;
    public showMessage = false;
    public cacheDirectory = "";
    public Files: string[] = [];
    public Directories: string[] = [];
    public showStats = true;
    public md_symbols: string[] = [];
    public md_entrypoints: string[] = [];
    public md_types: string[] = [];
    public md_metadata_files: string[] = [];
    public md_metadata_knowncopybooks: string[] = [];
    public workspaceFolders: string[] = [];
}

export class ScanStats {
    timeCap = 600000;
    directoriesScanned = 0;
    directoryDepth = 0;
    maxDirectoryDepth = 0;
    filesScanned = 0;
    copyBookExts = 0;
    fileCount = 0;
    filesUptodate = 0;
    programsDefined = 0;
    entryPointsDefined = 0;
    start = 0;
    showMessage = false;
}

export const COBSCANNER_STATUS = '@@STATUS';
export const COBSCANNER_SENDPRGID = '@@SEND.PRGID';
export const COBSCANNER_SENDEP = '@@SEND.EP';
export const COBSCANNER_SENDINTERFACE = "@@SEND.INTID";
export const COBSCANNER_SENDENUM = "@@SEND.ENUMID";
export const COBSCANNER_SENDCLASS = "@@SEND.CLASSID";
export const COBSCANNER_ADDFILE = "@@SEND.FILES";
export const COBSCANNER_KNOWNCOPYBOOK = "@@SEND.KNOWNCOPYBOOK";

function replacer(this: any, key: any, value: any): any {

    if (typeof value === 'bigint') {
        return value.toString();
    }

    return value;
}

export class ScanDataHelper {
    public static readonly scanFilename = "cobscanner.json";

    public static save(cacheDirectory: string, st: ScanData): string {
        const fn = path.join(cacheDirectory,ScanDataHelper.scanFilename);

        fs.writeFileSync(fn, JSON.stringify(st,replacer));

        return fn;
    }

    public static load(fn: string) : ScanData {
        // const fn = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        const str: string = fs.readFileSync(fn).toString();
        return JSON.parse(str) as ScanData;
    }
}