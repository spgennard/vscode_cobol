import path from "path";
import fs from "fs";

export class ScanData {
    public parse_copybooks_for_references = false;
    public cacheDirectory = "";
    public Files: string[] = [];
}

export class ScanDataHelper {
    static readonly scanFilename = "cobscanner.json";

    public static save(cacheDirectory: string, st: ScanData): void {
        const fn = path.join(cacheDirectory,ScanDataHelper.scanFilename);

        fs.writeFileSync(fn, JSON.stringify(st));
    }

    public static load(fn: string) : ScanData {
        // const fn = path.join(cacheDirectory, ScanDataHelper.scanFilename);
        const str: string = fs.readFileSync(fn).toString();
        return JSON.parse(str) as ScanData;
    }
}