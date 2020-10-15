import COBOLSourceScanner from "./cobolsourcescanner";
import { COBOLSymbolTableEventHelper } from "./cobolsymboltableeventhelper";
import { ScanDataHelper } from "./cobscannerdata";
import { ConsoleExternalFeatures } from "./consoleexternalfeatures";

import { IExternalFeatures } from "./externalfeatures";
import { FileSourceHandler } from "./filesourcehandler";
import { GlobalCachesHelper } from "./globalcachehelper";
import { COBOLSettings } from "./iconfiguration";

import * as crypto from 'crypto';
import * as fs from 'fs';
import { Hash } from "crypto";
import path from "path";

const args = process.argv.slice(2);
const features: IExternalFeatures = ConsoleExternalFeatures.Default;

class Utils {
    static readonly defaultStats = new fs.Stats();

    private static msleep(n:number) {
        Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, n);
    }

    public static sleep(n: number) {
        Utils.msleep(n * 1000);
    }

    private static isFileT(sdir: string): [boolean, fs.Stats] {
        let f: fs.Stats = Utils.defaultStats;
        try {
            if (fs.existsSync(sdir)) {
                f = fs.statSync(sdir);
                if (f && f.isFile()) {
                    return [true, f];
                }
            }
        }
        catch {
            return [false, f];
        }
        return [false, f];
    }

    public static getHashForFilename(filename: string) {
        const hash: Hash = crypto.createHash('sha256');
        hash.update(filename);
        return hash.digest('hex');
    }

    public static cacheUpdateRequired(cacheDirectory: string, nfilename: string): boolean {
        const filename = path.normalize(nfilename);

        const fn: string = path.join(cacheDirectory, this.getHashForFilename(filename) + ".sym");
        const fnStat = Utils.isFileT(fn);
        if (fnStat[0]) {
            const stat4cache = fnStat[1];
            const stat4src = fs.statSync(filename);
            if (stat4cache.mtimeMs < stat4src.mtimeMs) {
                return true;
            }
            return false;
        }

        return true;
    }

}

console.clear();

for (const arg of args) {

    if (arg.endsWith(".json")) {
        const exitnow = false;
        while (exitnow === false) {
            const scanData = ScanDataHelper.load(arg);
            GlobalCachesHelper.loadGlobalSymbolCache(scanData.cacheDirectory);
            for (const file of scanData.Files) {
                const cacheDir = scanData.cacheDirectory;

                if (Utils.cacheUpdateRequired(cacheDir, file)) {
                    const filesHandler = new FileSourceHandler(file, false);
                    const config = new COBOLSettings();
                    config.parse_copybooks_for_references = scanData.parse_copybooks_for_references;
                    const symbolCacher = new COBOLSymbolTableEventHelper(config);

                    features.logMessage(`  Updating: ${file}`);
                    const scanner = COBOLSourceScanner.ParseCached(filesHandler, config, cacheDir, false, symbolCacher, features);
                }
            }
            GlobalCachesHelper.saveGlobalCache(scanData.cacheDirectory);

            Utils.sleep(2);
        }
    }
    // console.log(scanner);
}