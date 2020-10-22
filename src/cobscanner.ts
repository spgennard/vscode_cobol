import COBOLSourceScanner, { SharedSourceReferences } from "./cobolsourcescanner";
import { COBOLSymbolTableEventHelper } from "./cobolsymboltableeventhelper";
import { ScanDataHelper, ScanStats } from "./cobscannerdata";
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

    private static msleep(n: number) {
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

    public static performance_now(): number {
        if (!process.env.BROWSER) {
            try {
                // eslint-disable-next-line @typescript-eslint/no-var-requires
                return require('performance-now').performance.now;
            }
            catch {
                return Date.now();
            }
        }

        return Date.now();
    }
}

console.clear();

for (const arg of args) {

    if (arg.endsWith(".json")) {
        let exitnow = false;
        while (exitnow === false) {
            const scanData = ScanDataHelper.load(arg);
            const aborted = false;
            const stats = new ScanStats();
            stats.start = Utils.performance_now();
            stats.directoriesScanned = scanData.directoriesScanned;
            stats.maxDirectoryDepth = scanData.maxDirectoryDepth;
            stats.fileCount = scanData.fileCount;

            GlobalCachesHelper.loadGlobalSymbolCache(scanData.cacheDirectory);
            features.logMessage(` Directories scanned : ${stats.directoriesScanned}`);
            features.logMessage(` Directory Depth     : ${stats.maxDirectoryDepth}`);
            features.logMessage(` Files found         : ${stats.fileCount}`);
            try {

                for (const file of scanData.Files) {
                    const cacheDir = scanData.cacheDirectory;

                    if (Utils.cacheUpdateRequired(cacheDir, file)) {
                        const filesHandler = new FileSourceHandler(file, false);
                        const config = new COBOLSettings();
                        config.parse_copybooks_for_references = scanData.parse_copybooks_for_references;
                        const symbolCacher = new COBOLSymbolTableEventHelper(config);
                        const qcp = new COBOLSourceScanner(filesHandler, config, cacheDir, new SharedSourceReferences(true), false, symbolCacher, features);
                        if (qcp.callTargets.size > 0) {
                            stats.programsDefined++;
                            if (qcp.callTargets !== undefined) {
                                stats.entryPointsDefined += (qcp.callTargets.size - 1);
                            }
                        }

                        if (scanData.showMessage) {
                            features.logMessage(`  Updating: ${file}`);
                        }
                        stats.filesScanned++;
                    } else {
                        stats.filesUptodate++;
                    }
                }
            } finally {
                const end = Utils.performance_now() - stats.start;
                const completedMessage = (aborted ? `Scan aborted (elapsed time ${end})` : 'Completed scanning all COBOL files in workspace');
                if (features.logTimedMessage(end, completedMessage) === false) {
                    features.logMessage(completedMessage);
                }

                if (stats.filesScanned !== 0) {
                    features.logMessage(` Files scanned       : ${stats.filesScanned}`);
                }

                if (stats.filesUptodate) {
                    features.logMessage(` Files up to date    : ${stats.filesUptodate}`);
                }

                features.logMessage(` Program Count       : ${stats.programsDefined}`);
                features.logMessage(` Entry-Point Count   : ${stats.entryPointsDefined}`);
                GlobalCachesHelper.saveGlobalCache(scanData.cacheDirectory);

                // delete the json file
                fs.unlinkSync(arg);
                exitnow = true;
            }
        }
    }
    // console.log(scanner);
}