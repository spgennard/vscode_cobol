import COBOLSourceScanner from "./cobolsourcescanner";
import { COBOLSymbolTableEventHelper } from "./cobolsymboltableeventhelper";
import { ScanDataHelper } from "./cobscannerdata";
import { ConsoleExternalFeatures } from "./consoleexternalfeatures";

import { IExternalFeatures } from "./externalfeatures";
import { FileSourceHandler } from "./filesourcehandler";
import { GlobalCachesHelper } from "./globalcachehelper";
import { COBOLSettings } from "./iconfiguration";

const args = process.argv.slice(2);
const features: IExternalFeatures = ConsoleExternalFeatures.Default;

for (const arg of args) {

    if (arg.endsWith(".json")) {
        const scanData = ScanDataHelper.load(arg);
        GlobalCachesHelper.loadGlobalSymbolCache(scanData.cacheDirectory);
        features.logMessage("Scanning : ");
        for(const file of scanData.Files) {
            const filesHandler= new FileSourceHandler(file, false);
            const config = new COBOLSettings();
            const cacheDir = scanData.cacheDirectory;
            const symbolCacher = new COBOLSymbolTableEventHelper(config);

            features.logMessage(`  => ${file}`);
            const scanner = COBOLSourceScanner.ParseCached(filesHandler, config, cacheDir, false, symbolCacher, features);
        }
        GlobalCachesHelper.saveGlobalCache(scanData.cacheDirectory);
    } else {
        features.logMessage(`Unknown argument : ${arg}`);

    }
    // console.log(scanner);
}