import COBOLSourceScanner from "./cobolsourcescanner";
import { COBOLSymbolTableEventHelper } from "./cobolsymboltableeventhelper";

import { EmptyExternalFeature } from "./externalfeatures";
import { FileSourceHandler } from "./filesourcehandler";
import { COBOLSettings } from "./iconfiguration";

const args = process.argv.slice(2);

for (const arg of args) {
    const file = new FileSourceHandler(arg, false);
    const config = new COBOLSettings();
    const cacheDir = "subdir";
    const symbolCacher = new COBOLSymbolTableEventHelper(config);
    const scanner = COBOLSourceScanner.ParseCached(file, config, cacheDir, false, symbolCacher, EmptyExternalFeature.Default);

    console.log(scanner);
}