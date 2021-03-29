import { extensions } from "vscode";
import { COBAPIConstants, COBOLPreprocessor } from "./cobapi";
import { CobApiHandle } from "./cobapiimpl";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { dumpPreProcInfo, ExternalFeatures, logException, logMessage } from "./extension";
import { ICOBOLSettings } from "./iconfiguration";

export class VSPreProc {

    public static areAllPreProcessorsReady(settings: ICOBOLSettings): boolean {
        return VSPreProc.registerPreProcessors(settings);
    }

    public static registerPreProcessors(settings: ICOBOLSettings): boolean {
        let show = false;
        let failed = false;
        let clear = false;

        for (const extName of settings.preprocessor_extensions) {
            if (COBOLPreprocessorHelper.preprocessorsExts.has(extName)) {
                continue;
            }
            try {
                const pp = VSPreProc.getCOBOLPreprocessor(extName);
                if (pp !== undefined) {
                    const handle = new CobApiHandle(pp.getPackageJson(), ExternalFeatures);
                    try {
                        if (pp.getImplementedVersion() !== COBAPIConstants.COB_API_INTERFACE_VERSION) {
                            failed = true;
                            settings.preprocessor_extensions = [];      // brutal
                            logMessage(` WARNING: PreProcessors disable due to ${extName} implementing a old version of the interface`);
                        } else {
                            COBOLPreprocessorHelper.preprocessors.set(handle, pp);
                            COBOLPreprocessorHelper.preprocessorsExts.set(extName, handle);
                            show = true;
                        }
                    } catch {
                        clear = true;
                        logMessage(` WARNING: PreProcessors disable due to ${extName} causing an error during initialization`);
                    }
                } else {
                    logMessage(` WARNING: PreProcessors ${extName} not found, continuing without it`);
                    clear = true;
                }
            }
            catch(e){
                failed = true;
                logException(`Unable to get preprocessor : ${extName}`,e);
            }
        }

        if (show) {
            dumpPreProcInfo(settings);
        }

        if (clear) {
            settings.preprocessor_extensions = [];      // brutal
        }

        return !failed;
    }

    private static msleep(n: number) {
        Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, n);
    }


    public static getCOBOLPreprocessor(extensionName: string): COBOLPreprocessor | undefined {
        try {
            const preprocexp = extensions.getExtension(extensionName);
            if (preprocexp === undefined) {
                return preprocexp;
            }

            if (preprocexp?.exports === undefined) {
                return undefined;
            }

            return preprocexp.exports as COBOLPreprocessor;
        }
        catch {
            return undefined;
        }
    }

}