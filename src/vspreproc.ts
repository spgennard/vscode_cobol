import { extensions } from "vscode";
import { COBOLPreprocessor } from "./cobapi";
import { CobApiHandle } from "./cobapiimpl";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { dumpPreProcInfo, ExternalFeatures } from "./extension";
import { ICOBOLSettings } from "./iconfiguration";

export class VSPreProc {
    public static areAllPreProcessorsReady(settings: ICOBOLSettings): boolean {
        return VSPreProc.registerPreProcessors(settings);
    }

    public static registerPreProcessors(settings: ICOBOLSettings): boolean {
        let show = false;
        let failed = false;
        for (const extName of settings.preprocessor_extensions) {
            if (COBOLPreprocessorHelper.preprocessorsExts.has(extName)) {
                continue;
            }
            const pp = VSPreProc.getCOBOLPreprocessor(extName);
            if (pp !== undefined) {
                const handle = new CobApiHandle(pp.getPackageJson(), ExternalFeatures);
                COBOLPreprocessorHelper.preprocessors.set(handle, pp);
                COBOLPreprocessorHelper.preprocessorsExts.set(extName, handle);
                show = true;
            } else {
                failed = true;
            }
        }

        if (show) {
            dumpPreProcInfo(settings);
        }

        return !failed;
    }

    private static msleep(n: number) {
        Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, n);
    }


    public static getCOBOLPreprocessor(extensionName: string): COBOLPreprocessor | undefined {
        const preprocexp = extensions.getExtension(extensionName);
        if (preprocexp === undefined) {
            return preprocexp;
        }

        try {
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