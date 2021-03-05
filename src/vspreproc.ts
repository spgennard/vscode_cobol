import { extensions } from "vscode";
import { COBOLPreprocessor } from "./cobapi";
import { CobApiHandle } from "./cobapiimpl";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { ExternalFeatures } from "./extension";
import { ICOBOLSettings } from "./iconfiguration";

export class VSPreProc {
    public static registerPreProcessors(settings: ICOBOLSettings): void {
        for (const extName of settings.preprocessor_extensions) {
            if (COBOLPreprocessorHelper.preprocessorsExts.has(extName)) {
                continue;
            }
            const pp = VSPreProc.getCOBOLPreprocessor(extName);
            if (pp !== undefined) {
                const handle = new CobApiHandle(pp.getPackageJson(), ExternalFeatures);
                COBOLPreprocessorHelper.preprocessors.set(handle, pp);
                COBOLPreprocessorHelper.preprocessorsExts.set(extName, handle);
            }
        }
    }

    private static msleep(n: number) {
        Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, n);
    }


    public static getCOBOLPreprocessor(extensionName: string): COBOLPreprocessor | undefined {
        let retryCount = 4;
        let failedExtension = "";
        while (retryCount > 0) {
            try {
                const preprocexp = extensions.getExtension(extensionName);
                if (preprocexp?.exports === undefined) {
                    throw new Error(`${extensionName} has no exports`);
                }

                return preprocexp.exports as COBOLPreprocessor;
            }
            catch {
                failedExtension = extensionName;
                return undefined;
            }
            this.msleep(50);
            retryCount--;
        }

        ExternalFeatures.logMessage(`Unable to find ${failedExtension} in a timely fashion`);
        return undefined;
    }

}