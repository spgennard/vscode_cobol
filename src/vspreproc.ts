import { Extension, extensions } from "vscode";
import { COBAPIConstants, COBOLPreprocessor } from "./cobapi";
import { CobApiHandle } from "./cobapiimpl";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { dumpPreProcInfo, ExternalFeatures, logMessage, VSLogger } from "./extension";
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
                const ppExt = VSPreProc.getExtension(extName);
                if (ppExt === undefined) {
                    logMessage(` WARNING: PreProcessors ${extName} not found, continuing without it`);
                    clear = true;
                    break;
                }

                // leave early because the extenion is not ready
                if (ppExt.isActive === false) {
                    failed = true;
                    break;
                }

                const pp = VSPreProc.getCOBOLPreprocessor(ppExt, extName);
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
            catch (e) {
                failed = true;
                VSLogger.logException(`Unable to get preprocessor : ${extName}`, e);
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

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    private static getExtension(extensionName: string): Extension<any> | undefined {
        for (const x of extensions.all) {
            if (x.id === extensionName) {
                return x;
            }
        }
        return undefined;
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static getCOBOLPreprocessor(preprocexp: Extension<any>, extensionName: string): COBOLPreprocessor | undefined {
        try {

            if (preprocexp?.exports === undefined) {
                return undefined;
            }
            logMessage(`Extension ${preprocexp.id} is found, Active=${preprocexp.isActive}`);
            return preprocexp.exports as COBOLPreprocessor;
        }
        catch (e) {
            logMessage(`getCOBOLPreprocessor(${extensionName})`, e);
            return undefined;
        }
    }

}