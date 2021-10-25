import { Extension, extensions } from "vscode";
import { COBAPIConstants, COBOLPreprocessor } from "./cobapi";
import { CobApiHandle } from "./cobapiimpl";
import { COBOLPreprocessorHelper } from "./cobolsourcescanner";
import { VSLogger } from "./vslogger";
import { ICOBOLSettings } from "./iconfiguration";
import { VSExternalFeatures } from "./vsexternalfeatures";

export class VSPreProc {

    public static async areAllPreProcessorsReady(settings: ICOBOLSettings): Promise<boolean> {
        return VSPreProc.registerPreProcessors(settings);
    }

    public static async registerPreProcessors(settings: ICOBOLSettings): Promise<boolean> {
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
                    VSLogger.logMessage(` WARNING: PreProcessors ${extName} not found, continuing without it`);
                    clear = true;
                    break;
                }

                // wait for extension
                if (ppExt.isActive === false) {
                    // wait for extension
                    await ppExt.activate();
                }

                // leave early because the extenion is not ready
                if (ppExt.isActive === false) {
                    failed = true;
                    break;
                }

                const pp = VSPreProc.getCOBOLPreprocessor(ppExt, extName);
                if (pp !== undefined) {
                    const handle = new CobApiHandle(pp.getPackageJson(), VSExternalFeatures);
                    try {
                        if (pp.getImplementedVersion() !== COBAPIConstants.COB_API_INTERFACE_VERSION) {
                            failed = true;
                            settings.preprocessor_extensions = [];      // brutal
                            VSLogger.logMessage(` WARNING: PreProcessors disable due to ${extName} implementing a old version of the interface`);
                        } else {
                            COBOLPreprocessorHelper.preprocessors.set(handle, pp);
                            COBOLPreprocessorHelper.preprocessorsExts.set(extName, handle);
                            show = true;
                        }
                    } catch {
                        clear = true;
                        VSLogger.logMessage(` WARNING: PreProcessors disable due to ${extName} causing an error during initialization`);
                    }
                } else {
                    VSLogger.logMessage(` WARNING: PreProcessors ${extName} not found, continuing without it`);
                    clear = true;
                }
            }
            catch (e) {
                failed = true;
                VSLogger.logException(`Unable to get preprocessor : ${extName}`, e as Error);
            }
        }

        if (show) {
            VSPreProc.dumpPreProcInfo(settings);
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
            VSLogger.logMessage(`Extension ${preprocexp.id} is found, Active=${preprocexp.isActive}`);
            return preprocexp.exports as COBOLPreprocessor;
        }
        catch (e) {
            VSLogger.logMessage(`getCOBOLPreprocessor(${extensionName})`, e);
            return undefined;
        }
    }



    public static dumpPreProcInfo(settings: ICOBOLSettings): void {
        for (const extName of settings.preprocessor_extensions) {
            if (COBOLPreprocessorHelper.preprocessorsExts.has(extName)) {
                const activePreProc = COBOLPreprocessorHelper.preprocessorsExts.get(extName);

                if (activePreProc !== undefined) {
                    VSLogger.logMessage(` Active preprocessor              : ${activePreProc.id}`);
                    VSLogger.logMessage(`                                  : ${activePreProc.description}`);
                    VSLogger.logMessage(`                                  : ${activePreProc.bugReportEmail}`);
                    VSLogger.logMessage(`                                  : ${activePreProc.bugReportUrl}`);
                }
                continue;
            }
            VSLogger.logMessage('');
            VSLogger.logMessage(` Registered preprocessor          : ${extName}`);
            VSLogger.logMessage('');
        }
    }
}