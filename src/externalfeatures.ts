/* eslint-disable @typescript-eslint/ban-types */
/* eslint-disable class-methods-use-this */
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { ICOBOLSettings } from "./iconfiguration";

export interface IExternalFeatures {
    logMessage(message: string): void;
    logException(message: string, ex: Error): void;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean;
    performance_now(): number;
    expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string;
    getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined;
    setWorkspaceFolders(folders: string[]):void;
    getWorkspaceFolders(): string[];
    isFile(possibleFilename:string): boolean;
    isDirectory(possibleDirectory: string) : boolean;
    getFileModTimeStamp(filename:string):BigInt;
    getSourceTimeout(config: ICOBOLSettings): number;
    getURLCopyBookSearchPath(): string[];
    setURLCopyBookSearchPath(fileSearchDirectory: string[]):void;
    isFileASync(possibleFilename:string): Promise<boolean>;
    isDebuggerActive(): boolean;
}

export enum ESourceFormat {
    unknown = "unknown",
    fixed = "fixed",
    free = "free",
    terminal = "terminal",
    variable = "variable",
    jcl = "jcl"
}

export class CobolLinterProviderSymbols {
    public static NotReferencedMarker_internal = "CL001";
    public static NotReferencedMarker_external = "CL002";
    public static CopyBookNotFound = "CL003:";
    public static PortMessage = "CL004:";
    public static GeneralMessage = "CL005:";
          
    public static OLD_NotReferencedMarker_internal = "COBOL_NOT_REF";
    public static OLD_NotReferencedMarker_external = "ignore";
    public static OLD_CopyBookNotFound = "CopyBook";
}