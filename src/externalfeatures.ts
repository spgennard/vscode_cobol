/* eslint-disable class-methods-use-this */
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import ISourceHandler from "./isourcehandler";
import { ICOBOLSettings } from "./iconfiguration";

export interface IExternalFeatures {
    logMessage(message: string): void;
    logException(message: string, ex: Error): void;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean;
    performance_now(): number;
    expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string;
    getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat;
    getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined;
    setWorkspaceFolders(folders: string[]):void;
    getWorkspaceFolders(): string[];
    isDirectory(possibleDirectory: string) : boolean;
    getFileModTimeStamp(filename:string):BigInt;
    getCombinedCopyBookSearchPath(): string[];
    setCombinedCopyBookSearchPath(fileSearchDirectory: string[]):void;
}

export enum ESourceFormat {
    unknown = "unknown",
    fixed = "fixed",
    free = "free",
    terminal = "terminal",
    variable = "variable",
    jcl = "jcl"
}

export enum CacheDirectoryStrategy {
    Workspace = "workspace",
    UserDefinedDirectory = "user_defined_directory",
    Off = "off"
}

export class CobolLinterProviderSymbols {
    public static NotReferencedMarker_internal = "COBOL_NOT_REF";
    public static NotReferencedMarker_external = "ignore";
}

export class EmptyExternalFeature implements IExternalFeatures {
    public static readonly Default = new EmptyExternalFeature();


    public logMessage(message: string): void {
        return;
    }

    public logException(message: string, ex: Error): void {
        return;
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        return false;
    }

    public performance_now(): number {
        return Date.now();
    }

    public expandLogicalCopyBookToFilenameOrEmpty(filename: string, inDirectory: string, config: ICOBOLSettings): string {
        return "";
    }

    public getCOBOLSourceFormat(doc: ISourceHandler, config: ICOBOLSettings): ESourceFormat {
        return ESourceFormat.unknown;
    }

    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt): string | undefined {
        return undefined;
    }

    public setWorkspaceFolders(_folders: string[]) {
        //
    }

    public getWorkspaceFolders(): string[] {
        return [];
    }

    public isDirectory(possibleDirectory: string) : boolean {
        return false;
    }

    public getFileModTimeStamp(filename:string) : BigInt {
        return (BigInt)(0);
    }
    
    private fileSearchDirectory: string[] = [];
    
    public getCombinedCopyBookSearchPath(): string[] {
        return this.fileSearchDirectory;
    }

    public setCombinedCopyBookSearchPath(fileSearchDirectory: string[]):void {
        this.fileSearchDirectory = fileSearchDirectory;
    }
}
