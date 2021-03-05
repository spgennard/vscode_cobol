// eslint-disable-next-line @typescript-eslint/no-var-requires
import path from 'path';
import { COBOLGlobalSymbolTable } from './cobolglobalcache';

export const InMemoryGlobalSymbolCache = new COBOLGlobalSymbolTable();

export class InMemoryGlobalCacheHelper {


    public static getFilenameWithoutPath(fullPath: string): string {
        const lastSlash = fullPath.lastIndexOf(path.sep);
        if (lastSlash === -1) {
            return fullPath;
        }
        return fullPath.substr(1 + lastSlash);
    }

    public static addFilename(filename: string, lastModified: BigInt): void {
        if (InMemoryGlobalSymbolCache.sourceFilenameModified.has(filename)) {
            InMemoryGlobalSymbolCache.sourceFilenameModified.delete(filename);
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, lastModified);
        } else {
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, lastModified);
        }
    }

    public static getSourceFilenameModifiedTable(): string[] {

        const filenames:string[] = [];
        for(const [filename,lastModified] of InMemoryGlobalSymbolCache.sourceFilenameModified) {
            filenames.push(`${filename},${lastModified}`);
        }

        return filenames;
    }
}
