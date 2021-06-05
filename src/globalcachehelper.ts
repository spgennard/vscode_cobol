// eslint-disable-next-line @typescript-eslint/no-var-requires
import * as path from 'path';

import { COBOLGlobalSymbolTable, COBOLWorkspaceFile } from './cobolglobalcache';
import { COBOLFileUtils } from './fileutils';

export const InMemoryGlobalSymbolCache = new COBOLGlobalSymbolTable();

export class InMemoryGlobalCacheHelper {
    private static globalSymbolFilename = "globalsymbols.sym";

    public static (cacheDirectory: string): boolean {
        const fn: string = path.join(cacheDirectory, InMemoryGlobalCacheHelper.globalSymbolFilename);
        const fnStat = COBOLFileUtils.isFileT(fn);
        if (fnStat[0]) {
            return true;
        }

        return false;
    }

    public static getFilenameWithoutPath(fullPath: string): string {
        const lastSlash = fullPath.lastIndexOf(path.sep);
        if (lastSlash === -1) {
            return fullPath;
        }
        return fullPath.substr(1 + lastSlash);
    }

    public static addFilename(filename: string, wsf: COBOLWorkspaceFile): void {
        if (InMemoryGlobalSymbolCache.sourceFilenameModified.has(filename)) {
            InMemoryGlobalSymbolCache.sourceFilenameModified.delete(filename);
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, wsf);
        } else {
            InMemoryGlobalSymbolCache.sourceFilenameModified.set(filename, wsf);
        }
        InMemoryGlobalSymbolCache.isDirty = true;
    }

    public static getSourceFilenameModifiedTable(): string[] {

        const filenames:string[] = [];
        for(const [filename,lastModified] of InMemoryGlobalSymbolCache.sourceFilenameModified) {
            filenames.push(`${filename},${lastModified}`);
        }

        return filenames;
    }
}
