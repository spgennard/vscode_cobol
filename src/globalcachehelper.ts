// eslint-disable-next-line @typescript-eslint/no-var-requires
import * as fs from 'fs';
import * as path from 'path';

import { COBOLGlobalSymbolTable, COBOLWorkspaceFile } from './cobolglobalcache';

export const InMemoryGlobalSymbolCache = new COBOLGlobalSymbolTable();

export class InMemoryGlobalCacheHelper {
    private static globalSymbolFilename = "globalsymbols.sym";

    private static isFileT(sdir: string): [boolean, fs.Stats | undefined] {
        try {
            if (fs.existsSync(sdir)) {
                const f = fs.statSync(sdir);
                if (f && f.isFile()) {
                    return [true, f];
                }
            }
        }
        catch {
            return [false, undefined];
        }
        return [false, undefined];
    }

    public static (cacheDirectory: string): boolean {
        const fn: string = path.join(cacheDirectory, InMemoryGlobalCacheHelper.globalSymbolFilename);
        const fnStat = InMemoryGlobalCacheHelper.isFileT(fn);
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
    }

    public static getSourceFilenameModifiedTable(): string[] {

        const filenames:string[] = [];
        for(const [filename,lastModified] of InMemoryGlobalSymbolCache.sourceFilenameModified) {
            filenames.push(`${filename},${lastModified}`);
        }

        return filenames;
    }
}
