/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import { VSLogger } from "./vslogger";
import { CopyBookCacheKey, ICopyBookCache, IExternalFeatures } from "./externalfeatures";
import { ICOBOLSettings } from "./iconfiguration";

import fs from "fs";
import { COBOLFileUtils } from "./fileutils";
import { VSWorkspaceFolders } from "./vscobolfolders";
import { FileType, Uri, debug, workspace } from "vscode";
import { IVSCOBOLSettings } from "./vsconfiguration";
import { ISourceHandler } from "./isourcehandler";
import { VSCOBOLFileUtils } from "./vsfileutils";


export class VSCopyBookCache implements ICopyBookCache {
    public readonly resolvedCopyBooksByKey = new Map<string, string>();

    public clear() {
        this.resolvedCopyBooksByKey.clear();
    }

    public get(cacheKey: CopyBookCacheKey): string | undefined {
        return this.resolvedCopyBooksByKey.get(cacheKey.toCacheKey());
    }
    
    public set(cacheKey: CopyBookCacheKey, filename: string) {
        this.resolvedCopyBooksByKey.set(cacheKey.toCacheKey(), filename);
    }

    public has(cacheKey: CopyBookCacheKey): boolean {
        return this.resolvedCopyBooksByKey.has(cacheKey.toCacheKey());
    }
}

class VSExternalFeaturesImpl implements IExternalFeatures {
    public getCopyBookCache(): ICopyBookCache {
        return new VSCopyBookCache();
    }

    public logMessage(message: string): void {
        VSLogger.logMessage(message);
    }

    public logException(message: string, ex: Error): void {
        VSLogger.logException(message, ex);
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public logTimedMessage(timeTaken: number, message: string, ...parameters: any[]): boolean {
        return VSLogger.logTimedMessage(timeTaken, message, parameters);
    }

    public performance_now(): number {
        return Date.now();
    }

    public expandLogicalCopyBookToFilenameOrEmpty(copyBookCache: ICopyBookCache, filename: string, inDirectory: string,source: ISourceHandler, config: IVSCOBOLSettings): string {
        return VSCOBOLFileUtils.expandLogicalCopyBookOrEmpty(copyBookCache, filename, inDirectory, source.getFilename(), config, this);
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    public getFullWorkspaceFilename(sdir: string, sdirMs: BigInt, config: ICOBOLSettings): string | undefined {
        return VSCOBOLFileUtils.getFullWorkspaceFilename(this, sdir, sdirMs, config);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public setWorkspaceFolders(_folders: string[]) {
        //
    }

    public getWorkspaceFolders(config: ICOBOLSettings): string[] {
        const folders = VSWorkspaceFolders.get(config);
        if (folders === undefined) {
            return [];
        }
        const foldersArray: string[] = [];
        for (const folder of folders) {
            foldersArray.push(folder.uri.fsPath);
        }

        return foldersArray;
    }

    public isDirectory(possibleDirectory: string): boolean {
        return COBOLFileUtils.isDirectory(possibleDirectory);
    }

    public isFile(possibleFilename: string): boolean {
        try {
            if (fs.existsSync(possibleFilename)) {
                // not on windows, do extra check for +x perms (protects exe & dirs)
                // if (!COBOLFileUtils.isWin32) {
                //     try {
                //         fs.accessSync(sdir, fs.constants.F_OK | fs.constants.X_OK);
                //         return false;
                //     }
                //     catch {
                //         return true;
                //     }
                // }

                return true;
            }
        }
        catch {
            return false;
        }
        return false;
    }


    // Short-lived cache of mtime lookups to absorb LSP bursts where many
    // providers (hover, completion, diagnostics, ...) hit getCachedObject
    // in the same tick and re-stat the same copybook set.
    // eslint-disable-next-line @typescript-eslint/ban-types
    private mtimeCache: Map<string, { ts: number; mtime: BigInt | undefined }> = new Map();
    private static readonly MTIME_TTL_MS = 250;

    // Diagnostic counter: total real fs.statSync calls issued by getFileModTimeStamp.
    // Useful for telemetry / perf investigations; cheap (single integer).
    public statSyncCount = 0;

    public invalidateMtime(filename?: string): void {
        if (filename === undefined) {
            this.mtimeCache.clear();
        } else {
            this.mtimeCache.delete(filename);
        }
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    public getFileModTimeStamp(filename: string): BigInt|undefined {
        const now = Date.now();
        const hit = this.mtimeCache.get(filename);
        if (hit !== undefined && (now - hit.ts) < VSExternalFeaturesImpl.MTIME_TTL_MS) {
            return hit.mtime;
        }
        // eslint-disable-next-line @typescript-eslint/ban-types
        let mtime: BigInt | undefined;
        try {
            // Note: virtual workspaces (vscode-vfs://, github1s, etc.) can't be
            // stat'd via node fs; callers that operate on URIs should use
            // isFileASync / workspace.fs.stat instead. Here we only cover the
            // on-disk path used by the synchronous scanner.
            this.statSyncCount++;
            const f = fs.statSync(filename, { bigint: true, throwIfNoEntry: false });
            mtime = f === undefined ? undefined : (BigInt)(f.mtimeMs);
        } catch (e) {
            mtime = undefined;
        }
        this.mtimeCache.set(filename, { ts: now, mtime });
        return mtime;
    }

    public getSourceTimeout(config: ICOBOLSettings): number {
        return config.scan_time_limit;
    }

    private URLSearchDirectory: string[] = [];

    public getURLCopyBookSearchPath(): string[] {
        return this.URLSearchDirectory;
    }

    public setURLCopyBookSearchPath(fileSearchDirectory: string[]): void {
        this.URLSearchDirectory = fileSearchDirectory;
    }

    public async isFileASync(possibleFilename: string): Promise<boolean> {
        try {
            const u = Uri.parse(possibleFilename);
            const sf = await workspace.fs.stat(u);
            switch (sf.type) {
                case FileType.File: return true;
                case FileType.File | FileType.SymbolicLink: return true;
            }

            return false;
        } catch (e) {
            return false;
        }
    }

    public isDebuggerActive(): boolean {
        if (debug.activeDebugSession === undefined) {
            return false;
        }

        return true;
    }

}

export const VSExternalFeatures = new VSExternalFeaturesImpl();
