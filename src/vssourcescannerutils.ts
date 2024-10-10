
import { FileType, Position, Range, Uri, workspace } from "vscode";
import { COBOLFileUtils } from "./fileutils";
import { ICOBOLSettings } from "./iconfiguration";
import { COBOLSourceScanner, COBOLToken } from "./cobolsourcescanner";

export class VSCOBOLSourceScannerTools {

      public static async howManyCopyBooksInDirectory(directory: string, settings: ICOBOLSettings): Promise<number> {
        const folder = Uri.file(directory);
        const entries = await workspace.fs.readDirectory(folder);
        let copyBookCount = 0;
        for (const [entry, fileType] of entries) {
            switch (fileType) {
                case FileType.File | FileType.SymbolicLink:
                case FileType.File:
                    if (COBOLFileUtils.isValidCopybookExtension(entry, settings)) {
                        copyBookCount++;
                    }
            }
        }

        return copyBookCount;
    }

    public static ignoreDirectory(partialName: string): boolean {
        // do not traverse into . directories
        if (partialName.startsWith(".")) {
            return true;
        }
        return false;
    }

    public static getExecTokem(sf: COBOLSourceScanner, position: Position): COBOLToken|undefined {
        for (const token of sf.execTokensInOrder) {
            const p1 = new Position(token.rangeStartLine, token.rangeStartColumn);
            const p2 = new Position(token.rangeEndLine, token.rangeEndColumn);
            const execPos = new Range(p1, p2);
            if (execPos.contains(position)) {
                return token;
            }
        }

        return undefined;
    }

    public static isPositionInEXEC(sf: COBOLSourceScanner, position: Position): boolean {
        return (VSCOBOLSourceScannerTools.getExecTokem(sf,position) !== undefined) ? true : false;
    }
}

