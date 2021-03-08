import { InMemoryGlobalCacheHelper } from "./globalcachehelper";
import { COBOLToken, COBOLTokenStyle, ICOBOLSourceScanner, ICOBOLSourceScannerEvents } from "./cobolsourcescanner";
import { ICOBOLSettings } from './iconfiguration';
import { COBOLSymbol, COBOLSymbolTable } from './cobolglobalcache';
import { COBOLSymbolTableHelper } from './cobolglobalcache_file';
import { CacheDirectoryStrategy } from "./externalfeatures";
import { COBOLWorkspaceSymbolCacheHelper } from "./cobolworkspacecache";
import { COBSCANNER_ADDFILE, COBSCANNER_SENDCLASS, COBSCANNER_SENDENUM, COBSCANNER_SENDEP, COBSCANNER_SENDINTERFACE, COBSCANNER_SENDPRGID } from "./cobscannerdata";


export class COBOLSymbolTableEventHelper implements ICOBOLSourceScannerEvents {
    private qp: ICOBOLSourceScanner | undefined;
    private st: COBOLSymbolTable | undefined;
    private parse_copybooks_for_references: boolean;
    private config: ICOBOLSettings;

    public constructor(config: ICOBOLSettings) {
        this.config = config;
        this.parse_copybooks_for_references = config.parse_copybooks_for_references;
    }

    public start(qp: ICOBOLSourceScanner): void {
        this.qp = qp;
        this.st = new COBOLSymbolTable();
        this.st.fileName = qp.filename;
        this.st.lastModifiedTime = qp.lastModifiedTime;

        if (this.st?.fileName !== undefined && this.st.lastModifiedTime !== undefined) {
            InMemoryGlobalCacheHelper.addFilename(this.st?.fileName, qp.workspaceFile);

            if (process.send !== undefined) {
                process.send(`${COBSCANNER_ADDFILE},${this.st?.lastModifiedTime},${this.st?.fileName}`);
            }
        }

        COBOLWorkspaceSymbolCacheHelper.removeAllProgramEntryPoints(this.st?.fileName);
    }

    public processToken(token: COBOLToken): void {
        // hidden token should not be placed in the symbol table, as they from a different file
        if (token.ignoreInOutlineView) {
            return;
        }

        if (this.st === undefined) {
            return;
        }

        if (this.parse_copybooks_for_references === false) {
            switch (token.tokenType) {
                case COBOLTokenStyle.Constant:
                    if (this.parse_copybooks_for_references === false) {
                        this.st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.ConditionName:
                    if (this.parse_copybooks_for_references === false) {
                        this.st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.Variable:
                    if (this.parse_copybooks_for_references === false) {
                        this.st.variableSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.Paragraph:
                    if (this.parse_copybooks_for_references === false) {
                        this.st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
                case COBOLTokenStyle.Section:
                    if (this.parse_copybooks_for_references === false) {
                        this.st.labelSymbols.set(token.tokenNameLower, new COBOLSymbol(token.tokenName, token.startLine));
                    }
                    break;
            }
        }

        switch (token.tokenType) {
            case COBOLTokenStyle.ImplicitProgramId:
                COBOLWorkspaceSymbolCacheHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.ProgramId:
                if (process.send) {
                    process.send(`${COBSCANNER_SENDPRGID},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.EntryPoint:
                if (process.send) {
                    process.send(`${COBSCANNER_SENDEP},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.InterfaceId:
                if (process.send) {
                    process.send(`${COBSCANNER_SENDINTERFACE},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.EnumId:
                if (process.send) {
                    process.send(`${COBSCANNER_SENDENUM},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
            case COBOLTokenStyle.ClassId:
                if (process.send) {
                    process.send(`${COBSCANNER_SENDCLASS},${token.tokenName},${token.startLine},${this.st.fileName}`);
                }
                break;
                break;
            case COBOLTokenStyle.MethodId:
                // GlobalCachesHelper.addMethodSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
        }
    }

    public finish(): void {
        if (this.st !== undefined && this.qp !== undefined) {
            if (this.config.cache_metadata !== CacheDirectoryStrategy.Off &&
                this.parse_copybooks_for_references === false) {
                COBOLSymbolTableHelper.saveToFile(this.qp.cacheDirectory, this.st);
            }
        }
    }
}
