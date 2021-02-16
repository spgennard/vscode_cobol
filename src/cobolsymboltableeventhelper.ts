import { GlobalCachesHelper } from "./globalcachehelper";
import { COBOLToken, COBOLTokenStyle, ICOBOLSourceScanner, ICOBOLSourceScannerEvents } from "./cobolsourcescanner";
import { ICOBOLSettings } from './iconfiguration';
import { COBOLSymbol, COBOLSymbolTable } from './cobolglobalcache';
import { COBOLSymbolTableHelper } from './cobolglobalcache_file';
import { CacheDirectoryStrategy } from "./externalfeatures";
import { COBOLWorkspaceSymbolCacheHelper } from "./cobolworkspacecache";


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
            GlobalCachesHelper.addFilename(this.st?.fileName, this.st?.lastModifiedTime);
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
                COBOLWorkspaceSymbolCacheHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.EntryPoint:
                COBOLWorkspaceSymbolCacheHelper.addEntryPoint(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.InterfaceId:
                // GlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.EnumId:
                // GlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.ClassId:
                // GlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
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
