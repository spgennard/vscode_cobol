/* eslint-disable @typescript-eslint/no-explicit-any */
import * as path from 'path';
import * as crypto from 'crypto';


import { Hash } from "crypto";
import { InMemoryGlobalCachesHelper } from "./imemorycache";
import { COBOLToken, COBOLTokenStyle, ICOBOLSourceScanner, ICOBOLSourceScannerEvents } from "./cobolsourcescanner";
import { ICOBOLSettings } from './iconfiguration';
import { COBOLSymbol, COBOLSymbolTable } from './cobolglobalcache';

export class COBOLSymbolTableHelperDB implements ICOBOLSourceScannerEvents {

    private config: ICOBOLSettings;
    private qp: ICOBOLSourceScanner;
    private st: COBOLSymbolTable;
    private parse_copybooks_for_references: boolean;

    public constructor(config: ICOBOLSettings, qp: ICOBOLSourceScanner) {
        this.config = config;
        this.qp = qp;
        this.st = new COBOLSymbolTable();
        this.st.fileName = qp.filename;
        this.st.lastModifiedTime = qp.lastModifiedTime;
        this.parse_copybooks_for_references = config.parse_copybooks_for_references;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    start(qp: ICOBOLSourceScanner): void {
        throw new Error('Method not implemented.');
    }

    public processToken(token: COBOLToken): void {
        // hident token should not be placed in the symbol table, as they from a different file
        if (token.ignoreInOutlineView) {
            return;
        }

        switch (token.tokenType) {
            case COBOLTokenStyle.Constant:
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
            case COBOLTokenStyle.ImplicitProgramId:
                InMemoryGlobalCachesHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.ProgramId:
                InMemoryGlobalCachesHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.EntryPoint:
                InMemoryGlobalCachesHelper.addSymbol(this.st.fileName, token.tokenNameLower, token.startLine);
                break;
            case COBOLTokenStyle.InterfaceId:
                InMemoryGlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.EnumId:
                InMemoryGlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.ClassId:
                InMemoryGlobalCachesHelper.addClassSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
            case COBOLTokenStyle.MethodId:
                InMemoryGlobalCachesHelper.addMethodSymbol(this.st.fileName, token.tokenName, token.startLine);
                break;
        }
    }


    public finish():void {
            return;
    }

    private static getHashForFilename(filename: string) {
        const hash: Hash = crypto.createHash('sha256');
        hash.update(filename);
        return hash.digest('hex');
    }

    public static getSymbolTableGivenFile(cacheDirectory: string, nfilename: string): COBOLSymbolTable | undefined {
        const filename = path.normalize(nfilename);
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const fn: string = path.join(cacheDirectory, this.getHashForFilename(filename) + ".sym");
        return undefined;
    }
}
