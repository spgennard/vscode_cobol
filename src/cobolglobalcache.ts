/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

export class COBOLGlobalSymbolTable {
    public lastModifiedTime = 0;
    public callableSymbols = new Map<string, COBOLFileSymbol[]>();
    public isDirty = false;
    public sourceFilenameModified = new Map<string, number>();

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLGlobalSymbolTable {
        return Object.assign(new COBOLGlobalSymbolTable(), d);
    }
}

export class COBOLSymbolTable {
    public lastModifiedTime = 0;
    public fileName = "";

    public variableSymbols: Map<string, COBOLSymbol>;
    public labelSymbols: Map<string, COBOLSymbol>;

    public constructor() {
        this.variableSymbols = new Map<string, COBOLSymbol>();
        this.labelSymbols = new Map<string, COBOLSymbol>();
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLSymbolTable {
        return Object.assign(new COBOLSymbolTable(), d);
    }
}
export class COBOLFileSymbol {
    public filename: string;
    public lnum: number;

    public constructor(symbol?: string, lineNumber?: number) {
        this.filename = symbol === undefined ? "" : symbol;
        this.lnum = lineNumber === undefined ? 0 : lineNumber;
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLFileSymbol {
        return Object.assign(new COBOLFileSymbol(), d);
    }
}

export class COBOLSymbol {
    public symbol: string | undefined;
    public lnum: number | undefined;

    public constructor(symbol?: string, lineNumber?: number) {
        this.symbol = symbol;
        this.lnum = lineNumber;
    }

    static fromJSON(d: any): COBOLSymbol {
        return Object.assign(new COBOLSymbol(), d);
    }
}

export const globalSymbolFilename = "globalsymbols.sym";
export const fileSymbolFilename = "filesymbols.sym";

export const InMemorySymbolCache: Map<string, COBOLSymbolTable> = new Map<string, COBOLSymbolTable>();
export const InMemoryGlobalSymbolCache: COBOLGlobalSymbolTable = new COBOLGlobalSymbolTable();
