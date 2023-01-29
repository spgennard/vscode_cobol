/* eslint-disable @typescript-eslint/ban-types */
/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

export class COBOLWorkspaceFile {
    lastModifiedTime:BigInt;
    workspaceFilename: string;

    constructor(lastModifiedTime:BigInt, workspaceFilename: string) {
        this.lastModifiedTime = lastModifiedTime;
        this.workspaceFilename = workspaceFilename;
    }
}

export class COBOLGlobalSymbolTable {
    public defaultCallableSymbols = new Map<string,string>();
    public defaultCopybooks = new Map<string,string>();

    public callableSymbols = new Map<string, COBOLFileSymbol[]>();
    public entryPoints = new Map<string, COBOLFileSymbol[]>();

    public types = new Map<string, COBOLFileSymbol[]>();
    public interfaces = new Map<string, COBOLFileSymbol[]>();
    public enums =  new Map<string, COBOLFileSymbol[]>();

    public knownCopybooks = new Map<string, string>();
    
    public isDirty = false;
    public sourceFilenameModified = new Map<string, COBOLWorkspaceFile>();

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLGlobalSymbolTable {
        return Object.assign(new COBOLGlobalSymbolTable(), d);
    }
}

export class COBOLSymbolTable {
    public lastModifiedTime:BigInt = BigInt("0");
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

export class COBOLFileAndColumnSymbol {
    public filename: string;
    public lnum: number;
    public startColumn: number;

    public constructor(symbol?: string, lineNumber?: number, startColumn?: number) {
        this.filename = symbol === undefined ? "" : symbol;
        this.lnum = lineNumber === undefined ? 0 : lineNumber;
        this.startColumn = startColumn === undefined ? 0 : startColumn;
    }

    // eslint-disable-next-line @typescript-eslint/ban-types
    static fromJSON(d: Object): COBOLFileSymbol {
        return Object.assign(new COBOLFileSymbol(), d);
    }
}