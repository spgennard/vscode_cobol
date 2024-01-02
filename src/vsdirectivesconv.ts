

export class portResult {
    public readonly filename: string;
    public readonly linenum: number;
    public readonly replaceLine: string;
    public readonly message: string;

    constructor(filename: string, linenum: number, replaceLine: string, message: string) {
        this.filename = filename;
        this.linenum = linenum;
        this.replaceLine = replaceLine;
        this.message = message;
    }
}

class portSwap {
    lastNumber: number;
    search: RegExp;
    replaceLine: string;
    message: string;

    constructor(lastLine: number, search: RegExp, replaceLine: string, message: string) {
        this.lastNumber = lastLine;
        this.search = search;
        this.replaceLine = replaceLine;
        this.message = message;
    }
}

export class SourcePorter {
    private isActive = true;
    private _portsDirectives: portSwap[] =
        [
            new portSwap(100, new RegExp(">>CALL-CONVENTION COBOL", "i"), "$set defaultcalls(0)", "Change to defaultcalls(0)"),
            new portSwap(100, new RegExp("(>>CALL-CONVENTION EXTERN)", "i"), "*> $1", "Comment out"),
            new portSwap(100, new RegExp(">>CALL-CONVENTION STDCALL", "i"), "$set defaultcalls(74)", "Change to defaultcalls(74)"),
            new portSwap(100, new RegExp(">>CALL-CONVENTION STATIC", "i"), "$set litlink", "Change to $set litlink"),
            new portSwap(100, new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+FREE|FREE)", "i"), "$set sourceformat(free)", "Change to $set sourceformat(free)"),
            new portSwap(100, new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+FIXED|FIXED)", "i"), "$set sourceformat(fixed)", "Change to $set sourceformat(fixed)"),
            new portSwap(100, new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+VARIABLE|VARIABLE)", "i"), "$set sourceformat(variable)", "Change to $set sourceformat(variable)"),
            new portSwap(Number.MAX_VALUE, new RegExp("FUNCTION\\s+SUBSTITUTE","i"),"","Re-write to use 'INSPECT REPLACING' or custom/search replace")
        ];

    constructor() {

    }

    public isDirectiveChangeRequired(filename: string, lineNumber: number, line: string): portResult | undefined {
        /* always active on the first five lines */
        if (lineNumber <= 5) {
            this.isActive = true;
        }

        /* if not active leave */
        if (!this.isActive) {
            return undefined;
        }

        /* keep an account of active regex's */
        let acitveCount = this._portsDirectives.length;
        this.isActive = true;
        for (const portLine of this._portsDirectives) {
            if (portLine.lastNumber > lineNumber) {
                acitveCount--;
            }
            if (portLine.search.test(line)) {
                const repResult = portLine.replaceLine.length !== 0 ? line.replace(portLine.search, portLine.replaceLine) : "";
                return new portResult(filename, lineNumber, repResult, portLine.message);
            }
        }

        this.isActive = acitveCount !== 0;

        return undefined;
    }
}