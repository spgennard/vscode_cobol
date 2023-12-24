

type portSwap = [RegExp, string, string];
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

export class SourcePorter {

    private _gnuDirectives: portSwap[] =
        [
            [new RegExp(">>CALL-CONVENTION COBOL", "i"), "$set DEFAULTCALLS(0)", "Change to DEFAULTCALLS(0)"],
            [new RegExp("(>>CALL-CONVENTION EXTERN)", "i"), "*> $1", "Comment out"],
            [new RegExp(">>CALL-CONVENTION STDCALL", "i"), "$set DEFAULTCALLS(74)", "Change to DEFAULTCALLS(74)"],
            [new RegExp(">>CALL-CONVENTION STATIC", "i"), "$set litlink", "Change to $set litlink"],
            [new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+FREE|FREE)", "i"), "$set sourceformat(free)", "Change to $set sourceformat(free)"],
            [new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+FIXED|FIXED)", "i"), "$set sourceformat(fixed)", "Change to $set sourceformat(fixed)"],
            [new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+VARIABLE|VARIABLE)", "i"), "$set sourceformat(variable)", "Change to $set sourceformat(variable)"],

        ];

    constructor() {

    }

    isDirectiveChangeRequired(filename: string, lineNumber: number, line: string): portResult | undefined {
        for (const gnuDirctive of this._gnuDirectives) {
            if (gnuDirctive[0].test(line)) {
                const repResult = line.replace(gnuDirctive[0], gnuDirctive[1]);
                return new portResult(filename, lineNumber, repResult, gnuDirctive[2]);
            }
        }

        return undefined;
    }
}