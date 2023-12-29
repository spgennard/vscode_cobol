

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

type portSwap = [number, RegExp, string, string];

export class SourcePorter {
    private isActive = true;
    private _gnuDirectives: portSwap[] =
        [
            [100, new RegExp(">>CALL-CONVENTION COBOL", "i"), "$set defaultcalls(0)", "Change to defaultcalls(0)"],
            [100, new RegExp("(>>CALL-CONVENTION EXTERN)", "i"), "*> $1", "Comment out"],
            [100, new RegExp(">>CALL-CONVENTION STDCALL", "i"), "$set defaultcalls(74)", "Change to defaultcalls(74)"],
            [100, new RegExp(">>CALL-CONVENTION STATIC", "i"), "$set litlink", "Change to $set litlink"],
            [100, new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+FREE|FREE)", "i"), "$set sourceformat(free)", "Change to $set sourceformat(free)"],
            [100, new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+FIXED|FIXED)", "i"), "$set sourceformat(fixed)", "Change to $set sourceformat(fixed)"],
            [100, new RegExp(">>SOURCE\\s+FORMAT\\s+(IS\\s+VARIABLE|VARIABLE)", "i"), "$set sourceformat(variable)", "Change to $set sourceformat(variable)"]
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
        let acitveCount = this._gnuDirectives.length;
        this.isActive = true;
        for (const gnuDirctive of this._gnuDirectives) {
            if (gnuDirctive[0] > lineNumber) {
                acitveCount--;
            }
            if (gnuDirctive[1].test(line)) {
                const repResult = line.replace(gnuDirctive[1], gnuDirctive[2]);
                return new portResult(filename, lineNumber, repResult, gnuDirctive[3]);
            }
        }

        this.isActive = acitveCount !== 0;

        return undefined;
    }
}