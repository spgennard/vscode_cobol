

type portSwap = [RegExp, string,string];
export class portResult {
    public readonly filename: string;
    public readonly linenum: number;
    public readonly replaceLine: string;
    public readonly message: string;

    constructor(filename:string, linenum: number, replaceLine:string, message:string) {
        this.filename = filename;
        this.linenum = linenum;
        this.replaceLine = replaceLine;
        this.message = message;
    }
}

export class SourcePorter {

    private _gnuDirectives: portSwap[] =
        [
            [/>>CALL-CONVENTION COBOL/, "$set DEFAULTCALLS(0)", "Change to DEFAULTCALLS(0)"],
            [/>>CALL-CONVENTION EXTERN/, "", "Remove"],
            [/>>CALL-CONVENTION STDCALL/, "", "Remove"],
            [/>>CALL-CONVENTION STATIC/, "", "Remove"]
        ];

    constructor() {

    }

    gnuPortActionRequired(filename: string, lineNumber: number, line: string): portResult|undefined {
        for (const gnuDirctive of this._gnuDirectives) {
            if (gnuDirctive[0].test(line)) {
                const repResult = line.replace(gnuDirctive[0], gnuDirctive[1]);
                return new portResult(filename, lineNumber, repResult, gnuDirctive[2]);
            }
        }

        return undefined;
    }
}