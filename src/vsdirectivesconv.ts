
type portSwap = [RegExp, string];
export type portResult = [boolean, string];

const _gnuDirectives : portSwap[] = 
    [
        [/>>CALL-CONVENTION COBOL/, ""],
        [/>>CALL-CONVENTION EXTERN/, ""],
        [/>>CALL-CONVENTION STDCALL/, ""],
        [/>>CALL-CONVENTION STATIC/, ""]
    ];

export function gnuPortActionRequired(line: string) : portResult {
    for(const gnuDirctive of _gnuDirectives)
    {
        if (gnuDirctive[0].test(line))
        {
            const repResult = line.replace(gnuDirctive[0],gnuDirctive[1]);
            return [ true, repResult];
        }
    }

    return [false, ""];
}