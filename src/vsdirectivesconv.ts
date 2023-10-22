 
 const _gridOptions:Map<string, string> = 
    new Map([
        [">>CALL-CONVENTION COBOL", ""],
        [">>CALL-CONVENTION EXTERN", ""],
        [">>CALL-CONVENTION STDCALL", ""],
        [">>CALL-CONVENTION STATIC", ""],
        
    ]);

export function getGrip()
{
    return _gridOptions;
}