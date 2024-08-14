/* eslint-disable @typescript-eslint/naming-convention */
import { IKnownApis } from "./cobolCallTargets";

export class ACU_COMMON_APIs implements IKnownApis {
    public url = "https://www.microfocus.com/documentation/visual-cobol/vc50all/VS2019/GUID-E30C24B6-3C74-48A4-AE39-C7A8F811C7A3.html";
    public name = "Common ACU APIs";
    public apis = new Map<string, string[]>([
        ["C$CALLEDBY", ["Returns the name of the caller of the currently running COBOL program or spaces if no caller exists or if the caller is unknown. "]],
        ["C$CALLERR", ["Retrieves the reason why the last CALL statement failed. For accurate information, it must be called before any other CALL statement is executed. "]],
        ["C$CHDIR", ["Changes the current working directory. "]],
        ["C$COPY", ["Creates a copy of an existing file. "]],
        ["C$DELETE", ["Deletes a file. "]],
        ["C$FILEINFO", ["Retrieves some operating system information about a given file. "]],
        ["C$GetLastFileOp", ["Retrieves the last COBOL I/O operation performed. "]],
        ["C$JUSTIFY", ["C$JUSTIFY performs left or right justification of data and centering of data"]],
        ["C$LIST-DIRECTORY", ["The C$LIST-DIRECTORY routine lists the contents of a selected directory.", "Each operating system has a unique method for performing this task.", "C$LIST-DIRECTORY provides a single method that will work for all operating systems. "]],
        ["C$MAKEDIR", ["Creates a new directory. "]],
        ["C$MEMCPY", ["Copies bytes between any two memory locations. "]],
        ["C$MYFILE", ["Returns the filename of the disk file containing the currently executing program."]],
        ["C$NARG", ["This routine returns the number of parameters passed to the current program."]],
        ["C$PARAMSIZE", ["This routine returns the number of bytes actually passed by the caller for a particular parameter."]],
        ["C$REGEXP",["This routine allows you to search strings using regular expressions. "]],
        ["C$RERR", ["Returns extended file status information for the last I/O statement. "]],
        ["C$RERRNAME", ["Returns the name of the last file used in an I/O statement. ","Use it in conjunction with C$RERR to diagnose file errors."]],
        ["C$RUN",["ACUCOBOL-GT for Windows supports an alternate method for running other programs.","This is through the library routine C$RUN.","This library routine works identically to the SYSTEM library routine, except that the calling program does not wait for the called program to finish.","Instead, both programs run in parallel. "]],
        ["C$SLEEP",["This routine causes the program to pause in a machine efficient fashion."]],
        ["C$SYSTEM", ["This routine combines the functionality of \"SYSTEM\" and \"C$RUN\".","It allows you to run other programs from inside a COBOL application in a variety of ways. "]],
        ["C$TOUPPER", ["These routines translate text to upper-case"]],
        ["C$TOLOWER",["These routines translate text to lower-case"]],
        ["I$IO", ["The I$IO routine provides an interface to the file handler. "]],
        ["M$ALLOC", ["Allocates a new area of dynamic memory. "]],
        ["M$COPY", ["Copies a region of memory from one location to another. "]],
        ["M$FILL", ["Sets a region of memory to a constant value."]],
        ["M$FREE", ["Frees a previously allocated piece of memory."]],
        ["M$GET", ["Retrieves data from an allocated memory block."]],
        ["M$PUT", ["Stores data in an allocated memory block. "]],
        ["RENAME", ["Renames a file. "]],
        ["WIN$VERSION", ["Returns version information for Windows and Windows NT host platforms. "]]
    ]);

    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}