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
        
        // Screen Control APIs
        ["C$BEEP", ["Produces a beep or bell sound at the terminal."]],
        ["C$BELL", ["Produces a bell sound at the terminal (alias for C$BEEP)."]],
        ["C$CLEAR", ["Clears the screen and positions the cursor at the top-left corner."]],
        ["C$CURS", ["Controls cursor positioning and visibility on the screen."]],
        ["C$SCREEN", ["Provides screen manipulation and control functions."]],
        
        // Date and Time APIs
        ["C$CENTURY", ["Returns or sets the century window for 2-digit year processing."]],
        ["C$DATE", ["Returns the current system date in various formats."]],
        ["C$TIME", ["Returns the current system time in various formats."]],
        ["C$TIMEZONE", ["Gets or sets timezone information."]],
        
        // Environment and Configuration APIs
        ["C$GETENV", ["Retrieves the value of an environment variable."]],
        ["C$PUTENV", ["Sets or modifies an environment variable."]],
        ["C$CONFIG", ["Retrieves configuration information from ACUCOBOL-GT configuration file."]],
        ["C$GETCONF", ["Gets configuration settings for the runtime system."]],
        
        // Network and Socket APIs
        ["C$SOCKET", ["Creates and manages network sockets for TCP/IP communication."]],
        ["C$CONNECT", ["Establishes a connection to a remote socket."]],
        ["C$GETHOSTBYNAME", ["Resolves a hostname to an IP address."]],
        ["C$INET_ADDR", ["Converts an IP address string to binary format."]],
        
        // Advanced String Processing APIs
        ["C$TRIM", ["Removes leading and trailing spaces from a string."]],
        ["C$SPLIT", ["Splits a string into multiple parts based on a delimiter."]],
        ["C$REPLACE", ["Replaces occurrences of a substring within a string."]],
        ["C$ENCRYPT", ["Provides basic encryption capabilities for data protection."]],
        ["C$DECRYPT", ["Provides basic decryption capabilities for data protection."]],
        
        // Mathematical Functions
        ["C$SQRT", ["Calculates the square root of a number."]],
        ["C$SIN", ["Calculates the sine of an angle."]],
        ["C$COS", ["Calculates the cosine of an angle."]],
        ["C$TAN", ["Calculates the tangent of an angle."]],
        ["C$LOG", ["Calculates the natural logarithm of a number."]],
        ["C$EXP", ["Calculates the exponential function (e^x)."]],
        ["C$ABS", ["Returns the absolute value of a number."]],
        ["C$MOD", ["Returns the remainder of a division operation."]],
        
        // Process and Thread Management
        ["C$THREAD", ["Creates and manages threads for concurrent processing."]],
        ["C$MUTEX", ["Provides mutex (mutual exclusion) functionality for thread synchronization."]],
        ["C$SEMAPHORE", ["Manages semaphores for process synchronization."]],
        ["C$GETPID", ["Returns the current process ID."]],
        ["C$KILL", ["Sends a signal to terminate or control a process."]],
        
        // Advanced File Operations
        ["C$STAT", ["Retrieves detailed file status and attributes."]],
        ["C$LOCK", ["Provides file and record locking mechanisms."]],
        ["C$UNLOCK", ["Releases file and record locks."]],
        ["C$FLUSH", ["Forces buffered data to be written to disk."]],
        ["C$SEEK", ["Positions the file pointer for byte-stream operations."]],
        
        // Terminal and Console APIs
        ["C$GETCH", ["Reads a single character from the terminal without echo."]],
        ["C$KBHIT", ["Checks if a key has been pressed without waiting."]],
        ["C$CURSOR", ["Controls cursor position and attributes."]],
        ["C$COLOR", ["Sets text and background colors for terminal output."]],
        ["C$GETXY", ["Returns the current cursor position."]],
        ["C$GOTOXY", ["Moves the cursor to a specific screen position."]],
        
        // Windows-specific APIs
        ["WIN$PRINTER", ["Provides access to Windows printer functions."]],
        ["WIN$REGISTRY", ["Allows access to Windows registry operations."]],
        ["WIN$SHELL", ["Executes Windows shell commands and operations."]],
        ["WIN$MESSAGE", ["Displays Windows message boxes and dialog boxes."]],
        ["WIN$CLIPBOARD", ["Provides access to Windows clipboard operations."]],
        
        // File I/O and Memory APIs  
        ["I$IO", ["The I$IO routine provides an interface to the file handler. "]],
        ["M$ALLOC", ["Allocates a new area of dynamic memory. "]],
        ["M$COPY", ["Copies a region of memory from one location to another. "]],
        ["M$FILL", ["Sets a region of memory to a constant value."]],
        ["M$FREE", ["Frees a previously allocated piece of memory."]],
        ["M$GET", ["Retrieves data from an allocated memory block."]],
        ["M$PUT", ["Stores data in an allocated memory block. "]],
        ["M$SIZE", ["Returns the size of an allocated memory block."]],
        ["M$RESIZE", ["Resizes a previously allocated memory block."]],
        
        // File System Operations
        ["RENAME", ["Renames a file. "]],
        ["WIN$VERSION", ["Returns version information for Windows and Windows NT host platforms. "]]
    ]);

    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}