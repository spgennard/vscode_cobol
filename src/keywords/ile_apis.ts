/* eslint-disable @typescript-eslint/naming-convention */
import { IKnownApis } from "./cobolCallTargets";

export class ILE_DATE_TIME_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile4a1TOC.html"
    public name = "ILE CEE Date and Time APIs";
    public apis = new Map<string, string[]>([
        ["CEEDYWK", ["Calculate Day of Week from Lilian Date"]],
        ["CEEDAYS", ["Convert Date to Lilian Format, converts a string representing a date into a number representing the number of days since 14 October 1582."]],
        ["CEEISEC", ["Convert Integers to Seconds ,converts separate binary integers representing year, month, day, hour, minute, second, and millisecond to a number representing the number of seconds since 00,00,00 14 October 1582."]],
        ["CEEDATE", ["Convert Lilian Date to Character Format, formats a number representing a Lilian date."]],
        ["CEEDATM", ["Convert Seconds to Character Timestamp, formats a number representing the number of seconds since 00,00,00 14 October 1582."]],
        ["CEESECI", ["Convert Seconds to Integers, converts a number representing the number of seconds since 00,00,00 14 October 1582 to seven separate binary integers representing year, month, day, hour, minute, second, and millisecond."]],
        ["CEESECS", ["Convert Timestamp to Number of Seconds, converts a string representing a timestamp into a number representing the number of seconds since 00,00,00 14 October 1582."]],
        ["CEEGMT", ["Get Current Greenwich Mean Time, is an alias of CEEUTC."]],
        ["CEELOCT", ["Get Current Local Time, returns the current local time in three formats, Lilian date (the number of days since 14 October 1582, Lilian timestamp (the number of seconds since 00,00,00 14 October 1582, and Gregorian character string (in the form YYYYMMDDHHMISS999')."]],
        ["CEEUTCO", ["Get Offset from Universal Time Coordinated to Local Time, provides three values representing the current offset from Universal Time Coordinated (UTC) to local system time."]],
        ["CEEUTC", ["Get Universal Time Coordinated, returns the current Universal Time Coordinated as both a Lilian date and as the number of seconds since 00,00,00 14 October 1582."]],
        ["CEEQCEN", ["Query Century, queries the century within which 2-digit year values are assumed to lie."]],
        ["CEEFMDT", ["Return Default Date and Time Strings for Country or Region, returns the default date and time picture strings for the country or region specified in the country/region_code parameter."]],
        ["CEEFMDA", ["Return Default Date String for Country or Region, returns the default date picture string for the country or region specified in the country/region_code parameter."]],
        ["CEEFMTM", ["Return Default Time String for Country or Region, returns the default time picture string for the country or region specified in the country/region_code parameter."]],
        ["CEESCEN", ["Sets the century Window"]]
    ]);

    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_GROUP_FLOW_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile2a1TOC.html";
    public name = "Activation Group and Control Flow APIs";
    public apis = new Map<string, string[]>([
        ["CEE4ABN", ["Abnormal End abnormally ends the activation group containing the nearest control boundary."]],
        ["CEE4FCB", ["Find a Control Boundary searches the call stack for the nearest call stack entry that is a control boundary."]],
        ["CEETREC", ["Normal End is used to do a normal ending of the activation group containing the nearest control boundary."]],
        ["CEE4RAGE", ["Register Activation Group Exit Procedure is used to register procedures that are called when an activation group ends."]],
        ["CEE4RAGE2", ["Register Activation Group Exit Procedure 2 is used to register procedures that are called when an activation group ends (64 bit version of CEE4RAGE)."]],
        ["CEE4RAGEL", ["Register Activation Group Exit Procedure Last is used to register procedures that are called when an activation group ends."]],
        ["CEERTX", ["Register Call Stack Entry Termination User Exit Procedure registers a user-defined procedure that runs when the call stack entry for which it is registered is ended by anything other than a return to the caller."]],
        ["CEEUTX", ["Unregister Call Stack Entry Termination User Exit Procedure is used to unregister a user-defined procedure that was previously registered by the Register Call Stack Entry Termination User Exit Procedure (CEERTX) API."]]
    ]);

    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_COND_MAN_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile3a1TOC.html";
    public name = "Condition Management APIs";
    public apis = new Map<string, string[]>([
        ["CEENCOD", ["Construct a Condition Token is used to dynamically construct a 12-byte condition token."]],
        ["CEEDCOD", ["Decompose a Condition Token returns the individual elements of an existing condition token."]],
        ["CEE4HC", ["Handle a Condition handles a specified condition and, optionally, logs the condition."]],
        ["CEEMRCR", ["Move the Resume Cursor to a Return Point moves the resume cursor to a return point relative to the current handle cursor."]],
        ["CEEHDLR", ["Register a User-Written Condition Handler registers a user-written condition handler for the current call stack entry."]],
        ["CEEGPID", ["Retrieve ILE Version and Platform ID retrieves the ILE version ID and the platform (that is, operating system) ID. The IDs are those currently in use for processing the active condition."]],
        ["CEE4RIN", ["Return the Relative Invocation Number retrieves the relative invocation number of an invocation pointer, and returns it in rel_inv."]],
        ["CEESGL", ["Signal a Condition signals or resignals a condition to the ILE condition manager."]],
        ["CEEHDLU", ["Unregister a User-Written Condition Handler unregisters a user-written condition handler for the current call stack entry."]]
    ]);

    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_MATH_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile5a1TOC.html";
    public name = "Math APIs";
    public apis = new Map<string, string[]>([
        ["CEESxABS", ["Absolute Function"]],
        ["CEESxACS", ["Arccosine"]],
        ["CEESxASN", ["Arcsine"]],
        ["CEESxATN", ["Arctangent"]],
        ["CEESxAT2", ["Arctangent2"]],
        ["CEESxCJG", ["Conjugate of Complex"]],
        ["CEESxCOS", ["Cosine"]],
        ["CEESxCTN", ["Cotangent"]],
        ["CEESxERx", ["Error Function and Its Complement"]],
        ["CEESxEXP", ["Exponential Base e"]],
        ["CEESxXPx", ["Exponentiation"]],
        ["CEE4SxFAC", ["Factorial"]],
        ["CEESxDVD", ["Floating Complex Divide"]],
        ["CEESxMLT", ["Floating Complex Multiply"]],
        ["CEESxGMA", ["Gamma Function"]],
        ["CEESxATH", ["Hyperbolic Arctangent"]],
        ["CEESxCSH", ["Hyperbolic Cosine"]],
        ["CEESxSNH", ["Hyperbolic Sine"]],
        ["CEESxTNH", ["Hyperbolic Tangent"]],
        ["CEESxIMG", ["Imaginary Part of Complex"]],
        ["CEESxLGM", ["Log Gamma Function"]],
        ["CEESxLG1", ["Logarithm Base 10"]],
        ["CEESxLG2", ["Logarithm Base 2"]],
        ["CEESxLOG", ["Logarithm Base e"]],
        ["CEESxMOD", ["Modular Arithmetic"]],
        ["CEESxNJN", ["Nearest 64-Bit Integer"]],
        ["CEESxNIN", ["Nearest Integer"]],
        ["CEESxNWN", ["Nearest Whole Number"]],
        ["CEESxDIM", ["Positive Difference"]],
        ["CEESxSIN", ["Sine"]],
        ["CEESxSQT", ["Square Root"]],
        ["CEESxTAN", ["Tangent"]],
        ["CEESxSGN", ["Transfer of Sign"]],
        ["CEESxINT", ["Truncation"]],
        ["CEE4JNTS", ["Convert 64-Bit Integer to Character String"]],
        ["CEE4JSTN", ["Convert Character String to 64-Bit Integer"]]
    ]);

    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_MESSAGE_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile6a1TOC.html";
    public name = "Message Services APIs";
    public apis = new Map<string, string[]>([
        ["CEEMOUT", ["Dispatch a Message dispatches a message string."]],
        ["CEEMGET", ["Get a Message retrieves a message and stores it in a buffer."]],
        ["CEEMSG", ["Get, Format, and Dispatch a Message obtains, formats, and dispatches a message that corresponds to an input condition token."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_PROGRAM_OR_PROCEDURE_CALL_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile7a1TOC.html";
    public name = "Program or Procedure Call APIs";
    public apis = new Map<string, string[]>([
        ["CEE4GETRC", ["Get Program Return Code retrieves the program return code."]],
        ["CEEGSI", ["Get String Information retrieves string information about a parameter used in the call to this API."]],
        ["CEEDOD", ["Retrieve Operational Descriptor Information retrieves operational descriptor information about a parameter used in the call to this API."]],
        ["CEE4SETRC", ["Set Program Return Code sets the program return code."]],
        ["CEETSTA", ["Test for Omitted Argument is used to test for the presence or absence of an omissible argument."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}


export class ILE_STORAGE_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile8a1TOC.html";
    public name = "Storage Management APIs";
    public apis = new Map<string, string[]>([
        ["CEE4ALC", ["Allocation Strategy Type specifies the allocation strategy defined for the heap attributes."]],
        ["CEEFRST", ["Free Storage frees one previously allocated heap storage."]],
        ["CEEGTST", ["Get Heap Storage allocates storage within a heap."]],
        ["CEECZST", ["Reallocate Storage changes the size of previously allocated storage."]],
        ["CEECRHP", ["Create Heap creates a new heap."]],
        ["CEEDSHP", ["Discard Heap discards an existing heap."]],
        ["CEEMKHP", ["Mark Heap returns a token that can be used to identify heap storage to be freed by the CEERLHP API."]],
        ["CEERLHP", ["Release Heap frees all storage allocated in the heap since the mark was specified."]],
        ["CEE4DAS", ["Define Heap Allocation Strategy defines an allocation strategy that determines the attributes for a heap created with the CEECRHP API."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_STRING_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile9a1TOC.html";
    public name = "String Handling APIs";
    public apis = new Map<string, string[]>([
        ["CEESCLN", ["Scan String for Characters in a Set scans a string for one or more characters contained in a set of characters."]],
        ["CEESCPY", ["Copy String copies a source string to a result string."]],
        ["CEESCUP", ["Uppercase a String converts a string from its current case to uppercase."]],
        ["CEESLNG", ["Get String Length returns the length of a string."]],
        ["CEESSUB", ["Get Substring extracts a substring from a source string."]],
        ["CEESCAT", ["Concatenate Two Strings concatenates two strings and places the result into a destination string."]],
        ["CEESCMP", ["Compare Strings compares two strings and provides a result indicating their relative ordering."]],
        ["CEESINS", ["Insert Substring inserts a substring into a target string at a specified position."]],
        ["CEESRCH", ["Search String searches a source string for a specified pattern string."]],
        ["CEESPOS", ["Get String Position returns the position of the first occurrence of any character in a specified set within a string."]],
        ["CEESSET", ["Set Characters sets characters in a string to a specified character."]],
        ["CEESXLR", ["Translate String translates characters in a string from one set of characters to another."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_NLS_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/ile10a1TOC.html";
    public name = "National Language Support APIs";
    public apis = new Map<string, string[]>([
        ["CEENGTA", ["Get National Language Default Country or Region returns the default country/region setting for the national language currently in effect."]],
        ["CEENGTL", ["Get National Language Default Language returns the default language setting for the national language currently in effect."]],
        ["CEENGTT", ["Get National Language Default Date and Time returns the date and time picture strings for the national language currently in effect."]],
        ["CEENGFC", ["Get National Language Default Currency Symbol returns the default currency symbol for the national language currently in effect."]],
        ["CEENGFS", ["Get National Language Default Numeric Separators returns the default decimal separator and thousands separator for the national language currently in effect."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_PROGRAM_MGMT_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/pgmmgmtTOC.html";
    public name = "Program Management APIs";
    public apis = new Map<string, string[]>([
        ["QleActBndPgm", ["Activate Bound Program activates a service program and returns a handle that can be used by QleGetExp."]],
        ["QleGetExp", ["Get Export Pointer retrieves an export from an activated service program."]],
        ["QBNLPGMI", ["List ILE Program Information lists the module and service program information for a specified ILE program."]],
        ["QBNLSPGM", ["List Service Program Information lists the modules, service programs, and other information for a specified service program."]],
        ["QCLRPGMI", ["Clear Program Information is used to remove the module-level state information from a program."]],
        ["QBNRMVPM", ["Remove Program Module removes a module from an ILE program."]],
        ["QLICVTTP", ["Convert Type of Program converts a program from an original program model (OPM) program object to an ILE program."]],
        ["QleGetExpLong", ["Get Export Pointer (Long) retrieves an export from an activated service program, supporting long export names."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_DATA_AREA_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/QXXRTVDA.html";
    public name = "Data Area APIs";
    public apis = new Map<string, string[]>([
        ["QXXRTVDA", ["Retrieve Data Area retrieves the entire contents or a portion of a data area."]],
        ["QXXCHGDA", ["Change Data Area changes the entire contents or a portion of a data area."]],
        ["QWCRDTAA", ["Retrieve Data Area (QWCRDTAA) retrieves a data area and its description."]],
        ["QDBRTVFD", ["Retrieve File Description retrieves the file field description, including data areas associated with a file."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_SYSTEM_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/apisTOC.html";
    public name = "System APIs Commonly Called from COBOL";
    public apis = new Map<string, string[]>([
        ["QCMDEXC", ["Execute Command runs a single CL command from within a HLL program. The command string is passed as a parameter along with its length."]],
        ["QCAPCMD", ["Process Command processes a CL command string with options for syntax checking, command prompting, or execution."]],
        ["QCMDCHK", ["Check Command Syntax checks the syntax of a CL command string without running it."]],
        ["QUSRJOBI", ["Retrieve Job Information retrieves specific information about a job, such as job status, run priority, and other attributes."]],
        ["QUSROBJD", ["Retrieve Object Description retrieves various descriptions of a specific object on the system."]],
        ["QUSRMBRD", ["Retrieve Member Description retrieves descriptive information about a specific member of a database file."]],
        ["QUSLFLD", ["List Fields lists the fields in a record format of a file."]],
        ["QUSLJOB", ["List Job retrieves a list of jobs on the system based on specified selection criteria."]],
        ["QUILNGTX", ["Display Long Text displays a text string that may be up to 5000 characters in a pop-up window on the terminal."]],
        ["QMHSNDPM", ["Send Program Message sends a message to a call message queue or the external message queue."]],
        ["QMHRCVPM", ["Receive Program Message receives a message from a call message queue."]],
        ["QMHRMVPM", ["Remove Program Message removes a message from a call message queue."]],
        ["QUSCRTUS", ["Create User Space creates a user space in a specified library."]],
        ["QUSPTRUS", ["Retrieve Pointer to User Space retrieves a pointer to the beginning of a user space."]],
        ["QUSDLTUS", ["Delete User Space deletes a previously created user space."]],
        ["QUSRTVUS", ["Retrieve User Space retrieves the contents of a user space."]],
        ["QUSCHGUS", ["Change User Space changes the contents of a user space."]],
        ["QWCRTVCA", ["Retrieve Current Attributes retrieves current job attributes such as CCSID, date format, and time separator."]],
        ["QWCRSVAL", ["Retrieve System Value retrieves one or more system values."]],
        ["QLYSETS", ["Set Current Library sets the current library for the current thread."]],
        ["QDCXLATE", ["Convert Character Data converts a character string from one CCSID to another CCSID."]],
        ["QtqConvertTextString", ["Convert Text String converts a text string from one CCSID to another."]],
        ["Qp0zLprintf", ["Print to Job Log writes a formatted message to the job log."]],
        ["Qp0zUprintf", ["Print to Standard Output writes a formatted character string to stdout."]],
        ["QMHSNDM", ["Send Message to Message Queue sends an immediate or informational message to a specified message queue."]],
        ["QsyGetGroupFromGID", ["Get Group Name from GID retrieves the group profile name associated with a group identifier (GID)."]],
        ["QsyGetUserFromUID", ["Get User Name from UID retrieves the user profile name associated with a user identifier (UID)."]],
        ["QSZRTVPR", ["Retrieve Product Information retrieves the product information for a specified product."]],
        ["QWCRNETA", ["Retrieve Network Attributes retrieves the network attributes for the local system."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}

export class ILE_SQL_CLI_APIs implements IKnownApis {
    public url = "https://www.ibm.com/docs/en/i/7.6.0?topic=ssw_ibm_i_76/apis/cliTOC.html";
    public name = "SQL CLI (Call Level Interface) APIs";
    public apis = new Map<string, string[]>([
        ["SQLAllocConnect", ["Allocate Connection Handle allocates a connection handle for use with SQLConnect."]],
        ["SQLAllocEnv", ["Allocate Environment Handle allocates an environment handle for SQL CLI processing."]],
        ["SQLAllocHandle", ["Allocate Handle allocates an environment, connection, statement, or descriptor handle."]],
        ["SQLAllocStmt", ["Allocate Statement Handle allocates a statement handle for use with SQLPrepare and SQLExecDirect."]],
        ["SQLBindCol", ["Bind a Column binds a column to an application variable for data retrieval with SQLFetch."]],
        ["SQLBindParam", ["Bind a Parameter binds an application variable to a parameter marker in an SQL statement."]],
        ["SQLCancel", ["Cancel Statement cancels a statement that is running asynchronously or cancels a need-data situation."]],
        ["SQLCloseCursor", ["Close Cursor closes a cursor that was opened by SQLExecute or SQLExecDirect and discards pending results."]],
        ["SQLConnect", ["Connect to a Data Source connects to the specified data source using a connection handle."]],
        ["SQLDescribeCol", ["Describe Column returns the column name, type, size, decimal digits, and nullability for a result set column."]],
        ["SQLDisconnect", ["Disconnect from a Data Source disconnects from the specified data source."]],
        ["SQLExecDirect", ["Execute a Statement Directly prepares and executes an SQL statement in a single step."]],
        ["SQLExecute", ["Execute a Statement executes a previously prepared SQL statement."]],
        ["SQLFetch", ["Fetch Next Row fetches the next row of the result set and returns data for all bound columns."]],
        ["SQLFreeConnect", ["Free Connection Handle frees a connection handle and all associated resources."]],
        ["SQLFreeEnv", ["Free Environment Handle frees an environment handle and all associated resources."]],
        ["SQLFreeHandle", ["Free Handle frees an environment, connection, statement, or descriptor handle."]],
        ["SQLFreeStmt", ["Free Statement Handle frees a statement handle and all associated resources."]],
        ["SQLGetData", ["Get Data from a Column retrieves data for a single column in the result set."]],
        ["SQLNumResultCols", ["Number of Result Columns returns the number of columns in the result set."]],
        ["SQLPrepare", ["Prepare a Statement prepares an SQL string for execution."]],
        ["SQLRowCount", ["Row Count returns the number of rows affected by an UPDATE, INSERT, or DELETE statement."]]
    ]);
    public examples = new Map<string, string[]>();
    public snippets = new Map<string, string[]>();
}


