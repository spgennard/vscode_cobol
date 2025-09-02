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


