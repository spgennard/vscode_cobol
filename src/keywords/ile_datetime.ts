/* eslint-disable @typescript-eslint/naming-convention */
import { IKnownApis } from "./cobolCallTargets";

export class ILE_APIs implements IKnownApis
{
    public url = "https,//www.ibm.com/support/knowledgecenter/en/ssw_ibm_i_73/apis/ile4a1TOC.htm";
    public name = "ILE CEE Date and Time APIs";
    public apis = new Map<string, string>([
         [ "CEEDYWK", "Calculate Day of Week from Lilian Date"],
         [ "CEEDAYS", "Convert Date to Lilian Format, converts a string representing a date into a number representing the number of days since 14 October 1582." ],
         [ "CEEISEC", "Convert Integers to Seconds ,converts separate binary integers representing year, month, day, hour, minute, second, and millisecond to a number representing the number of seconds since 00,00,00 14 October 1582." ],
         [ "CEEDATE", "Convert Lilian Date to Character Format, formats a number representing a Lilian date." ],
         [ "CEEDATM", "Convert Seconds to Character Timestamp, formats a number representing the number of seconds since 00,00,00 14 October 1582." ],
         [ "CEESECI", "Convert Seconds to Integers, converts a number representing the number of seconds since 00,00,00 14 October 1582 to seven separate binary integers representing year, month, day, hour, minute, second, and millisecond." ],
         [ "CEESECS", "Convert Timestamp to Number of Seconds, converts a string representing a timestamp into a number representing the number of seconds since 00,00,00 14 October 1582." ],
         [ "CEEGMT", "Get Current Greenwich Mean Time, is an alias of CEEUTC." ],
         [ "CEELOCT", "Get Current Local Time, returns the current local time in three formats, Lilian date (the number of days since 14 October 1582, Lilian timestamp (the number of seconds since 00,00,00 14 October 1582, and Gregorian character string (in the form YYYYMMDDHHMISS999')." ],
         [ "CEEUTCO", "Get Offset from Universal Time Coordinated to Local Time, provides three values representing the current offset from Universal Time Coordinated (UTC) to local system time." ],
         [ "CEEUTC", "Get Universal Time Coordinated, returns the current Universal Time Coordinated as both a Lilian date and as the number of seconds since 00,00,00 14 October 1582." ],
         [ "CEEQCEN", "Query Century, queries the century within which 2-digit year values are assumed to lie." ],
         [ "CEEFMDT", "Return Default Date and Time Strings for Country or Region, returns the default date and time picture strings for the country or region specified in the country/region_code parameter." ],
         [ "CEEFMDA", "Return Default Date String for Country or Region, returns the default date picture string for the country or region specified in the country/region_code parameter." ],
         [ "CEEFMTM", "Return Default Time String for Country or Region, returns the default time picture string for the country or region specified in the country/region_code parameter." ],
         [ "CEESCEN", "Set Century, sets the century within which 2-digit year values are assumed to lie.]" ]
    ]);
}