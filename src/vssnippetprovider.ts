import { CancellationToken, CompletionContext, CompletionItem, CompletionItemKind, CompletionItemProvider, CompletionList, MarkdownString, Position, ProviderResult, Range, SnippetString, TextDocument, TextLine } from "vscode";
import { COBOLSourceScanner, SourceScannerUtils } from "./cobolsourcescanner";
import { VSCOBOLUtils, FoldAction } from "./vscobolutils";
import { ExtensionDefaults } from "./extensionDefaults";
import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";
import { KnownAPIs } from "./keywords/cobolCallTargets";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSCustomIntelliseRules } from "./vscustomrules";
import { VSExternalFeatures } from "./vsexternalfeatures";

const jsonCRLF = "\r\n";

interface ISimpleSnippet {
    prefix: string;
    label: string;
    body: string[];
    description: string;
    detail?: string;
    scope: string;
    triggerIntellisense?: boolean;
    alwaysUpperCase?: boolean;
}

const simpleSnippets: ISimpleSnippet[] = [
    {
        "prefix": "add",
        "label": "add ⦃⦄ to ⦃⦄ giving ⦃⦄",
        "body": [
            "add ${1:a} to ${2:b} giving ${3:c}",
            "$0"
        ],
        "description": "Add {identifier-1|literal-1} to {identifier-2} giving {identifier-3}",
        "scope": "cobol"
    },
    {
        "prefix": "accept",
        "label": "accept ⦃⦄",
        "body": [
            "accept ${1:variable}",
            "$0"
        ],
        "scope": "cobol",
        "description": "Accept identifier-1"
    },
    {
        "prefix": "accept",
        "label": "accept ⦃⦄ from ⦃date/time/..⦄",
        "body": [
            "accept ${1:variable} ${2|from date,from day,from day-of-week,from time|}",
            "$0"
        ],
        "description": "Accept identifier-1 from {date/day/week/time}",
        "scope": "cobol"
    },
    {
        "prefix": "compute equal",
        "label": "compute ⦃⦄ = ⦃⦄..",
        "body": ["compute ${1:id} equal ${2:expression}"],
        "description": "compute id equal expression",
        "scope": "cobol"
    },
    {
        "prefix": "converting",
        "label": "converting ⦃⦄",
        "body": ["converting ${1:item}"],
        "description": "converting item",
        "scope": "cobol"
    },
    {
        "prefix": "corr",
        "label": "corr ⦃⦄",
        "body": ["corr ${1:id}"],
        "description": "corr id",
        "scope": "cobol"
    },
    {
        "prefix": "count in",
        "label": "count in ⦃⦄",
        "body": ["count in ${1:id}"],
        "description": "count in id",
        "scope": "cobol"
    },
    {
        "prefix": "copy",
        "label": "copy ⦃⦄ replacing ==⦃⦄== by ==⦃⦄==",
        "body": [
            "copy \"${1:subprog.cpy}\"",
            "    replacing ==${2:ABC}== by ==${3:DEF}==.",
        ],
        "description": "Copy {text-name-1|literal-1} replacing =={}== by =={}==",
        "scope": "cobol"
    },
    {
        "prefix": "date",
        "label": "date",
        "body": ["date ${1:yyyymmdd}"],
        "description": "date yyyymmdd",
        "scope": "cobol"
    },
    {
        "prefix": "day",
        "label": "day",
        "body": ["day ${1:yyyyddd}"],
        "description": "day yyyyddd",
        "scope": "cobol"
    },
    {
        "prefix": "delimited by",
        "label": "delimited by ⦃⦄",
        "body": ["delimited by ${1:item}"],
        "description": "delimited by item",
        "scope": "cobol"
    },
    {
        "prefix": "delimiter",
        "label": "delimited by ⦃⦄",
        "body": ["delimiter ${1:item}"],
        "description": "delimited by",
        "scope": "cobol"
    },
    {
        "prefix": "divide",
        "label": "divide ⦃⦄ by ⦃⦄ giving ⦃⦄ [remainder ⦃⦄]",
        "body": [
            "divide ${1:a} by ${2:b} giving ${3:c} ${4:remainder ${5:d}}"
        ],
        "description": "divide a by b giving [remainder}",
        "scope": "cobol"
    },
    {
        "prefix": "entry",
        "label": "entry ⦃literlal-1⦄",
        "body": [
            "${1:AlternateEP}-ep section.",
            "entry \"$1\".",
            "    $0\t",
            "    goback.",
            ""
        ],
        "description": "entry ⦃literlal-1⦄",
        "scope": "cobol"
    },
    {
        "prefix": "evaluate",
        "label": "evaluate ⦃⦄ .. when..",
        "body": [
            "evaluate ${1:Expression}",
            "    when ${2:Phrase}",
            "       $0",
            "    when other",
            "       continue",
            "end-evaluate",
            ""
        ],
        "description": "evaluate ⦃⦄.. when.. when continue",
        "scope": "cobol"
    },
    {
        "prefix": "if",
        "label": "if ⦃⦄ end-if",
        "body": [
            "if ${1:condition}",
            "   $0",
            "end-if"
        ],
        "description": "if ⦃⦄ end-if",
        "scope": "cobol"
    },
    {
        "prefix": "if",
        "label": "if ⦃⦄ else ⦃⦄  end-if",
        "body": [
            "if ${1:condition}",
            "   $2",
            "else",
            "   $0",
            "end-if"
        ],
        "description": "if ⦃⦄ else ⦃⦄  end-if",
        "scope": "cobol"
    },
    {
        "prefix": "delimited",
        "label": "delimited by size",
        "body": [
            "delimited by size"
        ],
        "description": "delimited by size (string)",
        "scope": "cobol"
    },
    {
        "prefix": "delimited",
        "label": "delimited by space",
        "body": [
            "delimited by space"
        ],
        "description": "delimited by size (string)",
        "scope": "cobol"
    }, {
        "prefix": "inspect",
        "label": "inspect converting",
        "body": [
            "inspect $1 converting '${2:ABCDEFGHIJKLMNOPQRSTUVWXYZ}' to '${3:abcdefghijklmnopqrstuvwxyz}'"
        ],
        "description": "inspect converting ⦃⦄ to ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "inspect",
        "label": "inspect replacing",
        "body": [
            "inspect $1 replacing all ${2|spaces,zero|} by '${3}'"
        ],
        "description": "inspect ⦃⦄ replacing [⦃⦄|spaces,zero] by ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "inspect",
        "label": "inspect tallying",
        "body": [
            "move 0 to ${2:counter}",
            "inspect ${1:source} tallying ${2:counter} for ${3|all spaces,all \"abc\",characters|}"
        ],
        "description": "Inspect ⦃⦄ tallying ⦃⦄ for [⦃⦄|all spaces,all ⦃⦄]]..",
        "scope": "cobol"
    },
    {
        "prefix": "json generate",
        "label": "json generate from",
        "body": ["Json generate ${1:id} from ${2:id}"],
        "description": "json generate ⦃⦄ from ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "json parse",
        "label": "json parse into",
        "body": ["json parse ${1:id} into ${2:id}"],
        "description": "json parse ⦃⦄ into ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "linkage",
        "label": "linkage section.",
        "body": [
            "linkage section.",
            "$0"
        ],
        "description": "linkage section.",
        "scope": "cobol"
    },
    {
        "prefix": "local-storage",
        "label": "local-storage section.",
        "body": [
            "local-storage section.",
            "$0"
        ],
        "description": "local-storage section.",
        "scope": "cobol"
    },
    {
        "prefix": "procedure",
        "label": "procedure division",
        "body": [
            "procedure division.",
            "$0"
        ],
        "description": "procedure division",
        "scope": "cobol"
    },
    {
        "prefix": "program-id",
        "label": "program-id ⦃⦄",
        "body": [
            "program-id. ${1:${TM_FILENAME/(.*)\\..+$/$1/}}.",
            "$0"
        ],
        "description": "program-id. literal-1",
        "scope": "cobol"
    },
    {
        "prefix": "local-storage",
        "label": "local-storage section",
        "description": "local-storage section",
        "body": [
            "local-storage section.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "xml generate",
        "label": "xml generate",
        "body": ["xml generate ${1:item}"],
        "description": "xml generate ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "xml parse",
        "label": "xml parse",
        "body": ["xml parse ${1:item}"],
        "description": "xml parse ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "working-storage",
        "label": "working-storage section",
        "description": "working-storage section",
        "body": [
            "working-storage section.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "data",
        "label": "data division",
        "description": "data division",
        "body": [
            "data division.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "environment",
        "label": "environment division",
        "description": "environment division",
        "body": [
            "environment division.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "identification",
        "label": "identification division",
        "description": "identification division",
        "body": [
            "identification division.",
            "$0"
        ],
        "scope": "cobol"
    },
    {
        "prefix": "cancel",
        "label": "cancel",
        "body": [
            "cancel \"$1\"",
            "$0"
        ],
        "description": "CANCEL literal",
        "scope": "cobol"
    },
    {
        "prefix": "call",
        "label": "call literal",
        "body": [
            "call \"$1\" using",
            "    by ${2|value,reference,content|} ${3:identifer}",
            "    returning ${4:return-code}",
            "end-call",
            "$0"
        ],
        "description": "CALL literal",
        "scope": "cobol"
    },
    {
        "prefix": "exit",
        "label": "exit program",
        "body": [
            "exit program returning ${1:item}"
        ],
        "description": "exit program returning ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "exit",
        "label": "exit section",
        "body": [
            "exit section"
        ],
        "description": "exit section",
        "scope": "cobol"
    },
    {
        "prefix": "string",
        "label": "string",
        "body": [
            "string ${1:item1} delimited by size",
            "       ${2:item2} delimited by size",
            "       into ${3:result}",
            "end-string",
            "$0"
        ],
        "description": "string delimited by size",
        "scope": "cobol"
    },
    {
        "prefix": "subtract",
        "label": "subtract",
        "body": [
            "subtract ${1:a} from ${2:b} giving ${3:c}"
        ],
        "description": "subtract ⦃⦄ from ⦃⦄ giving ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "multiply",
        "label": "multiply",
        "body": [
            "multiply ${1:a} by ${2:b} giving ${3:c}"
        ],
        "description": "Multiply ⦃⦄ by ⦃⦄ giving ⦃⦄",
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform p1 through p2 ",
        "body": [
            "perform ${1:procedure-name-1} ${2|thru,through|} ${3:procedure-name-2}"
        ],
        "description": "perform p1 through p2",
        "triggerIntellisense": true,
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform paragraph ⦃⦄ times",
        "body": [
            "perform ${1:paragraph-name} ${2:value-1} times"
        ],
        "description": "perform paragraph ⦃⦄ times",
        "triggerIntellisense": true,
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform procedure-name-1 [thru procedure-name-2] with test until..",
        "body": [
            "perform ${1:procedure-name-1} ${3|thru,though|} ${4:procedure-name}",
            "\t${5|with test before,with test after|}",
            "\tuntil ${6|condition,exit|}",
            "end-perform"
        ],
        "description": "perform procedure-name-1 [thru|through] procedure-name-2 with test until [condition|exit]",
        "triggerIntellisense": true,
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform procedure-name with test until..",
        "body": [
            "perform ${1:procedure-name-1}",
            "\t${2|with test before,with test after|}",
            "\tuntil ${3|condition,exit|}",
            "end-perform"
        ],
        "description": "perform procedure-name with test until..",
        "triggerIntellisense": true,
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform paragraph-name",
        "body": [
            "perform ${1:paragraph-name}",
            "${0}"
        ],
        "description": "perform paragraph-name",
        "triggerIntellisense": true,
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform .. end-perform",
        "body": [
            "perform ${1:paragraph-name}",
            "\t${0}",
            "end-perform"
        ],
        "description": "perform <block> end-perform",
        "triggerIntellisense": true,
        "scope": "cobol"
    },
    {
        "prefix": "perform",
        "label": "perform paragraph varying",
        "body": [
            "perform ${1:paragraph-name} varying ${2:field-1}",
            " from ${3:value-1} by ${4:value-2}",
            " until ${5:condition}",
            "end-perform",
            "$0"
        ],
        "description": "perform paragraph varying",
        "triggerIntellisense": true,
        "scope": "cobol"
    },
    {
        "prefix": "end",
        "label": "end program",
        "body": [
            "end program ${1:${TM_FILENAME/(.*)\\..+$/$1/}}."
        ],
        "description": "end program literal.",
        "scope": "cobol"
    },
    {
        "prefix": "display",
        "label": "display",
        "body": [
            "display \"$0\""
        ],
        "description": "display literal",
        "scope": "cobol"
    },
    {
        "prefix": "exhibit",
        "label": "exhibit named variable",
        "body": [
            "exhibit named ${1:variable}",
            "$0"
        ],
        "description": "exhibit named variable",
        "scope": "cobol"
    },
    {
        "prefix": "dfhresp",
        "label": "DFHRESP(responseCode)",
        "body": [
            "DFHRESP(${1|NORMAL,ACTIVITYBUSY,ACTIVITYERR,ALLOCERR,CBIDERR,CHANGED,CONTAINERERR,DISABLED,DSNNOTFOUND,DSSTAT,DUPKEY,DUPREC,END,ENDDATA,ENDFILE,ENDINPT,ENQBUSY,ENVDEFERR,EOC,EODS,EOF,ERROR,EVENTERR,EXPIRED,FILENOTFOUND,FUNCERR,IGREQCD,IGREQID,ILLOGIC,INBFMH,INVERRTERM,INVEXITREQ,INVLDC,INVMPSZ,INVPARTN,INVPARTNSET,INVREQ,IOERR,ISCINVREQ,ITEMERR,JIDERR,LENGERR,LINKABEND,LOADING,LOCKED,MAPFAIL,MODELIDERR,NETNAMERR,NODEIDERR,NOJBUFSP,NONVAL,NOPASSBKRD,NOPASSBKWR,NOSPACE,NOSPOOL,NOSTART,NOSTG,NOTALLOC,NOTAUTH,NOTFINISHED,NOTFND,NOTOPEN,OPENERR,OUTDESCRERR,OVERFLOW,PARTNERIDERR,PARTNFAIL,PGMIDERR,POOLERR,PROCESSBUSY,PROCESSERR,PROFILEIDERR,QBUSY,QIDERR,QZERO,RDATT,RECORDBUSY,RETPAGE,ROLLEDBACK,RTEFAIL,RTESOME,SELNERR,SESSBUSY,SESSIONERR,SIGNAL,SPOLBUSY,SPOLERR,STRELERR,SUPPRESSED,SYMBOLERR,SYSBUSY,SYSIDERR,TASKIDERR,TCIDERR,TEMPLATERR,TERMERR,TERMIDERR,TIMERERR,TOKENERR,TRANSIDERR,TSIOERR,UNEXPIN,UOWLNOTFOUND,UOWNOTFOUND,USERIDERR,WRBRK|})$0"
        ],
        "alwaysUpperCase": true,
        "description": "CICS DFHRESP Reponse code",
        "scope": "cobol"
    },
    {
        "prefix": "by",
        "label": "by reference",
        "body": [
            "by reference $0"
        ],
        "description": "by reference",
        "scope": "cobol"
    },
    {
        "prefix": "by",
        "label": "by value",
        "body": [
            "by value $0"
        ],
        "description": "by value",
        "scope": "cobol"
    },
    {
        "prefix": "by",
        "label": "by content",
        "body": [
            "by content $0"
        ],
        "description": "by content",
        "scope": "cobol"
    }
];

const functionSnippets: ISimpleSnippet[] = [
    {
        "prefix": "abs",
        "label": "function abs",
        "body": [
            "function abs(${1:number})$0"
        ],
        "description": "function absolute value of number",
        "scope": "cobol"
    },
    {
        "prefix": "acos",
        "label": "function acos",
        "body": [
            "function acos(${1:cosine})$0"
        ],
        "description": "function trigonometric arc-cosine, or inverse cosine, of cosine",
        "scope": "cobol"
    },
    {
        "prefix": "annuity",
        "label": "function annuity",
        "body": [
            "function annuity(${1:interest-rate},${2:number-of-periods})$0"
        ],
        "description": "function annuity",
        "scope": "cobol"
    },
    {
        "prefix": "asin",
        "label": "function asin",
        "body": [
            "function asin(${1:sine})$0"
        ],
        "description": "function trigonometric arc-sine, or inverse sine, of sine",
        "scope": "cobol"
    },
    {
        "prefix": "atan",
        "label": "function atan",
        "body": [
            "function atan(${1:tangent})$0"
        ],
        "description": "function trigonometric arc-tangent, or inverse tangent, of tangent",
        "scope": "cobol"
    },
    {
        "prefix": "function bit-of",
        "label": "function bit-of",
        "body": ["function bit-of(${1:integer})$0"],
        "description": "function to convert the argument to bits",
        "scope": "cobol"
    },
    {
        "prefix": "function bit-to-char",
        "label": "function bit-to-char",
        "body": ["function bit-to-char(${1:integer})$0"],
        "description": "function to convert bits to character",
        "scope": "cobol"
    },
    {
        "prefix": "function char",
        "label": "function char",
        "body": ["function char(${1:integer})$0"],
        "description": "get char in current col sequence",
        "scope": "cobol"
    },
    {
        "prefix": "boolean-of-integer",
        "label": "function boolean-of-integer",
        "body": [
            "function boolean-of-integer(${1:integer},${2:length})$0"
        ],
        "description": "function boolean item of usage bit representing the binary value of integer; with the given length",
        "scope": "cobol"
    },
    {
        "prefix": "byte-length",
        "label": "function byte-length",
        "body": [
            "function byte-length(${1:integer})$0"
        ],
        "description": "function byte-length (in bytes) of item",
        "scope": "cobol"
    },
    {
        "prefix": "char",
        "label": "function char",
        "body": [
            "function char(${1:integer})$0"
        ],
        "description": "function character in the ordinal position specified by integer, from the current alphanumeric collating sequence",
        "scope": "cobol"
    },
    {
        "prefix": "char-national",
        "label": "function char-national",
        "body": [
            "function char-national(${1:integer})$0"
        ],
        "description": "function character in the ordinal position specified by &lt;integer&gt; from the current national collating sequence",
        "scope": "cobol"
    },
    {
        "prefix": "combined-datetime",
        "label": "function combined-datetime",
        "body": [
            "function combined-datetime(${1:integer-date},${2:standard-numeric-time})$0"
        ],
        "description": "function combines integer-date and standard-numeric-time into a single numeric item from which both date and time components can be derived",
        "scope": "cobol"
    },
    {
        "prefix": "concatenate",
        "label": "function concatenate",
        "body": [
            "function concatenate(${1:string1},${2:string2})$0"
        ],
        "description": "function concatenates the string1.. string2 into a single string result",
        "scope": "cobol"
    },
    {
        "prefix": "cos",
        "label": "function cos",
        "body": [
            "function cos(${1:angle})$0"
        ],
        "description": "function trigonometric cosine of angle",
        "scope": "cobol"
    },
    {
        "prefix": "currency-symbol",
        "label": "function currency-symbol",
        "body": [
            "function currency-symbol()$0"
        ],
        "description": "function currency symbol character currently in effect for the locale under which the program is running",
        "scope": "cobol"
    },
    {
        "prefix": "current-date",
        "label": "function current-date",
        "body": [
            "function current-date(${1:date-and-time-format})$0"
        ],
        "description": "function current date and time as 21-character value",
        "scope": "cobol"
    },
    {
        "prefix": "formatted-date",
        "label": "function formatted-date",
        "body": [
            "function formatted-date(${1:date-format},${2:integer-date})$0"
        ],
        "description": "function formatted-date",
        "scope": "cobol"
    },
    {
        "prefix": "formatted-datetime",
        "label": "function formatted-datetime",
        "body": [
            "function formatted-datetime(${1:date-and-time-format},${2:integer-date},${3:standard-numeric-time},${4:offset_or_system_offset})$0"
        ],
        "description": "function formatted-datetime",
        "scope": "cobol"
    },
    {
        "prefix": "formatted-time",
        "label": "Function formatted-time",
        "body": [
            "function formatted-time(${1:time-format},${2:standard-numeric-time},${3:offset_or_system_offset})$0"
        ],
        "description": "function formatted-time",
        "scope": "cobol"
    },
    {
        "prefix": "fraction-part",
        "label": "function fraction-part",
        "body": [
            "function fraction-part(${1:number})$0"
        ],
        "description": "portion of number that occurs to the right of the decimal point",
        "scope": "cobol"
    },
    {
        "prefix": "highest-algebraic",
        "label": "function highest-algebraic",
        "body": [
            "function highest-algebraic(${1:numeric-identifier})$0"
        ],
        "description": "highest value that could possibly be stored in the specified numeric-identifier",
        "scope": "cobol"
    },
    {
        "prefix": "hex-of",
        "label": "function hex-of",
        "body": [
            "function hex-of(${1:alphanumeric})$0"
        ],
        "description": "returns an alphanumeric character string consisting of a hexadecimal representation of the argument used on input",
        "scope": "cobol"
    },
    {
        "prefix": "hex-to-char",
        "label": "function hex-to-char",
        "body": [
            "function hex-to-char(${1:alphanumeric})$0"
        ],
        "description": "returns a character string that represents the hexadecimal digit characters supplied on input",
        "scope": "cobol"
    },
    {
        "prefix": "integer",
        "label": "function integer",
        "body": [
            "function integer(${1:number})$0"
        ],
        "description": "greatest integer value that is less than or equal to number",
        "scope": "cobol"
    },
    {
        "prefix": "integer-of-boolean",
        "label": "function integer-of-boolean",
        "body": [
            "dunction integer-of-boolean(${1:boolean-item})$0"
        ],
        "description": "numeric value of boolean-item",
        "scope": "cobol"
    },
    {
        "prefix": "integer-of-date",
        "label": "function integer-of-date",
        "body": [
            "function integer-of-date(${1:yyyymmdd})$0"
        ],
        "description": "converts yyyymmdd to an internal integer-date",
        "scope": "cobol"
    },
    {
        "prefix": "integer-of-day",
        "label": "function integer-of-day",
        "body": [
            "function integer-of-day(${1:yyyymmdd})$0"
        ],
        "description": "converts yyyymmdd to an internal integer-day",
        "scope": "cobol"
    },
    {
        "prefix": "integer-of-formatted-date",
        "label": "function integer-of-formatted-date",
        "body": [
            "function integer-of-formatted-date(${1:format},${2:date})$0"
        ],
        "description": "converts date in specified format to an internal integer",
        "scope": "cobol"
    },
    {
        "prefix": "integer-part",
        "label": "function integer-part",
        "body": [
            "function integer-part(${1:number})$0"
        ],
        "description": "portion of number that occurs to the left of the decimal point",
        "scope": "cobol"
    },
    {
        "prefix": "length",
        "label": "function length",
        "body": [
            "function length(${1:item})$0"
        ],
        "description": "returns the length (in character positions) of the specified item",
        "scope": "cobol"
    },
    {
        "prefix": "length-an",
        "label": "function length-an",
        "body": [
            "function length-an(${1:item})$0"
        ],
        "description": "returns the length (in bytes) of the specified item;",
        "scope": "cobol"
    },
    {
        "prefix": "locale-compare",
        "label": "function locale-compare",
        "body": [
            "function locale-compare(${1:argument1},${2:argument2},${3:locale-optional})$0"
        ],
        "description": "character '=' or '<' or '>' indicating the result of comparing argument1 and argument2 using a culturall",
        "scope": "cobol"
    },
    {
        "prefix": "locale-date",
        "label": "function locale-date",
        "body": [
            "function locale-date(${1:yyyymmdd},${2:locale-optional})$0"
        ],
        "description": "format yyyymmd; according to locale",
        "scope": "cobol"
    },
    {
        "prefix": "locale-time",
        "label": "function locale-time",
        "body": [
            "function locale-time(${1:yyyymmdd},${2:locale-optional})$0"
        ],
        "description": "format time; according to locale",
        "scope": "cobol"
    },
    {
        "prefix": "locale-time-from-seconds",
        "label": "function locale-time-from-seconds",
        "body": [
            "function locale-time-from-seconds(${1:integer-time},${2:locale-optional})$0"
        ],
        "description": "format integer-time (internal-format) according to locale",
        "scope": "cobol"
    },
    {
        "prefix": "log",
        "label": "function log",
        "body": [
            "function log(${1:number})$0"
        ],
        "description": "format base e logarithm of number",
        "scope": "cobol"
    },
    {
        "prefix": "log10",
        "label": "function log10",
        "body": [
            "function log10(${1:number})$0"
        ],
        "description": "format base 10 logarithm of number",
        "scope": "cobol"
    },
    {
        "prefix": "lowercase",
        "label": "function lowercase",
        "body": [
            "function lowercase(${1:string})$0"
        ],
        "description": "character string that contains string with any uppercase letters replaced by their corresponding lowercase letters",
        "scope": "cobol"
    },
    {
        "prefix": "lowest-algebraic",
        "label": "function lowest-algebraic",
        "body": [
            "function lowest-algebraic(${1:numeric-identifier})$0"
        ],
        "description": "lowest value that could possibly be stored in the specified numeric-identifier",
        "scope": "cobol"
    },
    {
        "prefix": "max",
        "label": "function max",
        "body": [
            "function max(${1:number},${2:...})$0"
        ],
        "description": "Alphanumeric/Index/Integer/National/Numeric maximum value from the specified list of numbers",
        "scope": "cobol"
    },
    {
        "prefix": "nmean",
        "label": "function mean",
        "body": [
            "function mean(${1:number},${2:...})$0"
        ],
        "description": "statistical mean value of the specified list of numbers",
        "scope": "cobol"
    },
    {
        "prefix": "median",
        "label": "function median",
        "body": [
            "function median(${1:number},${2:...})$0"
        ],
        "description": "statistical median value of the specified list of numbers",
        "scope": "cobol"
    },
    {
        "prefix": "midrange",
        "label": "function midrange",
        "body": [
            "function midrange(${1:number},${2:...})$0"
        ],
        "description": "statistical midrange value of the specified list of numbers",
        "scope": "cobol"
    },
    {
        "prefix": "min",
        "label": "function min",
        "body": [
            "function min(${1:number},${2:...})$0"
        ],
        "description": "statistical min value of the specified list of numbers",
        "scope": "cobol"
    },
    {
        "prefix": "mod",
        "label": "function mod",
        "body": [
            "function mod(${1:value},${2:modulus})$0"
        ],
        "description": "remainder from the division of value by modulus",
        "scope": "cobol"
    },
    {
        "prefix": "monetary-decimal-point",
        "label": "function monetary-decimal-point",
        "body": [
            "function monetary-decimal-point()$0"
        ],
        "description": "character used to separate the integer portion from the fractional part of a monetary currency value according to the current locale",
        "scope": "cobol"
    },
    {
        "prefix": "monetary-thousands-separator",
        "label": "function monetary-thousands-separator",
        "body": [
            "function monetary-thousands-separator()$0"
        ],
        "description": "character used to separate the thousands digit groupings in a monetary currency value according to the current locale",
        "scope": "cobol"
    },
    {
        "prefix": "national-of",
        "label": "function national-of",
        "body": [
            "function national-of(${1:alphanumeric-string},${2:replacement-char})$0"
        ],
        "description": "convert alphanumeric-string to the national coded character set representation",
        "scope": "cobol"
    },
    {
        "prefix": "numeric-thousands-separator",
        "label": "function numeric-thousands-separator",
        "body": [
            "function numeric-thousands-separator()$0"
        ],
        "description": "character used to separate the thousands digit groupings in a numeric value according to the current locale",
        "scope": "cobol"
    },
    {
        "prefix": "numval",
        "label": "function numval",
        "body": [
            "function numval(${1:string})$0"
        ],
        "description": "corresponding numeric value for string",
        "scope": "cobol"
    },
    {
        "prefix": "numval-c",
        "label": "function numval-c",
        "body": [
            "function numval-c(${1:string},${2:currency-symbol}${3:,ANYCASE})$0"
        ],
        "description": "corresponding numeric value for string, case-sensitive if ANYCASE not given",
        "scope": "cobol"
    },
    {
        "prefix": "numval-f",
        "label": "function numval-f",
        "body": [
            "function numval-f(${1:string})$0"
        ],
        "description": "corresponding numeric value for floating-point-string",
        "scope": "cobol"
    },
    {
        "prefix": "ord",
        "label": "function ord",
        "body": [
            "function ord(${1:char},${2:...})$0"
        ],
        "description": "ordinal position in the program character set corresponding to char",
        "scope": "cobol"
    },
    {
        "prefix": "ord-max",
        "label": "function ord-max",
        "body": [
            "function ord-max(${1:char},${2:...})$0"
        ],
        "description": "max. ordinal position in the program character set corresponding to list of chars",
        "scope": "cobol"
    },
    {
        "prefix": "ord-min",
        "label": "function ord-min",
        "body": [
            "function ord-min(${1:char},${2:...})$0"
        ],
        "description": "min. ordinal position in the program character set corresponding to list of chars",
        "scope": "cobol"
    },
    {
        "prefix": "pi",
        "label": "function pi",
        "body": [
            "function pi()$0"
        ],
        "description": "pi",
        "scope": "cobol"
    },
    {
        "prefix": "present-value",
        "label": "function present-value",
        "body": [
            "function present-value(${1:discount-rate},${2:amount},${3:...})$0"
        ],
        "description": "approximation of the present value of a series of future period-end amount",
        "scope": "cobol"
    },
    {
        "prefix": "random",
        "label": "function random",
        "body": [
            "function random(${1:seed})$0"
        ],
        "description": "pseudo-random number 0> <1 from a rectangular distribution with optional seed",
        "scope": "cobol"
    },
    {
        "prefix": "range",
        "label": "function range",
        "body": [
            "function range(${1:argument},${2:...})$0"
        ],
        "description": "value of the maximum argument minus the value of the minimum argument",
        "scope": "cobol"
    },
    {
        "prefix": "rem",
        "label": "function rem",
        "body": [
            "function rem(${1:number},${2:divisor})$0"
        ],
        "description": "remainder of number divided by divisor",
        "scope": "cobol"
    },
    {
        "prefix": "reverse",
        "label": "function reverse",
        "body": [
            "function reverse(${1:string})$0"
        ],
        "description": "reverse representation with same length of string",
        "scope": "cobol"
    },
    {
        "prefix": "seconds-from-formatted-time",
        "label": "function seconds-from-formatted-time",
        "body": [
            "function seconds-from-formatted-time(${1:format},${2:time})$0"
        ],
        "description": "decode time according to format (a time format or a combined date and time format)",
        "scope": "cobol"
    },
    {
        "prefix": "seconds-past-midnight",
        "label": "function seconds-past-midnight",
        "body": [
            "function seconds-past-midnight()$0"
        ],
        "description": "current time of day expressed as the total number of elapsed seconds since midnight",
        "scope": "cobol"
    },
    {
        "prefix": "sign",
        "label": "function sign",
        "body": [
            "function sign(${1:number})$0"
        ],
        "description": "sign representation of number as -1, 0, 1",
        "scope": "cobol"
    },
    {
        "prefix": "sin",
        "label": "function sin",
        "body": [
            "function sin(${1:angle})$0"
        ],
        "description": "trigonometric sine of the specified angle",
        "scope": "cobol"
    },
    {
        "prefix": "sqrt",
        "label": "function sqrt",
        "body": [
            "function sqrt(${1:number})$0"
        ],
        "description": "aproximation of the square root of number",
        "scope": "cobol"
    },
    {
        "prefix": "tan",
        "label": "function tan",
        "body": [
            "function tan(${1:angle})$0"
        ],
        "description": "trigonometric tangent of the specified angle",
        "scope": "cobol"
    },
    {
        "prefix": "trim",
        "label": "function trim",
        "body": [
            "function trim(${1:argument})$0"
        ],
        "description": "returns a character string that trims the leading and/or trailing spaces from the argument supplied on input (opt leading/trailing)",
        "scope": "cobol"
    },
    {
        "prefix": "upper-case",
        "label": "function upper-case",
        "body": [
            "function upper-case(${1:string})$0"
        ],
        "description": "character string that contains string with any lowercase letters replaced by their corresponding uppercase letters",
        "scope": "cobol"
    },
    {
        "prefix": "variance",
        "label": "function variance",
        "body": [
            "function variance(${1:number},${2:...})$0"
        ],
        "description": "statistical variance of the specified list of number argument",
        "scope": "cobol"
    },
    {
        "prefix": "year-to-yyyy",
        "label": "function year-to-yyyy",
        "body": [
            "function year-to-yyyy(${1:yy},${2:[yy-cutoff, default: 50},${3:yy-execution-time, default: now})$0"
        ],
        "description": "convert yy to yyyy with optional yy-cutoff to delineate centuries",
        "scope": "cobol"
    }
];

const dollarSnippets: ISimpleSnippet[] = [
    {
        "prefix": "$set",
        "label": "$set dialect",
        "body": [
            "\\$set dialect\"${2|ans85,bs2000,bs2000-offload,cob370,cob371,cob372,mf,mvs,os390,osvs,vsc21,vsc22,vsc23,vsc24|}\"",
            "$0"
        ],
        "description": "Set source dialect",
        "scope": "cobol"
    },
    {
        "prefix": "$set",
        "label": "$set sourceformat",
        "body": [
            "\\$set sourceformat\"${2|free,variable,fixed|}\"",
            "$0"
        ],
        "description": "Set source format",
        "scope": "cobol"
    },
    {
        "prefix": "$if",
        "label": "$if constant [expr] literal",
        "body": [
            "\\$if ${1:constant-name} ${2|=,<,>,not =,not <,not >|} ${3:literal}",
            "$0",
            "\\$end"
        ],
        "description": "$if constant-name |expr| literal",
        "scope": "cobol"
    },
    {
        "prefix": "$if",
        "label": "$if constant-name [not] defined",
        "body": [
            "\\$if ${1:constant-name} ${2|defined,not defined|}",
            "$0",
            "\\$end"
        ],
        "description": "$if constant-name [not] defined",
        "scope": "cobol"
    },
    {
        "prefix": "$if",
        "label": "$if platform [not] defined (MF9+",
        "body": [
            "\\$if ${1|__unix,__windows,__dotnet,__jvm,__native|} ${2|defined,not defined|}",
            "$0",
            "\\$end"
        ],
        "description": "$if platform [not] defined (MF9+)",
        "scope": "cobol"
    },
    {
        "prefix": "$end",
        "label": "$end",
        "body": [
            "\\$end",
            "$0"
        ],
        "description": "$end",
        "scope": "cobol"
    },
    {
        "prefix": "$else",
        "label": "$else",
        "body": [
            "\\$else",
            "$0"
        ],
        "description": "$else",
        "scope": "cobol"
    },
    {
        "prefix": "$if",
        "label": "$if directive set",
        "body": [
            "\\$if ${1|ACCEPTREFRESH,ACTUALPARAMS,ACUOPT,ACU-COMMENT,ACUSYNC,ACU-UNDERSCORE,ACU,ADDRSV,ADDSYN,ADV,ALIGN,ALPHA-LIT-CONT,ALPHASTART,ALTER,AMODE,ANIM,ANS85,APOST,AREACHECK,ARITH,ARITHMETIC,ASSIGN,ASSIGN-PRINTER,AUTOLOCK,BELL,BINLIT,BOUNDOPT,BOUND,BRIEF,BS2000,BWZSTAR,BYTE-MODE-MOVE,CALL-RECOVERY,CALLFH,CALLMCS,CALLSORT,CANCEL,CANCELLBR,CASE,CHANGE-MESSAGE,CHARSET,CHECK,CHECKDIV,CHECKNUM,CHECKREFMOD,CICSECM,CMPR2,COBFSTATCONV,COBIDY,COBOL370,COBOLDIR,COLLECTION,COMMAND-LINE-LINKAGE,COMP1,COMP2,COMP-5,COMP-6,COMP,COMS85,CONFIRM,CONSTANT,CONVSPACE,COPYEXT,COPYLBR,COPYLIST,COPYLISTCOMMENT,COPYPATH,COPYSEARCH,CSI,CURRENCY-SIGN,CURRENT-DATE,DATA,DATACOMPRESS,DATA-CONTEXT,DATAMAP,DATE,DECLARE,DB2,DBCHECK,DBCS,DBCSSOSI,DBSPACE,DE-EDIT,DEFAULTBYTE,DEFAULTCALLS,DETECT-LOCK,DG,DIALECT,DIRECTIVES,DIRECTIVES-IN-COMMENTS,DIR,DISPLAY,DISPSIGN,DOSVS,DOTNET,DPC-IN-SUBSCRIPT,DYNAM,EBC-COL-SEQ,ECHO,ECHOALL,ENTCOBOL,EOF-1A,ERRFORMAT,ERRLIST,ERRQ,EXITPROGRAM,FASTCALL,FASTINIT,FASTLINK,FASTSORT,FCD3,FCDALIGN,FCDCAT,FDCLEAR,FCDREG,FILESHARE,FILETYPE,FIXOPT,FLAG,FLAGAS,FLAGEUC,FLAGMIG,FLAGQ,FLAGSINEDIT,FLAGSTD,FOLD-CALL-NAME,FOLD-COPY-NAME,FORM,FP-ROUNDING,GNT,GNTLITLINKSTD,HIDE-MESSAGE,HOSTARITHMETIC,HOSTCONTZERO,HOSTFD,HOST-NUMCOMPARE,HOST-NUMMOVE,HOSTRW,IBM-MS,IBMCOMP,IDENTIFIERLEN,IDXFORMAT,IGNOREEXEC,ILARRAYPROPERTY,ILASSEMBLY,ILCLR,ILCOMPANY,ILCOPYRIGHT,ILCULTURE,ILCUTPREFIX,ILDELAYSIGN,ILDESCRIPTION,ILDOC,ILDYNCALL,ILEXPOSEALPHA,ILEXPOSEGROUP,ILEXPONENTIATION,ILFILEVERSION,ILSTRINGLOAD,JVMGEN,ILGEN,ILICON,ILKEYFILE,ILKEYNAME,ILMAIN,ILMANIFEST,ILNAMESPACE,ILNATIVE,ILNATIVERESOURCE,ILOPTIMIZEDATA,ILOUTPUT,ILPARAMS,ILPINVOKE,ILPRODUCT,ILPRODUCTVERSION,ILREF,ILRESOURCE,ILSHOWPERFORMOVERLAP,ILSMARTANNOTATE,ILSMARTLINKAGE,ILSMARTNEST,ILSMARTRESTRICT,ILSMARTSERIAL,ILSMARTTRIM,ILSOURCE,ILSTACKSIZE,ILSTATIC,ILSTDLIB,ILSUBSYSTEM,ILTARGET,ILTITLE,ILTRADEMARK,ILUSING,ILVERIFY,ILVERSION,IMPLICITSCOPE,INITIAL,INDD,INFORETURN,INIT-BY-TYPE,INITCALL,INITPTR,INT,INTDATE,INTLEVEL,IOCONV,ISO2002,IXNLSKEY,IXNUMKEY,JAPANESE,JAVA-SHAREABLE,JAVA-CALLABLE,JAVA-GEN-PROGS,JAVA-GEN-STRG,JAVA-OUTPUT-PATH,JAVA-PACKAGE-NAMEKEYCHECK,KEYCOMPRESS,LIBRARIAN,LINE-COUNT,LINKALIAS,LINKCHECK,LIST,LISTPATH,LISTWIDTH,LW,LITLINK,LITVAL-SIZE,LNKALIGN,LOCALCOUNT,LOCALSOURCEFORMAT,LOCKTYPE,MAINFRAME-FLOATING-POINT,MAKESYN,MAPNAME,MAX-ERROR,METHODDEFAULT,MF,MFLEVEL,MFCOMMENT,MFSYNC,MOVE-LEN-CHECK,MS,MVS,NATIONAL,NATIVE,NATIVE-FLOATING-POINT,NCHAR,NLS,NLSCURRENCYLENGTH,NSYMBOL,NULL-ESCAPE,NUMPROC,OBJ,ODOOSVS,ODOSLIDE,OLDBLANKLINE,OLDCOPY,OLDINDEX,OLDNEXTSENTENCE,OLDREADINTO,OLDSTRMIX,OOCTRL,OPT(Intelx86platforms),OPT(Non-Intelx86platforms),OPTIONAL-FILE,OS390,OSEXT,OSVS,OUTDD,OVERRIDE,P64,PANVALET,PARAMCOUNTCHECK,PC1,PCOMP,PERFORM-TYPE,PERFORMOPT,PPLITLINK,PREPLIST,PREPROCESS,PRESERVECASE,PRINT,PRINT-EXT,PROFILE,PROGID-COMMENT,PROGID-INT-NAME,PROTECT-LINKAGE,PROTOTYPE,P,QUAL,QUALPROC,QUERY,QUOTE,RAWLIST,RDFPATH,RDW,RECMODE,RECURSECHECK,REENTRANT,REF,REFNO,REMAINDER,REMOVE,REPORT-LINE,REPOSITORY,RESEQ,RESTRICT-GOTO,RETRYLOCK,REWRITE-LS,RM,RTNCODE-TYPE,RTNCODE-SIZE,RUNTIME-ENCODING,RWHARDPAGE,SAA,SCHEDULER,SEG,SEQCHK,SEQUENTIAL,SERIAL,SETTING,SETTINGS,SHARE-OUTDD,SHOW-DIR,SIGN,SIGNDISCARD,SIGN-FIXUP,SORTTYPE,SOURCEASM,SOURCE-ENCODING,SOURCEFORMAT,SOURCETABSTOP,SQL,SSRANGE,STDERR,STICKY-LINKAGE,STICKY-PERFORM,SUPFF,SWITCH-TYPE,SYMBSTART,SYSPUNCH,TERMPAGE,TESTCOVER,TIME,TRACE,TRUNC,TRUNCCALLNAME,TRUNCCOPY,TRUNCINC,UNICODE,USE,VERBOSE,VSC2,WARNINGS,WARNING,WB2,WB3,WB,WRITELOCK,WRITE-LOCK,WRITETHROUGHWRITETHRU,XDB,XMLGEN,XMLPARSE,XOPEN,XREF,ZEROLENGTHFALSE,ZEROSEQ,ZWB|} set",
            "${TM_SELECTED_TEXT}",
            "\\$end"
        ],
        "description": "$if directive set",
        "scope": "cobol"
    },
    {
        "prefix": "$display",
        "label": "$display message",
        "body": [
            "\\$display ${1:message}",
            "$0"
        ],
        "description": "$display message",
        "scope": "cobol"
    },
    {
        "prefix": "$display",
        "label": "$display VCS = version-number",
        "body": [
            "\\$display VCS = ${1:version-number}",
            "$0"
        ],
        "description": "$display VCS = version-number",
        "scope": "cobol"
    },
    {
        "prefix": "$end-region",
        "label": "$end-region",
        "body": [
            "\\$end-region",
            "$0"
        ],
        "description": "$end-region",
        "scope": "cobol"
    },
    {
        "prefix": "$region",
        "label": "$region",
        "body": [
            "\\$region ${1:description}"
        ],
        "description": "$region",
        "scope": "cobol"
    },
];

const isoSnippets: ISimpleSnippet[] = [
    {
        "label": ">>define directive",
        "prefix": ">>define",
        "body": [
            ">>define ${1:variable-name} as ${2|10,41 + 1,parameter|}",
            "$0"
        ],
        "description": ">>define directive",
        "scope": "cobol"
    },
    {
        "label": ">>define directive + override",
        "prefix": ">>define",
        "body": [
            ">>define ${1:variable-name} as ${2|10,41 + 1,parameter|} ${3:override}",
            "$0"
        ],
        "description": ">>define directive + override",
        "scope": "cobol"
    },
    {
        "label": ">>if constant [condition]",
        "prefix": ">>if ",
        "body": [
            ">>if ${1:variable-name} ${2|is defined,= 10|} ",
            "$0",
            ">>end-if"
        ],
        "description": ">>if constant [condition]",
        "scope": "cobol"
    },
    {
        "label": ">>evaluate directive + override",
        "prefix": ">>evaluate",
        "body": [
            ">>evaluate ${1|true,false,10,41 + 1,literal|}",
            ">>when ${2:expression}",
            "$0",
            ">>end-evaluate"
        ],
        "description": ">>evaluate.. >>when... >>end-evaluate",
        "scope": "cobol"
    },

    {
        "label": ">>java-shareable [on|off]",
        "prefix": ">>java-shareable",
        "body": [
            ">>java-shareable ${1|on,off|}",
        ],
        "description": ">>java-shareable [on|off]",
        "scope": "cobol"
    }

];

class SnippetHelper {
    protected foldKeywordLine(texts: string[], languageid: string, settings: ICOBOLSettings): string {
        const sb = [];
        for (const text of texts) {
            sb.push(VSCOBOLUtils.foldTokenLine(text, undefined, FoldAction.Keywords, false, languageid, settings, settings.intellisense_style));
        }

        return sb.join(jsonCRLF);
    }

    protected addSnippet(settings: ICOBOLSettings, kind: CompletionItemKind, snippet: ISimpleSnippet, langId: string, targets: Map<string, CompletionItem[]>): void {
        let items = targets.get(snippet.prefix);
        if (items === undefined) {
            targets.set(snippet.prefix, []);
            items = targets.get(snippet.prefix);
        }

        if (items === undefined) {
            return;
        }

        let preselect = snippet.label;
        let kiSnippet = "";
        const istyle = snippet.alwaysUpperCase ? intellisenseStyle.UpperCase : VSCustomIntelliseRules.Default.findCustomIStyle(settings, snippet.prefix, settings.intellisense_style);

        switch (istyle) {
            case intellisenseStyle.CamelCase:
                preselect = SourceScannerUtils.camelize(snippet.label);
                kiSnippet = this.foldKeywordLine(snippet.body, langId, settings);
                break;
            case intellisenseStyle.UpperCase:
                preselect = preselect.toUpperCase();
                kiSnippet = this.foldKeywordLine(snippet.body, langId, settings);
                break;
            case intellisenseStyle.LowerCase:
                preselect = preselect.toLowerCase();
                kiSnippet = this.foldKeywordLine(snippet.body, langId, settings);
                break;
            case intellisenseStyle.Unchanged:
                kiSnippet = snippet.body.join(jsonCRLF);
                break;
        }

        const ci = new CompletionItem(preselect);
        ci.keepWhitespace = false;
        ci.kind = kind;
        ci.insertText = new SnippetString(kiSnippet);
        ci.preselect = true;
        ci.commitCharacters = [];
        ci.documentation = snippet.description;
        if (snippet.triggerIntellisense) {
            ci.command = { command: "editor.action.triggerSuggest", title: "Re-trigger completions..." };
        }
        ci.detail = snippet.detail !== undefined ? snippet.detail : "";

        items.push(ci);
    }
}

export class KeywordSnippetProvider extends SnippetHelper {
    public static Default: KeywordSnippetProvider = new KeywordSnippetProvider()

    private keywordTargets = new Map<string, CompletionItem[]>();

    public reInitKeyMap(settings: ICOBOLSettings): KeywordSnippetProvider {
        this.keywordTargets.clear();

        for (const simpleSnippet of simpleSnippets) {
            this.addSnippet(settings, CompletionItemKind.Keyword, simpleSnippet, ExtensionDefaults.defaultCOBOLLanguage, this.keywordTargets);
        }
        return this;
    }

    public getKeywordSnippet(word: string): CompletionItem[] {
        const snippets: CompletionItem[] = [];
        const targets = this.keywordTargets.get(word.toLowerCase());
        if (targets === undefined) {
            return snippets;
        }

        for (const ci of targets) {
            if (ci.insertText !== undefined) {
                snippets.push(ci);
            }
        }
        return snippets;
    }
}

export class SnippetCompletionItemProvider extends SnippetHelper implements CompletionItemProvider {
    public static Default: SnippetCompletionItemProvider = new SnippetCompletionItemProvider()

    private allCallTargets = new Map<string, CompletionItem[]>();
    private functionTargets = new Map<string, CompletionItem[]>();
    private dollarTargets = new Map<string, CompletionItem[]>();
    private isoTargets = new Map<string, CompletionItem[]>();
    private wordRegEx = new RegExp("[>$#0-9a-zA-Z][>a-zA-Z0-9-_]*");

    public reInitCallMap(settings: ICOBOLSettings): SnippetCompletionItemProvider {
        this.allCallTargets.clear();
        this.functionTargets.clear();
        this.dollarTargets.clear();
        this.isoTargets.clear();

        const callMap = KnownAPIs.getCallTargetMap(ExtensionDefaults.defaultCOBOLLanguage);
        for (const [api,] of callMap) {
            const ci = this.getCompletionItemForAPI(settings, ExtensionDefaults.defaultCOBOLLanguage, api);
            if (ci !== undefined) {
                let cis = this.allCallTargets.get(api);
                if (cis === undefined) {
                    this.allCallTargets.set(api, []);
                    cis = this.allCallTargets.get(api);
                }
                if (cis !== undefined) {
                    cis.push(ci);
                }
            }
        }

        KeywordSnippetProvider.Default.reInitKeyMap(settings);

        for (const simpleSnippet of functionSnippets) {
            this.addSnippet(settings, CompletionItemKind.Snippet, simpleSnippet, ExtensionDefaults.defaultCOBOLLanguage, this.functionTargets);
        }

        for (const dollarSnippet of dollarSnippets) {
            this.addSnippet(settings, CompletionItemKind.Snippet, dollarSnippet, ExtensionDefaults.defaultCOBOLLanguage, this.dollarTargets);
        }

        for (const isoSnippet of isoSnippets) {
            this.addSnippet(settings, CompletionItemKind.Snippet, isoSnippet, ExtensionDefaults.defaultCOBOLLanguage, this.isoTargets);
        }

        return this;
    }


    public getAllFunctions(): CompletionItem[] {
        const snippets: CompletionItem[] = [];

        for (const [, cis] of this.functionTargets) {
            for (const ci of cis) {
                if (ci.insertText !== undefined) {
                    snippets.push(ci);
                }
            }
        }

        return snippets;
    }


    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private getCompletionItemForAPI(settings: ICOBOLSettings, langId: string, api: string): CompletionItem | undefined {
        const keyword = "call";
        const ki = KnownAPIs.getCallTarget(langId, api);
        if (ki === undefined) {
            return undefined;
        }
        let kiExample = "";
        let kiSnippet = "";
        let callStatement = keyword;

        const iStyle = VSCustomIntelliseRules.Default.findCustomIStyle(settings, ki.api, settings.intellisense_style);
        switch (iStyle) {
            case intellisenseStyle.CamelCase:
                kiSnippet = this.foldKeywordLine(ki.snippet, langId, settings);
                kiExample = this.foldKeywordLine(ki.example, langId, settings);
                callStatement = SourceScannerUtils.camelize(callStatement);
                break;
            case intellisenseStyle.UpperCase:
                kiSnippet = this.foldKeywordLine(ki.snippet, langId, settings);
                kiExample = this.foldKeywordLine(ki.example, langId, settings);
                callStatement = callStatement.toUpperCase();
                break;
            case intellisenseStyle.LowerCase:
                kiSnippet = this.foldKeywordLine(ki.snippet, langId, settings);
                kiExample = this.foldKeywordLine(ki.example, langId, settings);
                callStatement = callStatement.toLowerCase();
                break;
            case intellisenseStyle.Unchanged:
                kiSnippet = ki.snippet.join(jsonCRLF);
                kiExample = ki.example.join(jsonCRLF);
                break;
        }
        const preselect = `${callStatement} "${api}"`;
        const ci = new CompletionItem(preselect);
        ci.keepWhitespace = false;
        ci.kind = CompletionItemKind.Snippet;
        ci.insertText = new SnippetString(kiSnippet);
        ci.preselect = true;
        ci.filterText = preselect;
        ci.commitCharacters = [`${callStatement} "${api}"`];

        let documentation = ki === undefined ? "" : ki.description.join(jsonCRLF);

        if (ki !== undefined && ki.example.length !== 0) {
            documentation += `${jsonCRLF}${jsonCRLF}Example:${jsonCRLF}~~~${jsonCRLF}${kiExample}${jsonCRLF}~~~`;
        }

        ci.documentation = new MarkdownString(documentation);
        return ci;
    }

    private getAllSnippets(document: TextDocument, position: Position, prevWordLower: string, targets: Map<string, CompletionItem[]>) {
        const position_plus1 = new Position(position.line, position.character + 1);
        const position_plus1_char = document.getText(new Range(position, position_plus1));
        const snippets: CompletionItem[] = [];

        for (const [, cis] of targets) {
            for (const ci of cis) {
                if (ci.insertText !== undefined) {
                    const line = document.lineAt(position.line);
                    const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf(prevWordLower);
                    if (position_plus1_char !== undefined && position_plus1_char === "\"") {
                        ci.range = new Range(new Position(position.line, charPosForCall), position_plus1);
                    } else {
                        ci.range = new Range(new Position(position.line, charPosForCall), position);
                    }

                    snippets.push(ci);
                }
            }
        }
        return snippets;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public provideCompletionItems(document: TextDocument, position: Position, token: CancellationToken, context: CompletionContext): ProviderResult<CompletionItem[] | CompletionList> {
        if (token.isCancellationRequested) {
            return [];
        }

        const config = VSCOBOLConfiguration.get_resource_settings(document, VSExternalFeatures);

        if (config.snippets === false) {
            return [];
        }

        const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, config);
        if (qcp === undefined) {
            return [];
        }
        const currentlineText = document.lineAt(position.line);
        const previousCharacter = currentlineText.text.substring(position.character - 1);
        // const currentLine = currentlineText.text;

        if (previousCharacter === "$") {
            return this.getAllSnippets(document, position, "$", this.dollarTargets);
        }

        if (previousCharacter === ">>") {
            return this.getAllSnippets(document, position, ">>", this.isoTargets);
        }

        const wordRange = document.getWordRangeAtPosition(new Position(position.line, position.character - 2), this.wordRegEx); // 1 space -1
        if (!wordRange) return [];
        const previousWord = document.getText(wordRange);

        if (previousWord.length >= 4 && previousCharacter === " ") {
            const prevWordLower = previousWord.toLowerCase();
            switch (prevWordLower) {
                case "call": return this.getAllSnippets(document, position, prevWordLower, this.allCallTargets);
                case "function": return this.getAllSnippets(document, position, prevWordLower, this.functionTargets);
            }

            return [];
        }

        const position_plus1 = new Position(position.line, position.character + 1);
        const position_plus1_char = document.getText(new Range(position, position_plus1));
        const snippets: CompletionItem[] = [];
        if (previousWord !== undefined) {
            if (previousWord.startsWith("$")) {
                this.getExactDollorOrPartial(position, position_plus1, previousWord, currentlineText, position_plus1_char, snippets);
                return snippets;
            }

            if (previousWord.startsWith(">>")) {
                this.getExactIsoOrPartial(position, position_plus1, previousWord, currentlineText, position_plus1_char, snippets);
                return snippets;
            }

            const prevWordChar = position.character - previousWord.length - 3; // 2 spaces -1
            if (prevWordChar >= 0) {
                const wordRangeForCall = document.getWordRangeAtPosition(new Position(position.line, prevWordChar));
                const pre2Word = document.getText(wordRangeForCall);

                if (pre2Word !== undefined) {
                    const pre2wordLower = pre2Word.toLowerCase();
                    switch (pre2wordLower) {
                        case "call":
                            this.getExactCallSnipetOrPartialSnippet(position, position_plus1, previousWord, currentlineText, position_plus1_char, snippets);
                            break;
                        case "function":
                            this.getExactFunctionSnipetOrPartialSnippet(position, position_plus1, previousWord, currentlineText, position_plus1_char, snippets);
                            break;
                    }
                }
            }
        }

        return snippets;
    }

    private getExactCallSnipetOrPartialSnippet(position: Position, position_plus1: Position, preWord: string, line: TextLine, position_plus1_char: string, snippets: CompletionItem[]) {
        const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf("call");
        const preTrimmedWork = COBOLSourceScanner.trimLiteral(preWord,false);
        const preTrimmedWorkUpper = preTrimmedWork.toUpperCase();
        const cis = this.allCallTargets.get(preTrimmedWorkUpper);
        if (cis !== undefined) {
            for (const ci of cis) {
                if (ci.insertText !== undefined) {
                    if (position_plus1_char !== undefined && position_plus1_char === "\"") {
                        ci.range = new Range(new Position(position.line, charPosForCall), position_plus1);
                    } else {
                        ci.range = new Range(new Position(position.line, charPosForCall), position);
                    }
                }
                snippets.push(ci);
            }
        } else {
            // get partial snippets
            for (const [api, cis] of this.allCallTargets) {
                for (const ci of cis) {
                    if (ci.insertText !== undefined) {
                        if (api.startsWith(preTrimmedWorkUpper)) {
                            if (position_plus1_char !== undefined && position_plus1_char === "\"") {
                                ci.range = new Range(new Position(position.line, charPosForCall), position_plus1);
                            } else {
                                ci.range = new Range(new Position(position.line, charPosForCall), position);
                            }
                            snippets.push(ci);
                        }
                    }
                }
            }
        }
    }

    private getExactFunctionSnipetOrPartialSnippet(position: Position, position_plus1: Position, preWord: string, line: TextLine, position_plus1_char: string, snippets: CompletionItem[]) {
        const charPosForCall = line.text.toLocaleLowerCase().lastIndexOf("function");
        const preTrimmedWork = COBOLSourceScanner.trimLiteral(preWord, true);
        const preTrimmedWorkLower = preTrimmedWork.toLowerCase();
        const cis = this.functionTargets.get(preTrimmedWorkLower);
        if (cis !== undefined && cis.length > 0) {
            for (const ci of cis) {
                if (ci.insertText !== undefined) {
                    ci.range = new Range(new Position(position.line, charPosForCall), position);
                }
                snippets.push(ci);
            }
        } else {
            // get partial snippets
            for (const [api, cis] of this.functionTargets) {
                for (const ci of cis) {
                    if (ci.insertText !== undefined) {
                        if (api.startsWith(preTrimmedWorkLower)) {
                            ci.range = new Range(new Position(position.line, charPosForCall), position);
                            snippets.push(ci);
                        }
                    }
                }
            }
        }
    }


    private getExactDollorOrPartial(position: Position, position_plus1: Position, preWord: string, line: TextLine, position_plus1_char: string, snippets: CompletionItem[]) {
        const charPosForDollar = line.text.toLocaleLowerCase().lastIndexOf("$");
        const preTrimmedWork = COBOLSourceScanner.trimLiteral(preWord, true);
        const preTrimmedWorkLower = preTrimmedWork.toLowerCase();
        const dis = this.dollarTargets.get(preTrimmedWorkLower);
        if (dis !== undefined && dis.length > 0) {
            for (const ci of dis) {
                if (ci.insertText !== undefined) {
                    ci.range = new Range(new Position(position.line, charPosForDollar), position);
                }
                snippets.push(ci);
            }
        } else {
            // get partial snippets
            for (const [dollarSnippet, dis] of this.dollarTargets) {
                for (const ci of dis) {
                    if (ci.insertText !== undefined) {
                        if (dollarSnippet.startsWith(preTrimmedWorkLower)) {
                            ci.range = new Range(new Position(position.line, charPosForDollar), position);
                            snippets.push(ci);
                        }
                    }
                }
            }
        }
    }

    private getExactIsoOrPartial(position: Position, position_plus1: Position, preWord: string, line: TextLine, position_plus1_char: string, snippets: CompletionItem[]) {
        const charPosForISO = line.text.toLocaleLowerCase().lastIndexOf(">>");
        const preTrimmedWork = COBOLSourceScanner.trimLiteral(preWord, false);
        const preTrimmedWorkLower = preTrimmedWork.toLowerCase();
        const dis = this.isoTargets.get(preTrimmedWorkLower);
        if (dis !== undefined && dis.length > 0) {
            for (const ci of dis) {
                if (ci.insertText !== undefined) {
                    ci.range = new Range(new Position(position.line, charPosForISO), position);
                }
                snippets.push(ci);
            }
        } else {
            // get partial snippets
            for (const [isoSnippet, dis] of this.isoTargets) {
                for (const ci of dis) {
                    if (ci.insertText !== undefined) {
                        if (isoSnippet.startsWith(preTrimmedWorkLower)) {
                            ci.range = new Range(new Position(position.line, charPosForISO), position);
                            snippets.push(ci);
                        }
                    }
                }
            }
        }
    }
}