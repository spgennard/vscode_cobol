# COBOL 85 Verbs: Optional Modules

These verbs are part of optional COBOL modules (for example, Report Writer and Communication). Compiler support varies.

## INITIATE
Starts Report Writer processing for one or more report descriptions.

```cobol
INITIATE SALES-REPORT
```

## GENERATE
Creates a report line/group according to report description rules.

```cobol
GENERATE DETAIL-LINE
```

## SUPPRESS
Suppresses printing of a report group in specific conditions.

```cobol
SUPPRESS PRINTING
```

## TERMINATE
Ends Report Writer processing and flushes pending report output.

```cobol
TERMINATE SALES-REPORT
```

## ENABLE
Enables communication queues/devices for message processing.

```cobol
ENABLE INPUT TERMINAL-1
```

## DISABLE
Disables communication queues/devices.

```cobol
DISABLE INPUT TERMINAL-1
```

## SEND
Sends a message through Communication Section facilities.

```cobol
SEND MSG-BUFFER FROM WS-MESSAGE
```

## RECEIVE
Receives a message from Communication Section facilities.

```cobol
RECEIVE MSG-BUFFER INTO WS-MESSAGE
```

## Final complete example
This complete example shows skeleton usage of Report Writer and Communication verbs.

```cobol
	   IDENTIFICATION DIVISION.
	   PROGRAM-ID. OPTIONALM.

	   DATA DIVISION.
	   WORKING-STORAGE SECTION.
	   01  WS-MESSAGE            PIC X(80).
	   01  MSG-BUFFER            PIC X(80).

	   REPORT SECTION.
	   RD  SALES-REPORT.
	   01  TYPE DETAIL-LINE.
		   03 COLUMN 1 PIC X(30) SOURCE WS-MESSAGE.

	   PROCEDURE DIVISION.
	   MAIN.
		   INITIATE SALES-REPORT.

		   MOVE "SALES EVENT START" TO WS-MESSAGE.
		   GENERATE DETAIL-LINE.

		   IF WS-MESSAGE = SPACES
			   SUPPRESS PRINTING
		   END-IF.

		   ENABLE INPUT TERMINAL-1.
		   RECEIVE MSG-BUFFER INTO WS-MESSAGE.
		   SEND MSG-BUFFER FROM WS-MESSAGE.
		   DISABLE INPUT TERMINAL-1.

		   TERMINATE SALES-REPORT.
		   STOP RUN.
```
