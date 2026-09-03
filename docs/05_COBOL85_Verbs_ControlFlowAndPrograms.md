# COBOL 85 Verbs: Control Flow And Programs

## IF
Executes statements conditionally based on a logical test.

```cobol
IF WS-BALANCE < 0
    DISPLAY "OVERDRAWN"
ELSE
    DISPLAY "OK"
END-IF
```

## EVALUATE
Performs multi-branch selection similar to a case statement.

```cobol
EVALUATE WS-STATUS
   WHEN "A" DISPLAY "ACTIVE"
   WHEN "I" DISPLAY "INACTIVE"
   WHEN OTHER DISPLAY "UNKNOWN"
END-EVALUATE
```

## PERFORM
Calls paragraphs/sections or loops inline and out-of-line blocks.

```cobol
PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
    ADD WS-IDX TO WS-SUM
END-PERFORM
```

## CONTINUE
Explicit no-op used as a legal placeholder statement.

```cobol
IF WS-FLAG = "Y"
    CONTINUE
END-IF
```

## GO
Transfers control to a target paragraph.

```cobol
GO TO ERROR-HANDLER
```

## ALTER
Changes a GO TO target at runtime (obsolete, retained for compatibility).

```cobol
ALTER ROUTE-A TO PROCEED TO ROUTE-B
```

## EXIT
Ends execution of a performed paragraph/section or method context.

```cobol
EXIT
```

## STOP
Terminates the run unit.

```cobol
STOP RUN
```

## CALL
Invokes another program or subprogram.

```cobol
CALL "CALCTAX" USING WS-AMOUNT WS-TAX
```

## CANCEL
Removes a called subprogram from memory so it can reinitialize on next CALL.

```cobol
CANCEL "CALCTAX"
```

## ENTRY
Defines an alternate entry point within a called program.

```cobol
ENTRY "CALCTAX-ALT" USING WS-AMOUNT WS-TAX
```

## SEARCH
Scans a table defined with OCCURS and indexed access.

```cobol
SEARCH CUSTOMER-TABLE
    AT END SET NOT-FOUND TO TRUE
    WHEN CUST-ID (CUST-IDX) = WS-LOOKUP-ID
         MOVE CUST-NAME (CUST-IDX) TO WS-NAME
END-SEARCH
```

## EXHIBIT
Displays diagnostic information, typically for debugging.

```cobol
EXHIBIT NAMED WS-TOTAL WS-COUNT
```

## READY TRACE
Enables procedure tracing where supported by compiler/runtime.

```cobol
READY TRACE
```

## Final complete example
This complete program demonstrates procedural flow, branching, table search, and subprogram invocation.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLOWDEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STATUS             PIC X VALUE SPACE.
       01  WS-LOOKUP-ID          PIC 9(3) VALUE 0.
       01  WS-NAME               PIC X(20).
       01  WS-IDX                PIC 9 VALUE 1.
       01  WS-SUM                PIC 9(4) VALUE 0.
       01  WS-FOUND-FLAG         PIC X VALUE "N".
           88 FOUND              VALUE "Y".
           88 NOT-FOUND          VALUE "N".
       01  CUSTOMER-TABLE.
           05 CUST-ROW OCCURS 3 TIMES INDEXED BY CUST-IDX.
              10 CUST-ID         PIC 9(3).
              10 CUST-NAME       PIC X(20).

       PROCEDURE DIVISION.
       MAIN.
           READY TRACE.
           PERFORM LOAD-TABLE.

           DISPLAY "ENTER STATUS (A/I):".
           ACCEPT WS-STATUS.

           EVALUATE WS-STATUS
             WHEN "A"
               DISPLAY "ACTIVE PATH"
             WHEN "I"
               DISPLAY "INACTIVE PATH"
             WHEN OTHER
               DISPLAY "UNKNOWN STATUS"
           END-EVALUATE.

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
               ADD WS-IDX TO WS-SUM
           END-PERFORM.
           DISPLAY "LOOP SUM=" WS-SUM.

           DISPLAY "ENTER CUSTOMER ID (101/102/103):".
           ACCEPT WS-LOOKUP-ID.
           SET NOT-FOUND TO TRUE.

           SEARCH CUSTOMER-TABLE
               AT END CONTINUE
               WHEN CUST-ID (CUST-IDX) = WS-LOOKUP-ID
                    SET FOUND TO TRUE
                    MOVE CUST-NAME (CUST-IDX) TO WS-NAME
           END-SEARCH.

           IF FOUND
               DISPLAY "FOUND CUSTOMER: " WS-NAME
           ELSE
               DISPLAY "CUSTOMER NOT FOUND"
           END-IF.

           EXHIBIT NAMED WS-STATUS WS-LOOKUP-ID WS-SUM.

           CALL "ECHOID" USING WS-LOOKUP-ID.
           CANCEL "ECHOID".

           STOP RUN.

       LOAD-TABLE.
           MOVE 101 TO CUST-ID (1).
           MOVE "ALPHA LTD" TO CUST-NAME (1).
           MOVE 102 TO CUST-ID (2).
           MOVE "BETA INC" TO CUST-NAME (2).
           MOVE 103 TO CUST-ID (3).
           MOVE "GAMMA PLC" TO CUST-NAME (3).
           EXIT.
```

