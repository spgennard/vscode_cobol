# COBOL 85 Verbs: Files, Sort, And Output

## OPEN
Opens files in INPUT, OUTPUT, I-O, or EXTEND mode.

```cobol
OPEN INPUT CUSTOMER-FILE
     OUTPUT REPORT-FILE
```

## CLOSE
Closes one or more open files.

```cobol
CLOSE CUSTOMER-FILE REPORT-FILE
```

## READ
Reads the next or keyed record from an open input or I-O file.

```cobol
READ CUSTOMER-FILE
    AT END SET EOF-SW TO TRUE
END-READ
```

## WRITE
Writes a new record to an output or extend file.

```cobol
WRITE REPORT-REC FROM WS-REPORT-LINE
```

## REWRITE
Updates the current record in an I-O file.

```cobol
REWRITE CUSTOMER-REC FROM WS-CUSTOMER-REC
```

## DELETE
Removes the current record from an indexed or relative file.

```cobol
DELETE CUSTOMER-FILE RECORD
```

## START
Positions an indexed or relative file at a key before sequential READ NEXT.

```cobol
START CUSTOMER-FILE KEY >= WS-START-KEY
    INVALID KEY SET NOT-FOUND TO TRUE
END-START
```

## USE
Declares declarative error-handling procedures for file exceptions.

```cobol
DECLARATIVES.
FILE-ERROR SECTION.
    USE AFTER STANDARD ERROR PROCEDURE ON CUSTOMER-FILE.
FILE-ERROR-PARA.
    DISPLAY "FILE STATUS=" WS-FILE-STATUS.
END DECLARATIVES.
```

## SORT
Sorts records using a sort file and input/output procedures.

```cobol
SORT SORT-WORK-FILE
    ON ASCENDING KEY SORT-KEY
    INPUT PROCEDURE  IS LOAD-SORT
    OUTPUT PROCEDURE IS WRITE-SORTED
```

## MERGE
Merges already sorted input files into one ordered output stream.

```cobol
MERGE SORT-WORK-FILE
    ON ASCENDING KEY SORT-KEY
    USING SORTED-IN-1 SORTED-IN-2
    GIVING MERGED-OUT
```

## RELEASE
Passes a record into a SORT input procedure.

```cobol
RELEASE SORT-REC FROM WS-SORT-REC
```

## RETURN
Retrieves a sorted record from the SORT output procedure.

```cobol
RETURN SORT-WORK-FILE
    AT END SET SORT-EOF TO TRUE
END-RETURN
```

## Final complete example
This complete program reads input records, sorts by key, and writes a report output file.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILESORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "customer_in.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "customer_out.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORTWK ASSIGN TO SORT-WORK.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  IN-REC.
           05 IN-KEY             PIC X(05).
           05 IN-NAME            PIC X(20).
       FD  OUTFILE.
       01  OUT-REC               PIC X(40).
       SD  SORTWK.
       01  SORT-REC.
           05 SORT-KEY           PIC X(05).
           05 SORT-NAME          PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-EOF                PIC X VALUE "N".
           88 EOF-SW             VALUE "Y".
       01  WS-SORT-EOF           PIC X VALUE "N".
           88 SORT-EOF           VALUE "Y".

       PROCEDURE DIVISION.
       MAIN.
           SORT SORTWK
               ON ASCENDING KEY SORT-KEY
               INPUT PROCEDURE IS LOAD-SORT
               OUTPUT PROCEDURE IS WRITE-SORTED.
           STOP RUN.

       LOAD-SORT.
           OPEN INPUT INFILE.
           PERFORM UNTIL EOF-SW
               READ INFILE
                   AT END
                       SET EOF-SW TO TRUE
                   NOT AT END
                       MOVE IN-KEY TO SORT-KEY
                       MOVE IN-NAME TO SORT-NAME
                       RELEASE SORT-REC
               END-READ
           END-PERFORM.
           CLOSE INFILE.

       WRITE-SORTED.
           OPEN OUTPUT OUTFILE.
           PERFORM UNTIL SORT-EOF
               RETURN SORTWK
                   AT END
                       SET SORT-EOF TO TRUE
                   NOT AT END
                       STRING SORT-KEY DELIMITED BY SIZE
                              " " DELIMITED BY SIZE
                              SORT-NAME DELIMITED BY SPACE
                           INTO OUT-REC
                       END-STRING
                       WRITE OUT-REC
               END-RETURN
           END-PERFORM.
           CLOSE OUTFILE.
```

