# COBOL 85 Verbs: Data And Arithmetic

## ACCEPT
Reads input into a data item, often from terminal input or environment/date sources.

```cobol
ACCEPT WS-USER-NAME
```

## DISPLAY
Writes text or data values to the screen or console output.

```cobol
DISPLAY "TOTAL=" WS-TOTAL
```

## MOVE
Copies data from one item to another with COBOL conversion rules.

```cobol
MOVE CUSTOMER-NAME TO WS-NAME
```

## INITIALIZE
Sets a group item or field set to default values based on data types.

```cobol
INITIALIZE CUSTOMER-RECORD
```

## ADD
Adds one or more values to a target.

```cobol
ADD WS-AMOUNT TO WS-TOTAL
```

## SUBTRACT
Subtracts values from a target.

```cobol
SUBTRACT WS-DISCOUNT FROM WS-TOTAL
```

## MULTIPLY
Multiplies numeric values.

```cobol
MULTIPLY WS-PRICE BY WS-QUANTITY GIVING WS-EXTENDED
```

## DIVIDE
Divides numeric values, optionally with remainder.

```cobol
DIVIDE WS-NUMERATOR BY WS-DENOMINATOR
    GIVING WS-QUOTIENT
    REMAINDER WS-REMAINDER
```

## COMPUTE
Performs arithmetic expressions in one statement.

```cobol
COMPUTE WS-NET = (WS-HOURS * WS-RATE) - WS-TAX
```

## SET
Assigns values to index items, pointers, switches, and condition names.

```cobol
SET EOF-SW TO TRUE
```

## INSPECT
Counts or replaces characters/substrings in text data.

```cobol
INSPECT WS-LINE
    TALLYING WS-SPACES FOR ALL SPACE
```

## STRING
Concatenates multiple fields into one destination string.

```cobol
STRING WS-FIRST-NAME DELIMITED BY SPACE
       " " DELIMITED BY SIZE
       WS-LAST-NAME DELIMITED BY SPACE
  INTO WS-FULL-NAME
END-STRING
```

## UNSTRING
Splits one source string into multiple target fields.

```cobol
UNSTRING WS-CSV-LINE DELIMITED BY ","
  INTO WS-COL-1 WS-COL-2 WS-COL-3
END-UNSTRING
```

## TRANSFORM
Converts characters using a mapping from one character set to another.

```cobol
MOVE "abcXYZ" TO WS-TEXT
TRANSFORM WS-TEXT
    CHARACTERS FROM "abcXYZ"
             TO "ABCxyz"
```

## Final complete example
This complete program shows the data and arithmetic verbs working together in a small invoice calculator.

```cobol
     IDENTIFICATION DIVISION.
     PROGRAM-ID. DATAARITHM.

     DATA DIVISION.
     WORKING-STORAGE SECTION.
     01  WS-INPUT-LINE         PIC X(40).
     01  WS-CUSTOMER-ID        PIC X(10).
     01  WS-ITEM-NAME          PIC X(20).
     01  WS-QTY                PIC 9(4) VALUE 0.
     01  WS-PRICE              PIC 9(5)V99 VALUE 0.
     01  WS-DISCOUNT-PCT       PIC 9V99 VALUE 0.
     01  WS-SUBTOTAL           PIC 9(7)V99 VALUE 0.
     01  WS-DISCOUNT           PIC 9(7)V99 VALUE 0.
     01  WS-NET                PIC 9(7)V99 VALUE 0.
     01  WS-SPACES             PIC 9(4) VALUE 0.
     01  WS-REMAINDER          PIC 9 VALUE 0.
     01  WS-QUOTIENT           PIC 9(4) VALUE 0.
     01  WS-STATUS-TEXT        PIC X(30).
     01  WS-TEXT               PIC X(10).

     PROCEDURE DIVISION.
     MAIN-LOGIC.
       INITIALIZE WS-CUSTOMER-ID WS-ITEM-NAME WS-STATUS-TEXT.

       DISPLAY "ENTER: CUSTID,ITEM,QTY,PRICE (e.g. C001,BOOK,2,19.95)".
       ACCEPT WS-INPUT-LINE.

       UNSTRING WS-INPUT-LINE DELIMITED BY ","
         INTO WS-CUSTOMER-ID WS-ITEM-NAME WS-QTY WS-PRICE
       END-UNSTRING.

       INSPECT WS-ITEM-NAME
         TALLYING WS-SPACES FOR ALL SPACE.

       MOVE "abcXYZ" TO WS-TEXT.
       TRANSFORM WS-TEXT
         CHARACTERS FROM "abcXYZ"
            TO "ABCxyz".

       COMPUTE WS-SUBTOTAL = WS-QTY * WS-PRICE.
       MOVE 0.10 TO WS-DISCOUNT-PCT.
       MULTIPLY WS-SUBTOTAL BY WS-DISCOUNT-PCT GIVING WS-DISCOUNT.
       SUBTRACT WS-DISCOUNT FROM WS-SUBTOTAL GIVING WS-NET.
       ADD 0 TO WS-NET.

       DIVIDE WS-QTY BY 2 GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.

       SET ADDRESS OF WS-STATUS-TEXT TO ADDRESS OF WS-STATUS-TEXT.
       STRING "CUSTOMER " DELIMITED BY SIZE
          WS-CUSTOMER-ID DELIMITED BY SPACE
          " NET=" DELIMITED BY SIZE
          INTO WS-STATUS-TEXT
       END-STRING.

       DISPLAY "ITEM=" WS-ITEM-NAME " QTY=" WS-QTY " PRICE=" WS-PRICE.
       DISPLAY "SUBTOTAL=" WS-SUBTOTAL " DISCOUNT=" WS-DISCOUNT.
       DISPLAY "NET=" WS-NET " HALF-QTY=" WS-QUOTIENT " R=" WS-REMAINDER.
       DISPLAY WS-STATUS-TEXT.
       DISPLAY "TRANSFORM SAMPLE=" WS-TEXT.

       STOP RUN.
```
