# Rocket Syntax: Interop And Data Extensions

## COMP-5
Defines native binary storage semantics commonly used for interop and high-performance arithmetic.

```cobol
01  WS-NATIVE-COUNT        PIC S9(9) COMP-5.
```

## COMP-X
Defines a raw binary storage format extension used by Rocket-family compilers.

```cobol
01  WS-RAW-FLAG            PIC X(4) COMP-X.
```

## POINTER and PROCEDURE-POINTER
Supports direct pointer and routine-pointer handling for advanced interop.

```cobol
01  WS-BUFFER-PTR          USAGE POINTER.
01  WS-ENTRY-PTR           USAGE PROCEDURE-POINTER.
```

## CALL-CONVENTION in SPECIAL-NAMES
Defines a numeric call-convention alias for external calls.

```cobol
special-names.
    call-convention 74 is stdcall.
```

## CALL using BY VALUE and BY REFERENCE
Controls argument passing style for external libraries.

```cobol
call "CalcNet" using by value ws-amount by reference ws-result
```

## ENTRY-CONVENTION on PROGRAM-ID
Declares the default external entry convention for a program.

```cobol
identification division.
program-id. calcbridge entry-convention 74.
```

## Final complete example
This complete example shows native numeric storage and external routine calling conventions.

```cobol
$set sourceformat"free"

identification division.
program-id. rocket-interop-demo entry-convention 74.

environment division.
configuration section.
special-names.
    call-convention 74 is stdcall.

data division.
working-storage section.
01 ws-amount               pic s9(9)v99 comp-5 value 1250.50.
01 ws-tax-rate             pic s9v9999 comp-5 value 0.0825.
01 ws-tax                  pic s9(7)v99 comp-5 value 0.
01 ws-net                  pic s9(9)v99 comp-5 value 0.
01 ws-raw-bytes            pic x(8) comp-x.
01 ws-buffer-ptr           usage pointer.
01 ws-proc-ptr             usage procedure-pointer.

procedure division.
main-logic.
    compute ws-tax = ws-amount * ws-tax-rate
    compute ws-net = ws-amount + ws-tax

    display "amount=" ws-amount
    display "tax=" ws-tax
    display "net=" ws-net

    call "ExternalAudit" using by value ws-net by reference ws-raw-bytes

    stop run.
```
