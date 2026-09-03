# Rocket Syntax: Compiler Directives

## $SET SOURCEFORMAT"FREE"
Sets free-format source parsing for the compilation unit.

```cobol
$set sourceformat"free"
identification division.
program-id. demo-free.
procedure division.
    display "free format enabled".
    stop run.
```

## $SET FLAGSTD"COBOL85"
Requests standards checking against COBOL 85 while still compiling with Rocket extensions enabled.

```cobol
$set flagstd"cobol85"
identification division.
program-id. std-check.
procedure division.
    display "compile with standards diagnostics".
    stop run.
```

## $SET DIALECT"MF"
Selects the Rocket/Micro Focus dialect rules for parsing and semantics.

```cobol
$set dialect"mf"
identification division.
program-id. mf-dialect.
procedure division.
    display "dialect set to mf".
    stop run.
```

## >>DEFINE / >>IF / >>ELSE / >>END-IF
Provides compile-time conditional compilation blocks.

```cobol
>>DEFINE DEBUG-MODE
identification division.
program-id. conditional-compile.
procedure division.
>>IF DEFINED(DEBUG-MODE)
    display "compiled in debug mode".
>>ELSE
    display "compiled in release mode".
>>END-IF
    stop run.
```

## COPY ... REPLACING with Rocket directive blocks
Rocket builds commonly combine copybook replacement with conditional directives for environment-specific code.

```cobol
>>DEFINE REGION-US
copy "taxlogic.cpy"
    replacing ==:REGION:== by ==US==.
```

## Final complete example
This complete example combines Rocket directive syntax for dialect selection, source format, and conditional compilation.

```cobol
$set sourceformat"free"
$set dialect"mf"
$set flagstd"cobol85"
>>define TRACE-ON

identification division.
program-id. rocket-directive-demo.

data division.
working-storage section.
01 ws-build-mode           pic x(10).

procedure division.
main-logic.
    move "release" to ws-build-mode

>>if defined(TRACE-ON)
    move "trace" to ws-build-mode
    display "trace instrumentation compiled" 
>>else
    display "trace instrumentation not compiled"
>>end-if

    display "build mode=" ws-build-mode
    stop run.
```
