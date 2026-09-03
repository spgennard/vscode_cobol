# Rocket Syntax: .NET Managed COBOL

This chapter focuses on Rocket managed COBOL syntax patterns used when targeting .NET.

This content aligns with Rocket's managed-language comparison guide and focuses on practical .NET interop equivalents for common C#/VB patterns.

## CLASS-ID with .NET type name
Declares a managed class and maps it to a fully qualified .NET type name.

```cobol
class-id. Demo.InvoiceService as "Demo.InvoiceService".
```

## REPOSITORY for .NET framework classes
Binds COBOL class names to .NET classes for interop.

```cobol
repository.
    class StringBuilder as "System.Text.StringBuilder"
    class DateTime      as "System.DateTime".
```

## OBJECT REFERENCE with .NET classes
Holds references to managed objects.

```cobol
01 sb object reference StringBuilder.
01 now object reference DateTime.
```

## INVOKE constructor and instance methods
Creates and uses .NET objects from COBOL.

```cobol
invoke StringBuilder "new" returning sb
invoke sb "Append" using by value "INV-"
invoke sb "Append" using by value "1001"
```

## INVOKE static members
Calls static methods on .NET classes.

```cobol
invoke DateTime "Now" returning now
```

## PROPERTY-ID in managed class design
Defines get/set property semantics in OO managed COBOL.

```cobol
property-id. InvoiceNumber.
    getter.
        goback returning ws-inv-no
    setter.
        move parameter-value to ws-inv-no
end property InvoiceNumber.
```

## Array-style modeling compared to C# arrays
In managed COBOL, fixed-size lists are commonly modeled with OCCURS. Unlike C#, resizing is not automatic.

```cobol
01 ws-nums.
   05 ws-num occurs 3 times pic s9(9) comp-5.
01 ws-i                     pic 9 value 1.

move 10 to ws-num (1)
move 20 to ws-num (2)
move 30 to ws-num (3)

perform varying ws-i from 1 by 1 until ws-i > 3
    display "NUM(" ws-i ")=" ws-num (ws-i)
end-perform
```

## Managed async behavior note
The comparison guide highlights async patterns in managed languages. For COBOL, the common approach is to call framework APIs rather than rely on language-level coroutine syntax.

```cobol
repository.
    class Task as "System.Threading.Tasks.Task".

invoke Task "Delay" using by value 1000 returning task-ref
*> Continue with synchronous flow or framework-specific await pattern where supported
```

## Final complete example
This complete example shows managed COBOL creating .NET objects, formatting data, and iterating an OCCURS array in a comparison-friendly style.

```cobol
$set sourceformat"free"

class-id. Demo.InvoicePrinter as "Demo.InvoicePrinter".

environment division.
configuration section.
repository.
    class StringBuilder as "System.Text.StringBuilder"
    class DateTime      as "System.DateTime".

data division.
working-storage section.
01 ws-inv-no              pic x(12) value "INV-1001".
01 ws-customer            pic x(30) value "ACME INDUSTRIES".
01 ws-total               pic 9(7)v99 value 1520.45.
01 ws-nums.
    05 ws-num occurs 3 times pic s9(9) comp-5.
01 ws-i                   pic 9 value 1.
01 ws-now                 object reference DateTime.
01 sb                     object reference StringBuilder.
01 ws-line                pic x(120).

method-id. PrintLine static.
procedure division.
    move 11 to ws-num (1)
    move 22 to ws-num (2)
    move 33 to ws-num (3)

    invoke StringBuilder "new" returning sb
    invoke DateTime "Now" returning ws-now
    invoke sb "Append" using by value ws-inv-no
    invoke sb "Append" using by value " | "
    invoke sb "Append" using by value ws-customer
    invoke sb "Append" using by value " | TOTAL="
    invoke sb "Append" using by value ws-total
    invoke sb "ToString" returning ws-line

    display ws-line

    perform varying ws-i from 1 by 1 until ws-i > 3
        display "ARR(" ws-i ")=" ws-num (ws-i)
    end-perform

    goback.
end method PrintLine.

end class Demo.InvoicePrinter.

identification division.
program-id. dotnet-runner.

procedure division.
main-logic.
    invoke Demo.InvoicePrinter "PrintLine"
    stop run.
```
