# C# Developer Guide To Rocket COBOL

This guide is for developers who already know C# and need to become productive in Rocket COBOL managed syntax.

It complements the verb and syntax references by translating common C# thinking into COBOL patterns.

## 1) Program structure: class and entry point

C#
```csharp
namespace Demo;

public class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("Hello");
    }
}
```

Rocket COBOL
```cobol
$set sourceformat"free"

identification division.
program-id. hello-main.

procedure division.
main-logic.
    display "Hello"
    stop run.
```

What to keep in mind:
- COBOL still uses named divisions and sections.
- `PROGRAM-ID` is your executable unit name.
- `STOP RUN` exits the run unit.

## 2) Variables and types

C#
```csharp
int count = 10;
decimal total = 123.45m;
string name = "ACME";
```

Rocket COBOL
```cobol
01 ws-count     pic s9(9) comp-5 value 10.
01 ws-total     pic s9(7)v99 value 123.45.
01 ws-name      pic x(20) value "ACME".
```

What to keep in mind:
- `PIC` defines shape/size.
- `COMP-5` is common for native binary numeric storage.
- strings are fixed-length unless using managed interop types.

## 3) Properties

C#
```csharp
public string Name { get; set; } = string.Empty;
```

Rocket COBOL
```cobol
01 ws-name pic x(30) value spaces.

property-id. Name.
    getter.
        goback returning ws-name
    setter.
        move parameter-value to ws-name
end property Name.
```

What to keep in mind:
- property syntax is explicit with `getter`/`setter` blocks.
- storage is often still represented in working-storage items.

## 4) Interfaces and implementation

C#
```csharp
public interface IMessageSink
{
    void WriteMessage(string message);
}

public sealed class ConsoleSink : IMessageSink
{
    public void WriteMessage(string message) => Console.WriteLine(message);
}
```

Rocket COBOL
```cobol
interface-id. Demo.Contracts.IMessageSink.

method-id. WriteMessage abstract.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
end method WriteMessage.

end interface Demo.Contracts.IMessageSink.

class-id. Demo.Runtime.ConsoleSink
    implements type Demo.Contracts.IMessageSink.

method-id. WriteMessage.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
    display lk-msg
    goback.
end method WriteMessage.

end class Demo.Runtime.ConsoleSink.
```

What to keep in mind:
- `INTERFACE-ID` + abstract methods model interface contracts.
- `implements type` maps class-to-interface implementation.

## 5) Static access and namespace qualification (`::`)

C#
```csharp
var value = LoggerFactory.Create();
```

Rocket COBOL
```cobol
invoke Demo.Logging.LoggerFactory::Create returning ws-logger
```

What to keep in mind:
- `::` is used for qualified managed member access.

## 6) Arrays and collections

C#
```csharp
var nums = new[] { 1, 2, 3 };
for (var i = 0; i < nums.Length; i++)
{
    Console.WriteLine(nums[i]);
}
```

Rocket COBOL (fixed table)
```cobol
01 ws-nums.
   05 ws-num occurs 3 times pic s9(9) comp-5.
01 ws-i pic 9 value 1.

move 1 to ws-num (1)
move 2 to ws-num (2)
move 3 to ws-num (3)

perform varying ws-i from 1 by 1 until ws-i > 3
    display ws-num (ws-i)
end-perform
```

Rocket COBOL (Java/.NET collection interop pattern)
```cobol
repository.
    class ArrayList as "java.util.ArrayList".

01 ws-list object reference ArrayList.
01 ws-count pic s9(9) comp-5.

invoke ArrayList "new" returning ws-list
invoke ws-list "add" using by value "A"
invoke ws-list "add" using by value "B"
invoke ws-list "size" returning ws-count
```

What to keep in mind:
- `OCCURS` is fixed-size.
- for dynamic sizing, interop collections are common in managed targets.

## 7) String composition

C#
```csharp
var line = $"{invoiceNo} | {customer} | TOTAL={total}";
```

Rocket COBOL
```cobol
string ws-inv-no delimited by space
       " | " delimited by size
       ws-customer delimited by space
       " | TOTAL=" delimited by size
       ws-total delimited by size
  into ws-line
end-string
```

What to keep in mind:
- COBOL string assembly is explicit with delimiters.

## 8) Method calls and parameter passing

C#
```csharp
TaxResult result = CalcTax(amount, rate);
```

Rocket COBOL
```cobol
call "CalcTax" using by value ws-amount by value ws-rate by reference ws-result
```

What to keep in mind:
- argument mode (`by value` vs `by reference`) matters for interop.

## 9) Conditional logic and pattern branching

C#
```csharp
if (status == "A")
    Console.WriteLine("Active");
else if (status == "I")
    Console.WriteLine("Inactive");
else
    Console.WriteLine("Unknown");
```

Rocket COBOL
```cobol
evaluate ws-status
  when "A"
    display "Active"
  when "I"
    display "Inactive"
  when other
    display "Unknown"
end-evaluate
```

What to keep in mind:
- `EVALUATE` is often the clearest mapping from switch/multi-branch logic.

## 10) Events and delegates (managed interop)

C#
```csharp
button.Click += OnClick;
```

Rocket COBOL
```cobol
01 ws-handler object reference type System.EventHandler.
invoke ws-button "add_Click" using by value ws-handler
```

What to keep in mind:
- event wiring is usually via managed interop accessors (`add_`/`remove_`).

## 11) Enum-style constants

C#
```csharp
public enum Severity { Info = 1, Warn = 2, Error = 3 }
```

Rocket COBOL (portable pattern)
```cobol
78 SEV-INFO   VALUE 1.
78 SEV-WARN   VALUE 2.
78 SEV-ERROR  VALUE 3.

01 ws-severity pic 9 value SEV-INFO.
```

What to keep in mind:
- 78-level constants provide a portable enum-like style.

## 12) Async expectations

C#
```csharp
await Task.Delay(1000);
```

Rocket COBOL managed pattern
```cobol
repository.
    class Task as "System.Threading.Tasks.Task".

invoke Task "Delay" using by value 1000 returning ws-task
*> Continue with framework-specific managed flow
```

What to keep in mind:
- async/await language syntax does not map 1:1.
- managed interop with platform async APIs is the usual approach.

## 13) Exceptions and error propagation

C#
```csharp
try
{
    service.Process(invoice);
}
catch (Exception ex)
{
    logger.LogError(ex, "Process failed");
    throw;
}
```

Rocket COBOL (.NET managed interop style)
```cobol
repository.
    class Exception as "System.Exception".

01 ws-ex object reference Exception.

try
    invoke ws-service "ProcessInvoice" using by value ws-invoice-id
catch exception
    set ws-ex to exception-object
    display "Process failed"
    raise exception ws-ex
end-try
```

Rocket COBOL (portable status-code style)
```cobol
01 ws-rc                pic s9(4) comp-5 value 0.
01 ws-error-msg         pic x(80) value spaces.

call "PROCESSINV" using by reference ws-invoice-id by reference ws-rc by reference ws-error-msg

if ws-rc not = 0
    display "Process failed rc=" ws-rc " msg=" ws-error-msg
    move 12 to return-code
    goback
end-if
```

What to keep in mind:
- In managed mode, `try/catch` with framework exceptions is available.
- In mixed/legacy systems, explicit return codes and message fields are still common.
- Use one strategy consistently at service boundaries to keep error handling predictable.

## 14) Dependency injection style patterns

C#
```csharp
public sealed class InvoiceService
{
    private readonly IRepository _repo;
    private readonly ILogger _log;

    public InvoiceService(IRepository repo, ILogger log)
    {
        _repo = repo;
        _log = log;
    }
}
```

Rocket COBOL
```cobol
interface-id. Demo.Contracts.IRepository.
method-id. Save abstract.
linkage section.
01 lk-id pic x any length.
procedure division using lk-id.
end method Save.
end interface Demo.Contracts.IRepository.

interface-id. Demo.Contracts.ILogger.
method-id. Info abstract.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
end method Info.
end interface Demo.Contracts.ILogger.

class-id. Demo.Runtime.InvoiceService.

working-storage section.
01 ws-repo object reference type Demo.Contracts.IRepository.
01 ws-log  object reference type Demo.Contracts.ILogger.

method-id. New.
linkage section.
01 lk-repo object reference type Demo.Contracts.IRepository.
01 lk-log  object reference type Demo.Contracts.ILogger.
procedure division using lk-repo lk-log.
    set ws-repo to lk-repo
    set ws-log  to lk-log
    goback.
end method New.

method-id. Process.
linkage section.
01 lk-id pic x any length.
procedure division using lk-id.
    invoke ws-log "Info" using by value "Processing invoice"
    invoke ws-repo "Save" using by value lk-id
    goback.
end method Process.

end class Demo.Runtime.InvoiceService.
```

What to keep in mind:
- Constructor-style dependency passing can be modeled with `New` methods.
- Prefer interfaces for collaboration points (repo, logger, gateways).
- Keep object graph creation in one composition root program/class.

## 15) Logging abstractions

C#
```csharp
public interface ILogger
{
    void Info(string message);
    void Error(string message);
}
```

Rocket COBOL
```cobol
interface-id. Demo.Contracts.ILogger.

method-id. Info abstract.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
end method Info.

method-id. Error abstract.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
end method Error.

end interface Demo.Contracts.ILogger.
```

Concrete console logger:

```cobol
class-id. Demo.Runtime.ConsoleLogger
    implements type Demo.Contracts.ILogger.

method-id. Info.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
    display "[INFO] " lk-msg
    goback.
end method Info.

method-id. Error.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
    display "[ERROR] " lk-msg
    goback.
end method Error.

end class Demo.Runtime.ConsoleLogger.
```

What to keep in mind:
- Treat logging as an interface, not a direct framework call in business logic.
- Use severity conventions consistently (`INFO`, `WARN`, `ERROR`).
- This keeps business logic testable and portable across host environments.

## Final complete example: C# style service flow in Rocket COBOL

```cobol
$set sourceformat"free"

interface-id. Demo.Contracts.IInvoiceSink.

method-id. WriteInvoice abstract.
linkage section.
01 lk-line pic x any length.
procedure division using lk-line.
end method WriteInvoice.

end interface Demo.Contracts.IInvoiceSink.

class-id. Demo.Runtime.ConsoleInvoiceSink
    implements type Demo.Contracts.IInvoiceSink.

method-id. WriteInvoice.
linkage section.
01 lk-line pic x any length.
procedure division using lk-line.
    display lk-line
    goback.
end method WriteInvoice.

end class Demo.Runtime.ConsoleInvoiceSink.

identification division.
program-id. csharp-minded-cobol.

environment division.
configuration section.
repository.
    class ConsoleInvoiceSink as "Demo.Runtime.ConsoleInvoiceSink"
    interface IInvoiceSink    as "Demo.Contracts.IInvoiceSink".

data division.
working-storage section.
78 SEV-INFO                VALUE 1.
01 ws-severity             pic 9 value SEV-INFO.
01 ws-inv-no               pic x(12) value "INV-2001".
01 ws-customer             pic x(30) value "ALPINE STORES".
01 ws-total                pic 9(7)v99 value 845.19.
01 ws-line                 pic x(120).
01 ws-sink                 object reference ConsoleInvoiceSink.
01 ws-contract             object reference IInvoiceSink.

procedure division.
main-logic.
    invoke ConsoleInvoiceSink "new" returning ws-sink
    set ws-contract to ws-sink

    string ws-inv-no delimited by space
           " | " delimited by size
           ws-customer delimited by space
           " | TOTAL=" delimited by size
           ws-total delimited by size
      into ws-line
    end-string

    evaluate ws-severity
      when SEV-INFO
        invoke ws-contract "WriteInvoice" using by value ws-line
      when other
        display "Skipped"
    end-evaluate

    stop run.
```

## Recommended reading order

1. `11_Rocket_Syntax_OOAndManagedExtensions.md`
2. `12_Rocket_Syntax_DotNet.md`
3. `13_Rocket_Syntax_JVM.md`
4. `14_Rocket_Syntax_AdvancedOO.md`
5. This guide
