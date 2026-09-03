# Managed Language Advanced Patterns: C#, COBOL, VB.NET, Java

This chapter fills the advanced comparison gaps by mapping common managed-language constructs into Rocket COBOL patterns.

## 1) Generics

C#
```csharp
List<string> names = new List<string>();
names.Add("A");
```

Rocket COBOL
```cobol
repository.
    class ListString as "System.Collections.Generic.List`1[System.String]".

01 ws-names object reference ListString.

invoke ListString "new" returning ws-names
invoke ws-names "Add" using by value "A"
```

VB.NET note:
- `Dim names As New List(Of String)` maps to the same managed generic type.

Java note:
- `List<String> names = new ArrayList<>();` uses type erasure at runtime.

## 2) Inheritance, virtual, override

C#
```csharp
public class BaseService
{
    public virtual string Name() => "Base";
}

public class InvoiceService : BaseService
{
    public override string Name() => "Invoice";
}
```

Rocket COBOL
```cobol
class-id. Demo.BaseService.
method-id. Name.
procedure division returning ret-name.
01 ret-name pic x(20).
    move "Base" to ret-name
    goback.
end method Name.
end class Demo.BaseService.

class-id. Demo.InvoiceService inherits Demo.BaseService.
method-id. Name override.
procedure division returning ret-name.
01 ret-name pic x(20).
    move "Invoice" to ret-name
    goback.
end method Name.
end class Demo.InvoiceService.
```

VB.NET note:
- `Overrides` and `Overridable` are the matching keywords.

Java note:
- uses `extends` and `@Override`.

## 3) Constructor overloading

C#
```csharp
public class Money
{
    public Money() { }
    public Money(decimal amount) { }
}
```

Rocket COBOL
```cobol
class-id. Demo.Money.
working-storage section.
01 ws-amount pic s9(9)v99 value 0.

method-id. New.
procedure division.
    move 0 to ws-amount
    goback.
end method New.

method-id. New.
linkage section.
01 lk-amount pic s9(9)v99.
procedure division using lk-amount.
    move lk-amount to ws-amount
    goback.
end method New.

end class Demo.Money.
```

VB.NET note:
- multiple `Sub New(...)` overloads.

Java note:
- multiple constructors with different signatures.

## 4) Method overloading

C#
```csharp
public int Add(int a, int b) => a + b;
public decimal Add(decimal a, decimal b) => a + b;
```

Rocket COBOL
```cobol
method-id. Add.
linkage section.
01 lk-a pic s9(9) comp-5.
01 lk-b pic s9(9) comp-5.
procedure division using lk-a lk-b returning lk-r.
01 lk-r pic s9(9) comp-5.
    compute lk-r = lk-a + lk-b
    goback.
end method Add.

method-id. Add.
linkage section.
01 lk-da pic s9(9)v99.
01 lk-db pic s9(9)v99.
procedure division using lk-da lk-db returning lk-dr.
01 lk-dr pic s9(9)v99.
    compute lk-dr = lk-da + lk-db
    goback.
end method Add.
```

VB.NET note:
- `Overloads Function Add(...)`.

Java note:
- overload resolution by parameter types.

## 5) Indexers and indexed access

C#
```csharp
public string this[int i]
{
    get => _items[i];
    set => _items[i] = value;
}
```

Rocket COBOL (table + accessor methods)
```cobol
01 ws-items.
   05 ws-item occurs 100 times pic x(30).

method-id. GetAt.
linkage section.
01 lk-i pic 9(3) comp-5.
procedure division using lk-i returning lk-value.
01 lk-value pic x(30).
    move ws-item (lk-i) to lk-value
    goback.
end method GetAt.

method-id. SetAt.
linkage section.
01 lk-i pic 9(3) comp-5.
01 lk-value pic x(30).
procedure division using lk-i lk-value.
    move lk-value to ws-item (lk-i)
    goback.
end method SetAt.
```

VB.NET note:
- default properties provide indexer-like access.

Java note:
- no indexer syntax; use `get(i)` / `set(i,v)` methods.

## 6) Operator overloading

C#
```csharp
public static Money operator +(Money a, Money b) => new(a.Amount + b.Amount);
```

Rocket COBOL
```cobol
*> Use explicit method-based pattern instead of operator symbols.
method-id. AddMoney static.
linkage section.
01 lk-a pic s9(9)v99.
01 lk-b pic s9(9)v99.
procedure division using lk-a lk-b returning lk-r.
01 lk-r pic s9(9)v99.
    compute lk-r = lk-a + lk-b
    goback.
end method AddMoney.
```

VB.NET note:
- `Operator +` exists.

Java note:
- no user-defined operator overloading.

## 7) Exception taxonomy mapping

C#
```csharp
catch (ArgumentException ex) { ... }
catch (InvalidOperationException ex) { ... }
catch (Exception ex) { ... }
```

Rocket COBOL managed pattern
```cobol
repository.
    class Exception as "System.Exception"
    class ArgumentException as "System.ArgumentException"
    class InvalidOperationException as "System.InvalidOperationException".

01 ws-ex object reference Exception.

try
    invoke ws-service "RunStep"
catch exception
    set ws-ex to exception-object
    *> Branch by exception type/name according to runtime support
    display "ERROR: managed exception caught"
    raise exception ws-ex
end-try
```

Portable pattern:
- map exception classes to return codes/messages at service boundaries.

## 8) Async composition

C#
```csharp
await Task.WhenAll(t1, t2);
```

Rocket COBOL (.NET interop style)
```cobol
repository.
    class Task as "System.Threading.Tasks.Task".

*> Placeholder pattern: call framework combinators and then continue flow.
*> invoke Task "WhenAll" using by value ws-task-array returning ws-whenall
```

Java note:
```java
CompletableFuture.allOf(f1, f2).join();
```

Rocket COBOL (JVM interop style)
```cobol
repository.
    class CompletableFuture as "java.util.concurrent.CompletableFuture".

*> invoke CompletableFuture "allOf" using ... returning ws-all
```

## 9) LINQ/lambda style mapping

C#
```csharp
var even = nums.Where(n => n % 2 == 0).Select(n => n * 10).ToList();
```

Rocket COBOL
```cobol
*> Use explicit PERFORM loops for portable transformation logic.
01 ws-i pic 9(3) comp-5.
01 ws-out-idx pic 9(3) comp-5 value 1.

perform varying ws-i from 1 by 1 until ws-i > ws-count
    if function mod(ws-num (ws-i), 2) = 0
        compute ws-out (ws-out-idx) = ws-num (ws-i) * 10
        add 1 to ws-out-idx
    end-if
end-perform
```

VB.NET note:
- query comprehensions compile to LINQ operators.

Java note:
- Streams API gives similar pipeline style.

## Final complete example

```cobol
$set sourceformat"free"

class-id. Demo.AdvancedInvoiceService.

environment division.
configuration section.
repository.
    class ListString as "System.Collections.Generic.List`1[System.String]".

data division.
working-storage section.
01 ws-items object reference ListString.
01 ws-i     pic 9(3) comp-5 value 1.
01 ws-line  pic x(40).

method-id. New.
procedure division.
    invoke ListString "new" returning ws-items
    goback.
end method New.

method-id. AddLine.
linkage section.
01 lk-line pic x any length.
procedure division using lk-line.
    invoke ws-items "Add" using by value lk-line
    goback.
end method AddLine.

method-id. PrintAll.
procedure division.
    perform varying ws-i from 0 by 1 until ws-i >= 3
        invoke ws-items "get_Item" using by value ws-i returning ws-line
        display ws-line
    end-perform
    goback.
end method PrintAll.

end class Demo.AdvancedInvoiceService.
```
