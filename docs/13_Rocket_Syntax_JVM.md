# Rocket Syntax: JVM Managed COBOL

This chapter focuses on Rocket managed COBOL syntax patterns used when targeting the JVM.

This content aligns with Rocket's managed-language comparison guide and emphasizes Java-style interop patterns in managed COBOL.

## CLASS-ID with JVM class name
Declares a managed COBOL class mapped to a JVM class name.

```cobol
class-id. Demo.OrderService as "demo.OrderService".
```

## REPOSITORY for Java classes
Maps Java classes so they can be referenced directly in COBOL.

```cobol
repository.
    class ArrayList as "java.util.ArrayList"
    class System    as "java.lang.System".
```

## OBJECT REFERENCE with Java classes
Stores references to Java objects for method invocation.

```cobol
01 orders object reference ArrayList.
```

## INVOKE constructor and instance methods on Java objects
Creates Java objects and calls methods using managed COBOL syntax.

```cobol
invoke ArrayList "new" returning orders
invoke orders "add" using by value "ORD-1001"
invoke orders "add" using by value "ORD-1002"
```

## INVOKE static Java methods
Calls static members from Java runtime classes.

```cobol
invoke System "currentTimeMillis" returning ws-epoch-ms
```

## METHOD-ID and RETURNING for Java-facing APIs
Defines methods that return values suitable for Java callers.

```cobol
method-id. CountOrders returning order-count.
01 order-count pic s9(9) comp-5.
procedure division.
    invoke orders "size" returning order-count
    goback.
end method CountOrders.
```

## Java collection behavior compared to fixed COBOL tables
Java collections can grow dynamically, while COBOL OCCURS tables are fixed unless redesigned. Managed COBOL can interoperate with Java collections when dynamic sizing is needed.

```cobol
repository.
    class ArrayList as "java.util.ArrayList".

01 ws-orders object reference ArrayList.
01 ws-count  pic s9(9) comp-5.

invoke ArrayList "new" returning ws-orders
invoke ws-orders "add" using by value "ORD-1001"
invoke ws-orders "add" using by value "ORD-1002"
invoke ws-orders "size" returning ws-count
display "COUNT=" ws-count
```

## Async behavior note for JVM
The comparison guide notes that Java does not provide coroutine syntax like C# async/await; similar behavior is typically done with executors/futures. In COBOL-on-JVM, the usual pattern is interop with Java concurrency classes.

```cobol
repository.
    class CompletableFuture as "java.util.concurrent.CompletableFuture".

*> Example placeholder for framework interop pattern:
*> invoke CompletableFuture "completedFuture" using by value "OK" returning ws-future
```

## Final complete example
This complete example creates a Java list, loads sample orders, reports metadata, and reads collection entries.

```cobol
$set sourceformat"free"

class-id. Demo.OrderCollector as "demo.OrderCollector".

environment division.
configuration section.
repository.
    class ArrayList as "java.util.ArrayList"
    class System    as "java.lang.System".

data division.
working-storage section.
01 ws-orders              object reference ArrayList.
01 ws-count               pic s9(9) comp-5 value 0.
01 ws-epoch-ms            pic s9(18) comp-5 value 0.
01 ws-item                pic x(20).

method-id. Run static.
procedure division.
    invoke ArrayList "new" returning ws-orders

    invoke ws-orders "add" using by value "ORD-1001"
    invoke ws-orders "add" using by value "ORD-1002"
    invoke ws-orders "add" using by value "ORD-1003"

    invoke ws-orders "size" returning ws-count
    invoke System "currentTimeMillis" returning ws-epoch-ms

    display "ORDER COUNT=" ws-count
    display "EPOCH MS=" ws-epoch-ms

    invoke ws-orders "get" using by value 0 returning ws-item
    display "FIRST ORDER=" ws-item

    goback.
end method Run.

end class Demo.OrderCollector.

identification division.
program-id. jvm-runner.

procedure division.
main-logic.
    invoke Demo.OrderCollector "Run"
    stop run.
```
