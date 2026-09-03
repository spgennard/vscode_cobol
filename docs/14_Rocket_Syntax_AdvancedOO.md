# Rocket Syntax: Advanced OO Constructs

This chapter focuses on advanced managed OO constructs used in Rocket COBOL, including namespace qualification (`::`), interfaces, enum-style patterns, delegates, and events.

## `::` namespace/type qualification
The `::` operator is used to qualify nested or static type/member access in managed contexts.

```cobol
invoke Demo.Logging.LoggerFactory::Create returning ws-logger
```

## INTERFACE-ID
Defines an interface contract that classes can implement.

```cobol
interface-id. Demo.Contracts.IPrinter.

method-id. Print abstract.
linkage section.
01 lk-text pic x any length.
procedure division using lk-text.
end method Print.

end interface Demo.Contracts.IPrinter.
```

## Class implementation with IMPLEMENTS
A class can implement one or more interfaces.

```cobol
class-id. Demo.ConsolePrinter implements type Demo.Contracts.IPrinter.

method-id. Print.
linkage section.
01 lk-text pic x any length.
procedure division using lk-text.
    display lk-text
    goback.
end method Print.

end class Demo.ConsolePrinter.
```

## Enum-style modeling
Some managed targets support enum constructs directly; where portability is needed, a common COBOL pattern is 78-level named constants.

```cobol
78 ORDER-NEW        VALUE 1.
78 ORDER-SHIPPED    VALUE 2.
78 ORDER-CANCELLED  VALUE 3.

01 ws-order-status  pic 9 value ORDER-NEW.
```

## Delegate-style callback references
Managed interop scenarios may use delegate patterns for callbacks.

```cobol
01 ws-handler object reference type System.EventHandler.
*> Handler binding is typically done through framework method calls.
```

## Event subscription pattern
Events are usually wired through `add_`/`remove_` accessor style invocations in managed interop.

```cobol
invoke ws-button "add_Click" using by value ws-handler
```

## Property override and interface property mapping
Property syntax can be used in interface-based designs.

```cobol
property-id. Name.
    getter.
        goback returning ws-name
    setter.
        move parameter-value to ws-name
end property Name.
```

## Final complete example
This complete example shows an interface, implementation class, enum-style constants, and a namespace-qualified call.

```cobol
$set sourceformat"free"

interface-id. Demo.Contracts.IMessageSink.

method-id. WriteMessage abstract.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
end method WriteMessage.

end interface Demo.Contracts.IMessageSink.

class-id. Demo.Runtime.ConsoleSink
    implements type Demo.Contracts.IMessageSink.

data division.
working-storage section.
78 SEV-INFO         VALUE 1.
78 SEV-WARN         VALUE 2.
78 SEV-ERROR        VALUE 3.
01 ws-severity      pic 9 value SEV-INFO.

method-id. WriteMessage.
linkage section.
01 lk-msg pic x any length.
procedure division using lk-msg.
    evaluate ws-severity
      when SEV-INFO
        display "[INFO] " lk-msg
      when SEV-WARN
        display "[WARN] " lk-msg
      when other
        display "[ERROR] " lk-msg
    end-evaluate
    goback.
end method WriteMessage.

method-id. SetSeverity.
linkage section.
01 lk-sev pic 9.
procedure division using lk-sev.
    move lk-sev to ws-severity
    goback.
end method SetSeverity.

end class Demo.Runtime.ConsoleSink.

identification division.
program-id. advanced-oo-runner.

environment division.
configuration section.
repository.
    class ConsoleSink as "Demo.Runtime.ConsoleSink"
    interface IMessageSink as "Demo.Contracts.IMessageSink".

data division.
working-storage section.
01 ws-sink           object reference ConsoleSink.
01 ws-contract       object reference IMessageSink.

procedure division.
main-logic.
    invoke ConsoleSink "new" returning ws-sink
    set ws-contract to ws-sink

    invoke ws-sink "SetSeverity" using by value 2
    invoke ws-contract "WriteMessage" using by value "Payment threshold reached"

    *> Namespace-qualified static call pattern example:
    *> invoke Demo.Runtime.Diagnostics::Trace using by value "done"

    stop run.
```
