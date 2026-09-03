# Rocket Syntax: OO And Managed Extensions

## CLASS-ID / END CLASS
Declares a COBOL class definition unit.

```cobol
class-id. InvoiceService.
end class InvoiceService.
```

## METHOD-ID / END METHOD
Defines a method body inside an OO COBOL class.

```cobol
method-id. GetVersion.
    display "InvoiceService v1".
end method GetVersion.
```

## OBJECT REFERENCE
Declares references to instantiated objects.

```cobol
01  WS-SVC                 object reference InvoiceService.
```

## INVOKE
Calls methods on object references.

```cobol
invoke WS-SVC "GetVersion"
```

## PROPERTY syntax
Defines get/set style object properties in Rocket-family OO COBOL.

```cobol
property-id. Name.
    getter.
        goback returning WS-NAME
    setter.
        move parameter-value to WS-NAME
end property Name.
```

## Final complete example
This complete example shows class, object reference, and method invocation syntax.

```cobol
$set sourceformat"free"

class-id. Demo.Customer as "Demo.Customer".

environment division.
configuration section.
repository.
    class CustomerFactory as "Demo.CustomerFactory".

data division.
working-storage section.
01 ws-name                 pic x(30) value spaces.

method-id. SetName.
linkage section.
01 lk-name                 pic x(30).
procedure division using lk-name.
    move lk-name to ws-name
    goback.
end method SetName.

method-id. ShowName.
procedure division.
    display "customer=" ws-name
    goback.
end method ShowName.

end class Demo.Customer.

identification division.
program-id. oo-runner.

data division.
working-storage section.
01 ws-customer             object reference Demo.Customer.

procedure division.
main-logic.
    invoke Demo.Customer "new" returning ws-customer
    invoke ws-customer "SetName" using "ACME INDUSTRIES"
    invoke ws-customer "ShowName"
    stop run.
```
