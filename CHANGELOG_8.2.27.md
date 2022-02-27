# bitlang.cobol - setting changes

The following boolean settings have been removed in preference to a single new setting:

```text
    coboleditor.intellisense_include_unchanged
    coboleditor.intellisense_include_camelcase
    coboleditor.intellisense_include_uppercase
    coboleditor.intellisense_include_lowercase
```

The new simplier setting is:

```text
    coboleditor.intellisense_style

    Which takes a enumation :
       unchanged
       camelcase
       lowercase
       uppercase
```
