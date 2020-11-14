# COBOL Extension for Visual Studio Code

## Changes: 6.10.4-8

Breaking change:

The ```coboleditor.cache_metadata```/```storagepath``` setting has been replaced with a user configurable directory via the ```coboleditor.cache_metadata=user_defined_directory``` setting and ```coboleditor.cache_metadata_user_directory``` setting to specific the location.

Each workspace cache directory is required to be in a seperate directory.

For example:
```json
    "coboleditor.cache_metadata": "user_defined_directory",
    "coboleditor.cache_metadata_user_directory" : "${HOME}/.vscode_cobol/myworkspace"
```

Remember to create the directory structure, for example, on Linux:

```bash
mkdir -p $HOME/.vscode_cobol/myworkspace
```

and Windows:

```dos
mkdir %USERPROFILE%\.vscode_cobol
mkdir %USERPROFILE%\.vscode_cobol\myworkspace
```

For more information see the online [README](https://github.com/spgennard/vscode_cobol_extension/blob/main/README.md#metadata-caching-location).
