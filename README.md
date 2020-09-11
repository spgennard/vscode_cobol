
[![Version](https://vsmarketplacebadge.apphb.com/version/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) [![Installs](https://vsmarketplacebadge.apphb.com/installs-short/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) [![Downloads](https://vsmarketplacebadge.apphb.com/downloads-short/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) [![Rating](https://vsmarketplacebadge.apphb.com/rating-star/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) [![chat-img](https://img.shields.io/badge/Gitter-Join_the_vscode_cobol_chat-brightgreen.svg)](https://gitter.im/vscode_cobol/community)

# COBOL Source editing for Visual Studio Code

This extension was originally aimed at providing syntax highlighting for COBOL but overtime is has been extended to provide syntax highlighting for other related languages/file formats such JCL, PL/I and Micro Focus directive files and Micro Focus Unit Test Reports.

## What can I expect from this extension

This extension is certainly not an Integrated Development Environment for COBOL.   It does provide enough functionality for the casual developer to view, edit and compile source code (via tasks & problem matchers).

## Does this extension include a language server protocol for COBOL

Visual Studio Code has two ways of providing language extensions, one if via the [language server protocol](https://langserver.org/) and the second is via a set of Visual Studio Code APIs.

This extension uses the Visual Studio Code APIs because the Language Server Protocol did not exist when this extension was created.

## What platform can it be used on?

Everywhere Visual Studio Code works.. aka Windows, Linux and Mac OSX.

## Code colorization for COBOL, PL/I and JCL:

 ![sieve_jcl](https://raw.githubusercontent.com/spgennard/vscode_cobol/master/images/screenshot_three.png)

## IntelliSense example:

![perform_add](https://raw.githubusercontent.com/spgennard/vscode_cobol/master/images/perform_add.gif)

## Breadcrumb support:

![breadcrumbs](https://raw.githubusercontent.com/spgennard/vscode_cobol/master/images/breadcrumb.png)

## Outline support:

![outline](https://raw.githubusercontent.com/spgennard/vscode_cobol/master/images/outline.png)

## Goto definition:

![gotodef](https://raw.githubusercontent.com/spgennard/vscode_cobol/master/images/gotodef.gif)

## Peek definition:

![peekdef](https://raw.githubusercontent.com/spgennard/vscode_cobol/master/images/peekdef.gif)

## Keybinds

| Keys                     |                          Description                           |
|--------------------------|:--------------------------------------------------------------:|
| ctrl+alt+p               |                    Goto procedure division                     |
| ctrl+alt+w               |                  Goto working-storage section                  |
| ctrl+alt+d               | Goto data division (or working-storage section if not present) |
| ctrl+alt+,               |             Go backwards to next section/division              |
| ctrl+alt+.               |            Go forward to next next section/division            |
| f12 or ctrl+click        |                     Move to copybook/file                      |
| ctrl+hover over copybook |                     Peek head of copybook                      |
| right mouse/peek         |            Peek copybook without opening the file)             |

## Settings

- COBOL tab stops can be changed by editing the *coboleditor.tabstops* setting.
- Extensions used for *move to copybook*, can be changed by editing the *coboleditor.copybookexts* settings.
- Directories used for *move to copybook*, can be changed by editing the *coboleditor.copybookdirs* settings.

## Tasks

Visual Studio code can be setup to build your COBOL source code.

### Task: Using MsBuild

MsBuild based projects can be consumed as build task, allowing navigation to error/warnings when they occur.

Below is an example of *build* task that uses *mycobolproject.sln*.

```json
{
    "version": "2.0.0",
    "tasks": [ {
            "label": "build",
            "type": "shell",
            "command": "msbuild",
            "args": [
                "/property:GenerateFullPaths=true",
                "/t:build",
                "mycobolproject.sln"
            ],
            "group": {
                "kind": "build",
                "isDefault": true
            },
            "presentation": {
                "reveal": "always"
            },
            "problemMatcher": "$mfcobol-msbuild"
        }
    ]
}
```

### Task: Single file compile using Micro Focus COBOL - ERRFORMAT(3)

The example below shows you how you can create a single task to compile one program using the `cobol` command.

```json
{
    "label": "mf cobol (single file)",
    "command": "cobol",
    "args": [
        "${file}",
        "noint",
        "nognt",
        "noobj",
        "noquery",
        "errformat(3)",
        "COPYPATH($COBCPY;${workspaceFolder}\\CopyBooks;${workspaceFolder}\\CopyBooks\\Public)",
        ";"
    ],
    "group": {
        "kind": "build",
        "isDefault": true
    },
    "options": {
        "cwd": "${workspaceRoot}"
    },
    "presentation": {
        "echo": true,
        "reveal": "never",
        "focus": true,
        "panel": "dedicated"
    },
    "problemMatcher": "$mfcobol-errformat3"
}
```

### Task: Single file compile using Micro Focus COBOL - ERRFORMAT(2)

The example below shows you how you can create a single task to compile one program using the `cobol` command.

For Net Express/Server Express compilers use the "$mfcobol-errformat2-netx-sx" problem matcher as although the directive ERRFORMAT"2" is used, the compiler output error format is slightly different.

```json
{
    "label": "mf cobol (single file)",
    "command": "cobol",
    "args": [
        "${file}",
        "noint",
        "nognt",
        "noobj",
        "noquery",
        "errformat(2)",
        "COPYPATH($COBCPY;${workspaceFolder}\\CopyBooks;${workspaceFolder}\\CopyBooks\\Public)",
        ";"
    ],
    "group": {
        "kind": "build",
        "isDefault": true
    },
    "options": {
        "cwd": "${workspaceRoot}"
    },
    "presentation": {
        "echo": true,
        "reveal": "never",
        "focus": true,
        "panel": "dedicated"
    },
    "problemMatcher": [ "$mfcobol-errformat2", "$mfcobol-errformat2-copybook" ]
}
```

### Task: Single file compile using GnuCOBOL/OpenCOBOL/COBOL-IT

The example below shows you how you can create a single task to compile one program using the `cobc` command.

This example is for GnuCOBOL 1-2.x, for GnuCOBOL use $gnucobol3-cob

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "gnucobol - cobc (single file)",
            "type": "shell",
            "command": "cobc",
            "args": [
                "-fsyntax-only",
                "-I${workspaceFolder}\\CopyBooks",
                "-I${workspaceFolder}\\CopyBooks\\Public",
                "${file}"
            ],
            "problemMatcher" : "$gnucobol-cobc"
        }
    ]
}
```

### Task: Single file compile using ACUCOBOL-GT

The example below shows you how you can create a single task to compile one program using the `ccbl32` command.

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "acu cobol - ccbl32 (single file)",
            "type": "shell",
            "command": "%ACUCOBOL%\\bin\\ccbl32",
            "args": [
                "-Sp", "${workspaceFolder}\\CopyBooks",
                "-Sp", "${workspaceFolder}\\CopyBooks\\Public",
                "${file}"
            ],
            "windows": {
                "options": {
                    "env": {
                        "ACUCOBOL" : "C:\\extend10.1.1\\AcuGT"
                    }
                }
            },
            "problemMatcher" : [ "$acucobol-warning-ccbl", "$acucobol-ccbl" ]
        }
    ]
}
```

### Task: Breakdown of problem matchers

| Product and Version                           | Tools                                                        | Problem matcher(s)                                                     |
|-----------------------------------------------|--------------------------------------------------------------|------------------------------------------------------------------------|
| COBOL-IT                                      | cobc                                                         | $cobolit-cobc                                                          |
| COBOL-IT                                      | cobc                                                         | $cobolit-error-cobc + $cobolit-note-cobc                               |
| open-cobol 1-1.5                              | cobc                                                         | $opencobol-cobc                                                        |
| open-cobol 1-1.5                              | cobc                                                         | $opencobol-warning-cobc + $opencobol-error-cobc + $opencobol-note-cobc |
| ACU-COBOLGT                                   | ccbl                                                         | $acucobol-ccbl + $acucobol-warning-ccbl                                |
| GnuCOBOL 1-2                                  | cobc                                                         | $gnucobol-cobc                                                         |
| GnuCOBOL 3                                    | cobc                                                         | $gnucobol3-cobc                                                        |
| GnuCOBOL 3                                    | cobc                                                         | $gnucobol3-warning-cobc + $gnucobol3-error-cobc + $gnucobol3-note-cobc |
| Micro Focus COBOL Net Express/Server Express  | cob or cobol.exe                                             | $mfcobol-errformat2-netx-sx                                            |
| Micro Focus Visual COBOL/Enterprise Developer | msbuild                                                      | $mfcobol-msbuild                                                       |
|                                               | cob or cobol.exe + ERRFORMAT"3"                              | $mfcobol-errformat3                                                    |
|                                               | cob or cobol.exe + ERRFORMAT"3" / filename extract with PATH | $mfcobol-errformat3-basefn                                             |
|                                               | cob or cobol.exe + ERRFORMAT"2"                              | $mfcobol-errformat2                                                    |
|                                               | cob or cobol.exe + ERRFORMAT"2" for errors in copybooks      | $mfcobol-errformat2-copybook                                           |

## Remote development using containers

If your main development is Micro Focus Visual COBOL/Enterprise Developer you may have access to base images that provide the compiler and its tools.

If you do, all that is required is another image that contains extra tools and a devcontainer.json to configure its use.

The following ```Dockerfile``` is an example on how you can extend your existing base image with java configured, ant, git and lsb tools.

This example uses the SLES 15.1 base images using Visual COBOL 5.0.

You may need to tweak the ```FROM``` clause in the Dockerfile and if you use a different platform or product version, the ```zypper``` will also require a change too if a different platform is used (different commant eg: yum, microdnf etc..).

Dockerfile:

```Dockerfile
FROM microfocus/vcdevhub:sles15_5.1_x64_login

USER root

ENV JAVA_HOME=/usr/java/default
ENV PATH=${JAVA_HOME}/bin:${PATH}

ENV COBDIR=${MFPRODBASE}

ENV PATH=${COBDIR}/bin:${PATH}
ENV LD_LIBRARY_PATH=${COBDIR}/lib:${LD_LIBRARY_PATH}

ENV ANT_HOME=/opt/microfocus/VisualCOBOL/remotedev/ant/apache-ant-1.9.9
ENV PATH=${ANT_HOME}/bin:${PATH}

RUN zypper --non-interactive install  --no-recommends lsb-release git
```

devcontainer.json:

```json
{
	// See https://aka.ms/vscode-remote/devcontainer.json for format details.
	"name": "Visual COBOL",

	// Update the 'dockerFile' property if you aren't using the standard 'Dockerfile' filename.
	"dockerFile": "Dockerfile",

	// The optional 'runArgs' property can be used to specify additional runtime arguments.
	"runArgs": [
		//  Uncomment the next line if you want to use Docker from the container. See the docker-in-docker definition for details.
		// "-v","/var/run/docker.sock:/var/run/docker.sock",

		// Uncomment the next two lines if you will use a ptrace-based debugger like C++, Go, and Rust
		"--cap-add=SYS_PTRACE",
		"--security-opt", "seccomp=unconfined"
	],

	// Uncomment the next line if you want to publish any ports.
	// "appPort": [],

	// Uncomment the next line if you want to add in default container specific settings.json values
	// "settings":  { "workbench.colorTheme": "Quiet Light" },

	// Uncomment the next line to run commands after the container is created.
	// "postCreateCommand": "uname -a",

	// Add the IDs of any extensions you want installed in the array below.
	"extensions": [
		"bitlang.cobol"
	]
}
```

## Complementary extensions

### [ToDo tree](https://marketplace.visualstudio.com/items?itemName=Gruntfuggly.todo-tree) by Gruntfuggly

Although this extension does not understand comments in COBOL source files, it can be made to by adding the following user setting:

```json
{
    "todo-tree.tree.flat": false,
    "todo-tree.tree.expanded": true,
    "todo-tree.regex.regex": "((//|#|<!--|;|/\\*|\\*>|^......\\*)\\s*($TAGS)|^\\s*- \\[ \\])",
    "todo-tree.general.tags": [
        "TODO",
        "FIXME",
        "!FIXME",
        "CHANGED",
        "BUG",
        "NOTE"
    ],
    "todo-tree.tree.filterCaseSensitive": true,
    "todo-tree.highlights.customHighlight": {
        "FIXME": {
            "icon": "flame",
            "iconColour": "#A188FF",
        },
        "NOTE": {
            "iconColour:" : "blue"
        },
        "TODO": {
            "iconColour:" : "cyan"
        },
        "CHANGED": {
            "iconColour:" : "yellow"
        },
        "BUG": {
            "icon": "bug"
        }
    }
}
```


## coboleditor.fileformat

When ```coboleditor.margin``` is enabled extension will look for "sourceformat" settings in the header of the source file itself.

However, if you need to tell the extension which file are which particular format, this can be achieved with ```coboleditor.fileformat``` property.

For example, if you want all the files that match ```A*.cbl``` to be fixed and every other *.cbl is free format, you can then use:

```json
    "coboleditor.fileformat": [
        {
            "pattern": "**/A*.cbl",
            "sourceformat": "fixed"
        },
        {
            "pattern": "**/*.cbl",
            "sourceformat": "free"
        }
    ],
```

## Handling code pages

The defaults embedded in the extension can be overwritten by either changing sessions at the user level in the settings json or more efficiently, you change it just for the "COBOL" files.

For example, to ensure you use utf8 for all you files use:

```json
{
    "[COBOL]": {
        "files.encoding": "utf8",
        "files.autoGuessEncoding": false
    }
}
```

## COBOLDOC

When editing COBOL source code, the documentation contained within the source code can be generated and view via "editor->Show documentation (COBOLDOC)".

The source code is generated to html in the "coboldoc" directory in the workspace.   The folder name can be changed by changing the property *coboleditor.coboldoc_workspace_folder*.

The source generation includes a index.html, as well as a sourcefile.cbl.html file, so if you change the directory please be aware that the *coboldoc* tool overwrites the index.html file too.

The source code generation is provided by the npm coboldoc and is required to be installed for this to work.

## coboleditor.experimental_features

Currently I have only one is active experimental feature and this is "hover" support for known APIs.

This currently includes most of the *Micro Focus COBOL Library API* (CBL_) and a subset of ILE date apis.

This can be activated by setting the flag coboleditor.experimental_features in the settings panel.

and looks like:

 ![hover](https://raw.githubusercontent.com/spgennard/vscode_cobol/master/images/hover.png)

## Scanning and caching

COBOL source code can be complex and enabling/disabling the caching will make the editor experience more responsive but will reduce the information available to the extension, which has an impact on features such as "find all references", "goto definition".

If you do not have caching enabled, my recommendation is to use "file searches" to locate the sourcrequired information.

| Settings                        | Description of use                                                                         |
|---------------------------------|--------------------------------------------------------------------------------------------|
| process_metadata_cache_on_start | Scan all files in the workspace and located copybooks                                |
| cache_metadata=on               | Use cache data from scanned source                                                        |
| parse_copybooks_for_references  | Scan for any copybooks when editing.     Helps with "goto definition" and "find all references. Does not require metadata caching enabled |

The metadata cache does not have to be created on workspace startup but can be created at will by using the "COBOL: Process files in workspace for metadata" command.

The command "COBOL: Clear metadata" can be used to remove the on-disk cache.

## Online resources

- [Facebook COBOL Group](https://www.facebook.com/groups/COBOLProgrammers/)
- [Micro Focus COBOL Community](https://community.microfocus.com/t5/Application-Modernization/ct-p/COBOL)

## Shortcuts


 - [ALT] + [SHIFT] + [C]: Change to COBOL Syntax (default)
 - [ALT] + [SHIFT] + [A]: Change to ACUCOBOL-GT Syntax
 - [ALT] + [SHIFT] + [O]: Change to OpenCOBOL Syntax
 - [ALT] + [SHIFT] + [G]: Change to GnuCOBOL Syntax
 - [ALT] + [SHIFT] + [M]: Toggle margins (overrides user/workspace settings)

## Contributors

I would like to thank the follow contributors for providing patches, fixes, kind words of wisdom and enhancements.

 - Ted John of Berkshire, UK
 - Kevin Abel of Lincoln, NE, USA
 - Simon Sobisch of Germany
