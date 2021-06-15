[![Version](https://vsmarketplacebadge.apphb.com/version/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) [![Installs](https://vsmarketplacebadge.apphb.com/installs-short/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) [![Downloads](https://vsmarketplacebadge.apphb.com/downloads-short/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) [![Rating](https://vsmarketplacebadge.apphb.com/rating-star/bitlang.cobol.svg)](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol)


# COBOL Source editing for Visual Studio Code

This extension was originally aimed at providing syntax highlighting for COBOL but overtime is has been extended to provide syntax highlighting for other related languages/file formats such JCL, PL/I and Micro Focus directive files and Micro Focus Unit Test Reports.

## What can I expect from this extension

This extension is certainly not an Integrated Development Environment for COBOL.   It does provide enough functionality for the casual developer to view, edit and compile source code (via tasks & problem matchers).

## Does this extension include a language server protocol for COBOL

Visual Studio Code has two ways of providing language extensions, the first is provided by the use of a [language server protocol](https://langserver.org/) and the second is via a set of Visual Studio Code APIs.

This extension uses the Visual Studio Code APIs because the Language Server Protocol did not exist when this extension was created.

## What platform can it be used on?

Everywhere Visual Studio Code works.

## Code colorization for COBOL, PL/I and JCL:

 ![sieve_jcl](https://raw.githubusercontent.com/spgennard/vscode_cobol/main/images/screenshot_three.png)

## IntelliSense example:

![perform_add](https://raw.githubusercontent.com/spgennard/vscode_cobol/main/images/perform_add.gif)

## Breadcrumb support:

![breadcrumbs](https://raw.githubusercontent.com/spgennard/vscode_cobol/main/images/breadcrumb.png)

## Outline support:

![outline](https://raw.githubusercontent.com/spgennard/vscode_cobol/main/images/outline.png)

## Go to definition:

![gotodef](https://raw.githubusercontent.com/spgennard/vscode_cobol/main/images/gotodef.gif)

## Peek definition:

![peekdef](https://raw.githubusercontent.com/spgennard/vscode_cobol/main/images/peekdef.gif)

## Keybindings

| Keys                     |                          Description                            |
|--------------------------|:---------------------------------------------------------------:|
| ctrl+alt+p               |                    Go to procedure division                     |
| ctrl+alt+w               |                  Go to working-storage section                  |
| ctrl+alt+d               | Go to data division (or working-storage section if not present) |
| ctrl+alt+,               |             Go backwards to next section/division               |
| ctrl+alt+.               |            Go forward to next next section/division             |
| f12 or ctrl+click        |                     Go to copybook/file                         |
| ctrl+hover               |                     Peek head of copybook or symbol/field       |
| right mouse/peek         |            Peek copybook without opening the file)              |

## Settings

- COBOL tab stops can be changed by editing the ```coboleditor.tabstops``` setting.
- Extensions used for *Go to copybook*, can be changed by editing the ```coboleditor.copybookexts``` settings.
- Directories used for *Go to copybook*, can be changed by editing the ```coboleditor.copybookdirs``` settings.

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

For Net Express/Server Express compilers use the "$mfcobol-errformat3-netx-sx" problem matcher as although the directive ERRFORMAT"3" is used, the compiler output error format is slightly different.

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

### Task: Single file compile using COBOL-IT

The example below shows you how you can create a single task to compile one program using the `cobc` command.


```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "cobolit - cobc (single file)",
            "type": "shell",
            "command": "cobc",
            "args": [
                "-fsyntax-only",
                "-I${workspaceFolder}\\CopyBooks",
                "-I${workspaceFolder}\\CopyBooks\\Public",
                "${file}"
            ],
            "problemMatcher" : "$cobolit-cobc"
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
            "label": "ACUCOBOL-GT - ccbl32 (single file)",
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

| Product and Version                           | Tools                                                            | Problem matcher(s)                                                     |
|-----------------------------------------------|------------------------------------------------------------------|------------------------------------------------------------------------|
| COBOL-IT                                      | *cobc*                                                           | $cobolit-cobc                                                          |
| COBOL-IT                                      | *cobc* for errors/notes                                          | $cobolit-error-cobc + $cobolit-note-cobc                               |
| ACU-COBOLGT                                   | *ccbl* for errors/warnings                                       | $acucobol-ccbl + $acucobol-warning-ccbl                                |
| Micro Focus COBOL Net Express/Server Express  | *cob* or *cobol.exe* + ERRFORMAT"2"                              | $mfcobol-errformat2-netx-sx                                            |
|                                               | *cob* or *cobol.exe* + ERRFORMAT"2" for errors in copybooks      | +mfcobol-errformat2-copybook-netx-sx                                   |
|                                               | *cob* or *cobol.exe* + ERRFORMAT"3"                              | $mfcobol-errformat3-netx-sx                                            |
|                                               | *cob* or *cobol.exe* + ERRFORMAT"3" for information              | +mfcobol-errformat3-info                                               |
| Micro Focus Visual COBOL/Enterprise Developer | *msbuild*                                                        | $mfcobol-msbuild                                                       |
|                                               | *cob* or *cobol.exe* + ERRFORMAT"3"                              | $mfcobol-errformat3                                                    |
|                                               | *cob* or *cobol.exe* + ERRFORMAT"3" / filename extract with PATH | $mfcobol-errformat3-basefn                                             |
|                                               | *cob* or *cobol.exe* + ERRFORMAT"2"                              | $mfcobol-errformat2                                                    |
|                                               | *cob* or *cobol.exe* + ERRFORMAT"2" for errors in copybooks      | $mfcobol-errformat2-copybook                                           |

NOTE: Problem matchers can be stacked in your task definition.   It is recommended that any "-copybook", "-info", "-note" and similar problem matcher are included before problem matchers without this suffix.

## Remote development using containers

If your main development is Micro Focus Visual COBOL/Enterprise Developer you may have access to base images that provide the compiler and its tools.

If you do, all that is required is another image that contains extra tools and a devcontainer.json to configure its use.

The following ```Dockerfile``` is an example on how you can extend your existing base image with java configured, ant, git and lsb tools.

This example uses the SLES 15.1 base images using Visual COBOL 5.0.

You may need to tweak the ```FROM``` clause in the Dockerfile and if you use a different platform or product version, the ```zypper``` will also require a change too if a different platform is used (different commands eg: yum, microdnf etc..).

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

### Workspace Setup

Visual Studio Code workspaces are not "projects" but do allow you keep your source in one place.

Things to consider:

 - If you have copybooks, you should change the ```coboleditor.copybookdirs``` settings to setup where the extension can find your copybooks
  - If use the *COPY* verb with this extension, you may also need to adjust the ```coboleditor.copybookexts``` settings (json array).
  - If you want the extension to understand the contents of your copybook before you access it, then turn on the ```coboleditor.parse_copybooks_for_references``` settings (json array) to allow the extension to look inside the copybook references


### COBOL Linter

The COBOL linter included with the extension performs two functions, the first function is to identify sections/paragraphs that are not used and the second is to apply any "house" standards.

The section/parapgraph linter by default generates warning message but if you prefer the messages to be marked as information, you change the ```coboleditor.linter_mark_as_information``` boolean setting, for example:

 ```json
 "coboleditor.linter_mark_as_information": true
 ```

The house standards are applied to fields in various sections, each section is named and a rule defined as a regular expression to enforce.

For example to enforce all working-storage items must start with ws and local-storage ls, you can use:

```json
"coboleditor.linter_house_standards_rules": [
        "file=.*",
        "thread-local=ls.*",
        "working-storage=ws.*",
        "object-storage=.*",
        "local-storage=.*",
        "linkage=.*",
        "communication=.*",
        "report=.*",
        "screen=.*",
    ]
```

### Metadata caching

By default the metadata caching is turned on and is stored in the current code workspace but it can be turned off via the ```coboleditor.maintain_metadata_cache``` setting.

Metadata is stored in your code workspace file but can also be stored in your single folder settings file by enabling the ```coboleditor.maintain_metadata_cache_single_folder``` setting.

## Pre-Processor support for "hidden" source code

Some pre-processor reference copybooks that are inserted into the code without using standard COBOL syntax.

These source files are often difficult to edit due this extension not knowing anything about these files.

In order to help this extension, a special style of comment can be inserted into the code that allows the extension to locate these extra copybooks.

For example, if your preprocessor includes the files ```Shared/foo.cbl OldCopyBooks/Shared/bar.cbl```, then you can use the following comment line.

```cobol
*> @source-dependency Shared/foo.cbl OldCopyBooks/Shared/bar.cbl
````

The source-dependency names are separated by a space.

To enable this feature enable the ```scan_comments_for_hints``` setting, for example:

```json
"coboleditor.scan_comments_for_hints": true
```

The hint token can be configured by the ```coboleditor.scan_comment_copybook_token``` setting, which has the default value set to ```source-dependency```.

It is recommended that the token name remain consistent in your source, otherwise it will make it hard for observers of your source to understand the code.


## coboleditor.fileformat & coboleditor.fileformat_strategy

When ```coboleditor.fileformat_strategy``` is set to "normal", the source format will be determined heuristically but can be overriden by either embedded directives with each source file.

However, if you need to tell the extension which file(s) are which particular file format, this can be achieved with ```coboleditor.fileformat``` property.

For example, if you want all the files that match ```A*.cbl``` to be fixed and every other ```*.cbl``` is free format, you can then use:

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

If always use ```fixed``` format you can set ```coboleditor.fileformat_strategy=always_fixed```.

## Handling code pages

The defaults embedded in the extension can be overwritten by either changing sessions at the user level in the settings json file or more efficiently, you change it just for the "COBOL" files.

For example, to ensure you use utf8 for all you files use:

```json
{
    "[COBOL]": {
        "files.encoding": "utf8",
        "files.autoGuessEncoding": false
    }
}
```

## Tips

- If you find you are not getting any symbols in the outline view or the peek/goto definition functionality does not work, please check the ``Output->COBOL`` panel as it may give you a reason for this.

   For example the editor line limit has been surpassed or the file fails to be identified as a COBOL source file.

 - The colors in the editor can be changed on a per theme basis, for example:

```json
"editor.tokenColorCustomizations": {
        "[Monokai]": {
            "comments": "#229977"
        }
    }
```

Where *comments* is a token name, standard tokens can be found in the [textmate documentation](https://macromates.com/manual/en/language_grammars).

Useful tokens that are often changed are: comment.line.cobol.newpage, keyword.operator.

## Complementary extensions

### [COBOL Language Dictionary - Code Spell Checker](https://marketplace.visualstudio.com/items?itemName=bitlang.code-spell-checker-cobol)

Spell checking code is helpful but without specific support for the COBOL language it can be a painful experience, so in order to make it easier. I have produced a spell checker extension that has a the standard COBOL reserved words and keywords from various dialects such a Micro Focus COBOL and IBM Enterprise COBOL.

Spell checking can be enabled/disabled in your source code by using:
```cobol
      * spell-checker: disable
      * spell-checker: enable
```

You can also ignore words in the code, for example:
```cobol
      * cSpell:ignoreWords TPCC, ridfld, dfhresp
```

Lastly, you use a regular expression, for example, to ignore words that contain a '-', you could use:

```cobol
      * cSpell:ignoreRegExp /\w*-\w*/
```

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

## Visual Studio Code Workspace Trust security

When in limited functionality mode the extension disables all functionality that might be use for malicious purposes.

The extension only enables features that allow basic editing, making it ideal for browsing untrusted source.

## Online resources

- Online communities
  - [Facebook COBOL Group](https://www.facebook.com/groups/COBOLProgrammers/)
   - [Micro Focus COBOL Community](https://community.microfocus.com/t5/Application-Modernization/ct-p/COBOL)
   - [Open Mainframe Project - COBOL Forum](https://community.openmainframeproject.org/c/cobol-technical-questions)
   - [Tek-Tips - COBOL General discussion](https://www.tek-tips.com/threadminder.cfm?pid=209)
 - Stack Overflow topics/tags:
   - [Micro Focus COBOL, PL/I, REXX, JCL and CICS](https://stackoverflow.com/questions/tagged/microfocus)
   - [ACUCOBOL-GT](https://stackoverflow.com/questions/tagged/acucobol-gt)
   - [COBOL](https://stackoverflow.com/questions/tagged/cobol)
   - [COBOL.NET](https://stackoverflow.com/questions/tagged/cobol.net)
   - [CICS](https://stackoverflow.com/questions/tagged/cics)
 - [COBOL Programming Language Articles on Reddit](https://www.reddit.com/r/cobol/)
 - [Linkedin Learning COBOL Resources](https://www.linkedin.com/learning/topics/cobol)
- wikipedia
  - [COBOL](https://en.wikipedia.org/wiki/COBOL)
  - [CICS](https://en.wikipedia.org/wiki/CICS)
- standards
  - [ICS > 35 > 35.060 / ISO/IEC 1989:2014](https://www.iso.org/standard/51416.html)

## Shortcuts

 - [ALT] + [SHIFT] + [C]: Change to COBOL Syntax (default)
 - [ALT] + [SHIFT] + [A]: Change to ACUCOBOL-GT Syntax
 - [ALT] + [SHIFT] + [M]: Toggle margins (overrides user/workspace settings)

## Contributors

I would like to thank the follow contributors for providing patches, fixes, kind words of wisdom and enhancements.

 - Ted John of Berkshire, UK
 - Kevin Abel of Lincoln, NE, USA
 - Simon Sobisch of Germany

 NOTE: Some of the above contributions have now been moved into the GnuCOBOL extension.
