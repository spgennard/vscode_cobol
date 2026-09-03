# Setting Up Your COBOL Environment

This chapter focuses on the practical setup needed for the two commercial COBOL environments used by this extension:

- ACUCOBOL-GT
- Rocket COBOL / Rocket Enterprise Developer (Visual COBOL)

The goals are the same for both products:

1. Install the compiler and runtime
2. Activate the product license
3. Put the compiler in the system path
4. Configure include/copybook paths
5. Confirm that a simple COBOL program can compile and run

## Common requirements

Before installing either product, check the following:

- Administrative rights are available on the machine
- A supported operating system is being used
- A valid license or evaluation key is available
- The target platform is known (Windows, Linux, or UNIX)
- The project will need source directories, copybooks, and a runtime environment

Both vendors typically assume that:

- the compiler install directory is known and stable
- executable files are available from the shell or command prompt
- include paths are configured for copybooks and library files
- the runtime libraries are available to the operating system

## Shared environment variables

The most common setup items in both environments are shared, even if the exact names vary slightly by product version.

### PATH
Add the compiler and runtime bin directories to the system PATH so the COBOL commands can be invoked from a terminal or IDE.

Typical examples:

- Windows: add the product install `bin` directory to PATH
- Linux/UNIX: add the installation `bin` directory to PATH and ensure runtime shared libraries are available

### COBDIR / install root
The compiler install root is usually referenced by an environment variable such as `COBDIR` or a product-specific install path. This is the base location that points to the compiler and runtime files.

### Copybook search paths
Both toolchains rely on copybooks and include files. Configure one or more of the following as needed:

- `COBPATH`
- `COPYPATH`
- project-local include directories

This is important because COBOL programs normally split logic across programs and copybooks.

For Rocket COBOL / Enterprise Developer, the supplied library directory is commonly the `cpylib` sub-directory under the product install tree. The documentation specifically points to `%ProgramFiles(x86)%\Rocket Software\Enterprise Developer\cpylib` on Windows and `$COBDIR/cpylib` on Unix, and suggests opening it in an editor to review the supplied library members.

### Runtime library path
On Linux and UNIX systems, the runtime libraries often need to be visible through a platform library path such as:

- `LD_LIBRARY_PATH` on Linux
- `LIBPATH` on AIX
- similar runtime library settings on other Unix variants

Without this, a successfully built COBOL program may still fail at runtime.

### Example environment settings
The exact install path depends on the installer, but these are the common default patterns used on Windows systems.

```bat
REM Common ACUCOBOL-GT default layout
set ACUCOBOL=C:\Program Files\Acucorp\ACUCOBOL-GT
set COBDIR=%ACUCOBOL%
set COBPATH=%ACUCOBOL%\copylib;C:\work\cobol\copybooks
set PATH=%COBDIR%\bin;%PATH%
set LD_LIBRARY_PATH=%COBDIR%\bin;%LD_LIBRARY_PATH%
```

```bat
REM Rocket COBOL / Enterprise Developer default layout
set COBDIR=%ProgramFiles(x86)%\Rocket Software\Enterprise Developer
set COBCPY=%COBDIR%\cpylib;C:\work\cobol\copybooks
set PATH=%COBDIR%\bin;%PATH%
```

If you are using a bash shell instead of the Windows command prompt, the equivalent pattern is:

```bash
export COBDIR="/opt/acucorp/acucobol-gt"
export COBPATH="/opt/acucorp/acucobol-gt/copylib:/workspace/cobol/copybooks"
export PATH="$COBDIR/bin:$PATH"
export LD_LIBRARY_PATH="$COBDIR/bin:$LD_LIBRARY_PATH"
```

```bash
export COBDIR="/opt/rocket/enterprise-developer"
export COBPATH="$COBDIR/cpylib:/workspace/cobol/copybooks"
export PATH="$COBDIR/bin:$PATH"
```

> Rocket's current documentation uses `%ProgramFiles(x86)%\Rocket Software\Enterprise Developer\cpylib` on Windows and `$COBDIR/cpylib` on Unix. The folder is intended to be opened and reviewed in a file editor to become familiar with the supplied copybooks and library members.

> ACUCOBOL-GT commonly uses `copylib`, while Rocket COBOL commonly uses `cpylib`.

> These examples are representative defaults. If the product was installed under a custom path, replace the directory values with the actual install root.

## ACUCOBOL-GT setup

ACUCOBOL-GT is a cross-platform COBOL toolchain with a runtime and a compiler. In practice, the setup is usually:

1. Install the ACUCOBOL-GT package for the target OS
2. Accept the license and choose the runtime/compiler installation type
3. Record the install directory, often called the ACUCOBOL home directory
4. Add the product `bin` directory to `PATH`
5. Set the install root and copybook directories in the environment
6. Confirm the runtime can find shared libraries on Linux/UNIX
7. Compile and run a simple test program

### Recommended ACUCOBOL-GT layout

Use a simple directory layout similar to:

```text
workspace/
  src/
    hello.cbl
  copybooks/
    common.cpy
  build/
```

Then ensure the compiler can see:

- the source directory
- the copybook directory
- the compiler `bin` directory

### ACUCOBOL-GT validation
After setup, validate with a minimal COBOL program and compile it using the vendor supplied compiler command. If the command is recognized from a terminal and the program builds without missing include paths or runtime library errors, the setup is correct.

### ACUCOBOL-GT notes

- The install root is a central concept in ACUCOBOL-GT setup
- Copybook search paths are especially important for legacy COBOL applications
- Linux and UNIX systems often require extra runtime environment configuration
- GUI-based applications may also need runtime display or terminal configuration

## Rocket COBOL / Rocket Enterprise Developer setup

Rocket COBOL products are typically installed as a full development environment, including the compiler, build tools, and often integration with Windows or IDE tooling. Typical setup steps are:

1. Install the Rocket COBOL product package
2. Verify the operating system and prerequisites are met
3. Activate or register the license
4. Install any required IDE or SDK dependencies
5. Add the product binary paths to `PATH`
6. Configure include/copybook locations
7. Validate with a small COBOL compilation

### Prerequisites for Rocket COBOL

Depending on the product edition and platform, you may need:

- a supported Windows or Linux version
- administrative privileges for installation
- .NET or Java tooling if using integration features
- an IDE or SDK dependency for the chosen developer workflow
- a valid product license file or activation

### Rocket setup pattern

A typical setup pattern is as follows:

```text
project/
  src/
  copybooks/
  outputs/
```

Then configure:

- product installation directory
- compiler executable paths in `PATH`
- `COBPATH` / `COPYPATH` for copybooks
- runtime output folder or build folder

### Rocket validation
After installation, do a minimal compile from a command prompt to confirm the environment is working. If the compiler is found and source files compile, the setup is valid. If compile errors mention missing includes, incorrect search paths, or missing libraries, fix the environment variables and runtime paths before continuing.

### Rocket notes

- Rocket's environment usually benefits from a clean developer shell or terminal session
- IDE integrations are often easiest when the compiler installation path is already visible to the system
- Project-level include directories reduce confusion when many copybooks are present

## Combined setup checklist

Use this checklist before starting a real project:

- [ ] Install the compiler and runtime
- [ ] Activate the license
- [ ] Add the product `bin` directory to `PATH`
- [ ] Set the install root if required by the product
- [ ] Configure copybook locations using `COBPATH` or `COPYPATH`
- [ ] Add runtime library paths on Linux/UNIX
- [ ] Create a simple source file and compile it
- [ ] Confirm the runtime can execute the compiled program
- [ ] Keep source, copybooks, and build outputs in separate directories

## Recommended project layout

A clean project layout reduces setup problems:

```text
cobol-project/
  src/
    hello.cbl
    account.cbl
  copybooks/
    common.cpy
  build/
    out/
```

This layout makes it easier to:

- add more COBOL programs
- separate sources from generated output
- keep copybooks visible to the compiler
- keep build and debugging output isolated

## Final guidance

The two environments are similar in spirit, even if their brand and installer differ. The most important setup points are the same:

- install the compiler correctly
- add compiler paths to the environment
- configure include/copybook paths
- confirm runtime DLL or shared library visibility
- test a simple compile before starting real work

Once this is in place, the rest of the COBOL project work is mostly about source organization, data layouts, and program structure rather than installation issues.
