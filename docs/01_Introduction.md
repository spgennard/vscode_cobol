# Getting Started with COBOL

## What is COBOL?

COBOL = COmmon Business Oriented Language. It is one of the oldest high-level programming languages still in active production use today.

It was designed to be readable by business people, not just programmers. COBOL reads almost like English:

```cobol
MOVE 100 TO CUSTOMER-BALANCE
IF ACCOUNT-STATUS = "ACTIVE" THEN DISPLAY "OK"
```


That readability was intentional. COBOL is record and file oriented, strong on numeric calculations, decimal arithmetic, and batch processing. It is the language that runs the back office of banking, insurance, government benefits, payroll, and airline reservation systems.

Today it is supported by the three major commercial compilers:
* **Rocket COBOL** - z/OS and Open Systems
* **ACUCOBOL-GT** - Windows with GUI extensions
* **Micro Focus Visual COBOL / Enterprise Developer** - Windows, Linux and mainframe with .NET/Java integration

## A Brief History of COBOL

### 1959 - Birth
* Grace Hopper and the Department of Defense formed CODASYL, the Conference on Data Systems Languages.
* COBOL was first released in 1959 as a joint government-industry effort to create a standardized business language.

### 1960s - Standardization
* 1960: First official COBOL report published.
* 1968: COBOL 68 standard.
* COBOL became the de facto standard for business data processing on mainframes from IBM, UNIVAC, Burroughs, etc.

### 1970s-1980s - ANSI Standard
* 1974: ANSI COBOL 74
* 1985: ANSI COBOL 85, the major modern standard. This is the base most commercial compilers still target.
* COBOL dominated mainframe business systems. Millions of lines were written.

### 1990s-2000s - Evolution
* 2002: ISO COBOL 2002 added object-oriented features, XML support, and pointer enhancements.
* Compilers modernized. Micro Focus Visual COBOL, ACUCOBOL-GT and later Rocket COBOL added Windows GUI, .NET/Java interoperability, and free-format source.

### 2010s-Present - Legacy + Modernization
* 2014: ISO COBOL 2014 standard released.
* COBOL is still estimated to run 70-80% of business transactions worldwide. Banks, pension systems, tax agencies, and insurance companies rely on it.
* Companies like Rocket Software, Micro Focus, and ACUCorp maintain and extend COBOL for cloud, Linux, and mainframe modernization rather than full replacement.

### Why Learn COBOL Now?

COBOL is old but not dead. Learning it means learning a stable, well-documented language with a huge installed base, and the skills transfer directly to Rocket COBOL, ACUCOBOL-GT, and Micro Focus Enterprise Developer.

In the next chapters you will:
1. Set up your development environment
2. Understand the four divisions of a COBOL program
3. Write your first portable program
4. See compiler-specific notes for Rocket, ACUCOBOL-GT and Micro Focus

