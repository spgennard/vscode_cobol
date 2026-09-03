# Getting Started with COBOL

## What is COBOL?

COBOL = COmmon Business Oriented Language. It is one of the oldest high-level programming languages still in active production use today.

It was designed to be readable by business people, not just programmers. COBOL reads almost like English:

```cobol
MOVE 100 TO CUSTOMER-BALANCE
IF ACCOUNT-STATUS = "ACTIVE" THEN DISPLAY "OK"
```


That readability was intentional. COBOL is record and file oriented, strong on numeric calculations, decimal arithmetic, and batch processing. It is the language that runs the back office of banking, insurance, government benefits, payroll, and airline reservation systems.

Today it is supported by three major commercial compilers:
* **IBM Enterprise COBOL for z/OS** - the dominant compiler on IBM mainframes
* **Rocket Visual COBOL / Rocket Enterprise Developer** (formerly Micro Focus, then briefly OpenText) - Windows, Linux, with .NET and JVM integration; Enterprise Developer also targets mainframe applications for off-host development and testing
* **Rocket ACUCOBOL-GT** (formerly Acucorp, then Micro Focus / OpenText) - cross-platform (Windows, Linux, many UNIXes) with GUI extensions

## A Brief History of COBOL

### 1959 - Birth
* Grace Hopper and the US Department of Defense convened the Conference on Data Systems Languages (CODASYL) to design a common business language.
* The CODASYL Short-Range Committee drafted the first specification in 1959.

### 1960s - Standardization
* 1960: COBOL-60 specification published - the first official COBOL report.
* 1968: ANSI COBOL 68 (ANSI X3.23-1968), the first ANSI standard.
* COBOL became the de facto standard for business data processing on mainframes from IBM, UNIVAC, Burroughs, and others.

### 1970s-1980s - ANSI Standard
* 1974: ANSI COBOL 74
* 1985: ANSI COBOL 85, the major modern standard. This is the base most commercial compilers still target.
* COBOL dominated mainframe business systems. Millions of lines were written.

### 1990s-2000s - Evolution
* 2002: ISO COBOL 2002 (ISO/IEC 1989:2002) added object-oriented features, national (Unicode) characters, free-format source, and pointer enhancements.
* Compilers modernized. Micro Focus Visual COBOL, ACUCOBOL-GT added Windows GUI, .NET/Java interoperability, and free-format source support.

### 2010s-Present - Legacy + Modernization
* 2014: ISO COBOL 2014 (ISO/IEC 1989:2014).
* 2023: ISO COBOL 2023 (ISO/IEC 1989:2023), the current revision.
* COBOL is still widely cited as running a large share of the world's business transactions, particularly in banking, pension systems, tax agencies, and insurance.
* The Micro Focus COBOL product line changed hands twice in short succession: Micro Focus was acquired by OpenText (completed January 2023), and OpenText then divested its Application Modernization and Connectivity (AMC) division - including Visual COBOL, Enterprise Developer and ACUCOBOL-GT - to Rocket Software (completed 1 May 2024).
* Vendors such as IBM and Rocket Software continue to maintain and extend COBOL for cloud, Linux, and mainframe modernization rather than full replacement.

### Why Learn COBOL Now?

COBOL is old but not dead. Learning it means learning a stable, well-documented language with a huge installed base, and the skills transfer directly to Rocket Visual COBOL / Enterprise Developer and Rocket ACUCOBOL-GT.

In the next chapters you will:
1. Set up your development environment
2. Understand the four divisions of a COBOL program
3. Write your first portable program
4. See compiler-specific notes for Rocket Visual COBOL / Enterprise Developer and Rocket ACUCOBOL-GT

