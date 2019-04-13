# COBOL,.DIR, PL/I & jCL Change Log

## Version - 4.9
 - Continue to evolve the quick parse

## Version - 4.8.1

- Fix #66 - Colourisation: extension not coloured 
- Fix #67 - Colourisation: "keep" in read lockt-fd-2 with kept lock has no colour 
- Fix #68 - Colourisation: thread-local if fd not colourised 

## Version - 4.8.0
 - Start to rework the quick parser
 
## Version - 4.7.1, 4.7.3
 - Fix #65 - Indexes not being picked up by parser 

## Version - 4.6.6
  - Fix #63 - Colourisation: end valuetype not coloured
  + Fix class-id/namespace issue

## Version - 4.6.5
 - Fix #62 - Go To/Peek definition of PERFORM/THRU paragraph wrong
   Remove the original inline tokenizer and use the same parser used 
   for the outliner, as it is now more mature and works better.
   (techincal debt that actually fixes this problem) 
 - Fix #61 - Colourisation: end-chain not coloured
 - Fix end-invoke colour
 
## Version - 4.6.4
 - Fix #60 - Multiline source-computer highlight captures too much 
 
## Version - 4.6.3
 - Fix typo in Fix #59
 
## Version - 4.6.2
 - Finally found a solution to multi-line installation, author,source-computer,
   object-computer,date-written,security,date-compiled statements.

## Version - 4.6.1
 - Fix #59 - goto paragraph

## Version - 4.6.0
 - Fix #58 - enhance goto section/paragraph to better understand fixed format
 
## Version - 4.5.9
 - Fix #57 - Show definition COPY REPLACING
   -> match copy xxx, previously this was too strict
   
## Version - 4.5.8
 - Add COBOL comments into exec sql
 - Add missing EXEC-SQL to keyword scanner list (ensures it not shownup in outline view)
 - Add column 1, sourceformat free comment line to scanner
 
## Version - 4.5.7
 - Fix #58 - Source not colourized if start from column 2.
 - Fix #56 - Show definition DCLGEN

## Version - 4.5.6
 - Fix #56 - Show definition DCLGEN
 
## Version - 4.5.5
 - Fix #54 - Paragraph detection
 - Fix #55 - doc comment continuation no longer works 

## Version - 4.5.4
 - Fix #53

## Version - 4.5.3
 - Add bold, high, lowlight, low, standard, background-high, background-low, background-standard to acu syntax
 - Above as invalid acu syntax in non-acu syntaxes
 - add exit/stop iterator

## Version - 4.5.2
 - Fix reference modification colourisation
 
## Version - 4.5.1
 - Fix issue #51

## Version - 4.5
 - Add $mfcobol-errformat2 problem matcher

## Version - 4.4.5
 - Fix #50
 
## Version - 4.4.4
 - Fix issue #50, add params keyword and mark it as a bad keyword in AcuCOBOL/OpenCOBOL

## Version - 4.4.3
 - Fix issue #49, add extra keywords and mark same keywords as bad in AcuCOBOL/OpenCOBOL
 
## Version - 4.4.1
 - Tweak icon

## Version - 4.4.0
 - More fixes to the way margin are activated

## Version - 4.3.3-9
 - Clean build

## Version - 4.3.2
 - Fix issue #47 - activator not present for AcuCOBOL

## Version - 4.3.1
 - Add alt+shift+m for margin shortcut toggle

## Version - 4.3
 - Add shortcuts inline with my old sublime plugin
 
## Version - 4.2.3
 - Tweaks to pic handling and config

## Version - 4.2.2
 - JCL changes for syntax and margin
 
## Version - 4.2.0
 - Add experimental margin support

## Version - 4.1.2
 - Only show source format button when it's useful todo it

## Version - 4.1.1
 - Add jcl into the right margin story
 
## Version - 4.1.0
 - Start work on sourceformat fixed right margin via a editor declaration

## Version - 4.0.8
 - Fix issue #43 (NOT & meta-symbol)

## Version - 4.0.7
 - Fix issue #43 (OPTIONAL)

## Version - 4.0.6
 - README changes
 
## Version - 4.0.5
 - Tweak configuration handling to be more efficent
 - README changes

## Version - 4.0.4
 - Add tunable to turn on/off parsing in area b onwards

## Vesion - 4.0.3
 - Tweaks for parsing working-storage

## Version - 4.0.2
 - Fix for "Keywords and literals are not colored when followed by comma or semicolon in AcuCOBOL" #42
 
## Version - 4.0.1
 - Add PL/I extension plinc
 - JCL rulers @ 71,72,80

## Version - 4.0.0
 - Ensure EXEC SQL/EXEC HTML are marked as an embedded language
   - now CTRL/ works in "exec sql" & "exec html"
   - also snippets for sql will appear in any are installed
   
## Version - 3.9.9
 - Fix issue #39
 
## Version - 3.9.8
 - Move themes to the "COBOL plus pack"
 
## Version - 3.9.7
 - add support for goto "fd" in sourcedefprovider

## Version - 3.9.6
 - tweaks to jcl syntax for issue #38

## Version - 3.9.5
 - Fix cobol parse, so it does not include 0 length paragraphs

## Version - 3.9.4
 - Initial version of paragraph parser for the outline view
 
## Version - 3.9.3
 - Fix issue #38 - "Add highlighting for JES2 control statements"

## Version - 3.9.2
 - Fix issue #37 - "Also take ".proc" as file extension for JCL"
 - Fix issue #36 - "PL/I claims Perl extension"
 
## Version - 3.9.1
 - Fix outline view (exit sectiomn & first line problem)

## Version - 3.9
 - Fix issue #35 - brace/any colourisation issue
 - Include a reworked tokenizer
 
## Version - 3.8
 - CBL_, PC_ literals are now marked as support.function
 
## Version - 3.7.10
 - empty release
 
## Version - 3.7.9
 - Add quoted arguments to PL/I syntax
 - Add .inc to the list of PL/I extensions
 
## Version - 3.7.8
 - Add fuzzy variable match for source definition provider

## Version - 3.7.7
 - Tweak expansion of copydir when env expansion occurs

## Version - 3.7.6
 - Add a simple para finder to the source definition provider
 
## Version - 3.7.5
 - Add $ expansion in copydir support and source definition provider

## Version - 3.7.4
 - Add a new theme
 
## Version - 3.7.3
 - Start to add some themes (all have the prefix throwback)
 
## Version - 3.7.2
 - PL/I tweaks (keywords)
 - Bring provider inline with 'C' style providers (functions for program-id, function-id etc.).
 
## Version - 3.7.1
 - some entry statements not being included in symbol providier
 
## Version - 3.7.0
 - Add very simple PL/I tmLanguage, just because I was fed up seeing "plain" text

## Version - 3.6.3
 - Tweak quotes in symbol provider (still not 100% happy but should be better)
 - Add enum-id, interface-id, valuetype-id to symbol provider
 
## Version - 3.6.2
 - Fix broken symbol provider due to removing a warning without looking closer at it, silly me.. sorry
 
## Version - 3.6.1
 - Tweak the symbol provider, heath robinson style, #34
 
## Version - 3.6
 - Add ultra simple symbol provider to mark program-id, division's and sections
 
## Version - 3.5.1
 - Add JCL keyword provider with a simple list of statements

## Version - 3.5.0
 - Add simple keyword provider
 - Tweak keywords list using IBM's table
 - Add JCL snipper for IEBGENER

## Version - 3.4.4
 - Tweak for issue #33
 
## Version - 3.4.2
 - Various tweaks
 -
## Version - 3.4.1
 - iterator/id changes for #31
 
## Version - 3.4.0
 - Bodge for "not" as operator and verb #30

## Version - 3.3.12
 - Readme changes (Thanks Simon Sobisch)
 - Add the start of some jcl snippets

## Version - 3.3.11
 - Finish off #2

## Version - 3.3.10
 - Fix issue #26,#27
 - Yet more jcl changes
 
## Version - 3.3.9
 - More jcl grammar tweaks
 
## Version - 3.3.8
 - Enhance jcl grammar, try to handle "DD *" overy multiple lines
 - add some extra jcl parameters
 
## Version - 3.3.7
 - Fix issue #25 add constraints, constrain

## Version - 3.3.6
 - $ set fixes
 
## Version - 3.3.5
 - More support for ACU COBOL Syntax
  - Fix bug shown up with an odd example with "REMARKS"/COMMENT
  
## Version - 3.3.4
 - Continue work on the jcl grammar
 
## Version - 3.3.3
 - Attempt to fix the untab with column zero issue #23
 - Add more support for ACU verbs (DOTNET/COM related)
 - Add various keywords to the keyword.control scope
 - make ACU verbs "strong" in cobol syntax, as they may or may not be available

## Version - 3.3.1
 - Remove superfluous brace

## Version - 3.3.0
 - Add some documentation for using tasks/problem matchers
 - Add some snippets for directive files (sourceformat,dialect,jvmgen,ilgen,copypath)
 
## Version - 3.2.15
 - Add simple .dir file colouriser for Micro Focus directive files
 - Add UPPERCASE version of various snippets, so if you are typing in 
    these will be matched first, not ideal having to duplicate but 
    unless I moving from using a snippet file to handing the snippets in
    code I can't see any other way.

## Version - 3.2.11
 - Start to add problemMatches/regex for compilation (related to issue #1)
 - Mark directives that are invalid in the $set as "invalid" so some theme colour them
 
## Version - 3.2.10
 - Disable messagebox in opencopybook
 
## Version - 3.2.9
 - Fix for #21 - Managed types should be coloured differently from keywords

## Version - 3.2.8
 - Tweak fix for #13, differentiate 0.$

## Version - 3.2.7
- Fix issue #18 (is crt is not colourised #18)

## Version - 3.2.6
- Mark non-print characters as constant.numeric just so it looks like other sequence number items

## Version - 3.2.5
- Ensure characters in the indicator column are consistently colour even when the line is a comment
- Ensure the continuation character is not identified as minus
- Remove .1/.2/.3 debug identifer from the grammar

## Version - 3.2.4
- Fix problem with indicator column area and comments

## Version - 3.3.3
- Add more support for AcuCOBOL syntax
- Add several AcuCOBOL specific syntax
- Tweak REMARKS/END-REMARKS

## Version - 3.2.2
- Fix issue #13 - bad tailing colourisation
- Fix warning for DocComment registration (no schema given)
- FIx warning about multi-line comments (remove empty config ops)
- Reduce number of warning message when we cannot find a copybook

## Version - 3.2.1
- Merge "Continue doc-comment when RETURN is pressed" (Thanks Ted)

## Version - 3.2.0
- Merge goto definition changes (Thanks Ted)
- Move away from using workspace to workspace folders
- fix schema warning message

## Version - 3.1.0
- Fix issue #13 - Some lines are incorrectly coloured
- Fix firstLine regex

## Version - 3.0.0
- Upgrade extension and port to TypeScript

## Version - 2.2.0
- Code cleanup
- Add badges to README.md
- Add language aliases
