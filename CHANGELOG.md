# COBOL Source colouriser for Visual Studio Code

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
