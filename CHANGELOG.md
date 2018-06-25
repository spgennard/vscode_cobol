# COBOL Source colouriser for Visual Studio Code

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
