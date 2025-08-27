# CHANGELOG

* 28a53ee ensure $region/$end-region works with upper-case
* d1683f1 fix typo and ensure $ is include in $region for range

## 25.8.20 

## 25.8.19 
* f0cfd8d Update
* 9e81632 Update
* 1ba2504 minor tidy up on hover provider for variable with one line
* 53a9234 keep in-sync with 'Rocket's extensions
* e3322be Update
* 7c982ec expand range for variables in the outline view
* 9602d91 re-locate code for missing copybook

## 25.8.18 
* 490bc15 Send overall copybook message to logger
* afb0141 make selection area better for non 01 group items
* 86c3e5f use startCol for COPYBOOK symbols

## 25.8.17 
* ac41465 tweak dump symbols
* e6d7a21 add some copybook recursion checking, so we are not relying on the depth limit checks

## 25.8.13 
* d9b3759 move static depth info to copybook state
* 8695ffa add config for copybook depth limit

## 25.8.12 
* e846f85 tweak recursive handling limit on copybooks

## 25.8.11 
* 1e0c387 tweak to handle multiple copybooks with repalcing

## 25.8.6 
* 31e2e51 Ensure rename only works on visible fields/sections/paras

## 25.8.5 
* cb2f456 Update
* ba78a93 Add found references in copybooks, when text replacement is okay, may need more tweaks #362

## 25.7.28 
* 408360f Continue work on #362
* bb98f6e fix regex for langs

## 25.7.25 
* ed45d2d Ensure ==xx== is one token #362

## 25.7.21 
* e6b110c Update
* ef53b65 add in-compatible formatter

## 25.7.16 
* 68643c0 Add back filenameDirname #361

## 25.7.15 
* a38ac00 check to see if the copybook is resolved to new location #361
* 264b8ce refactor #361

## 25.7.14 
* e4a9e91 Updates for #361
* c6c09b9 Change reference
* 60be46b Fix #361 - Use change filenameDirname to fileDirname

## 25.7.13 
* 0e7fa3d keep current

## 25.6.30 

## 25.6.14 
* 4f2d9bd Continue work on #360 - basic search, need to review caching
* fcb9bcc move per file settings into a different list
* 87649e4 Expand copybook interface to include sourcehandler, in prep for #360

## 25.6.1 
* 714ef8b update for mac

## 25.5.23 

## 25.5.24 
* 0e0fd00 enough being silly and change name
* 5ba3460 fix publishit
* a1a09d1 Update
* 8154546 add yesterday

## 25.4.30 
* 35d9a00 add extra extensions

## 25.4.27 
* 2a663de update readme
* f7984ad Expand the ILECOBOL support

## 25.4.26 
* 5a4c525 remove duplicate lines, as these are not necessary for seeing the flow

## 25.4.18 
* 2021a85 Fix #358

## 25.4.16 
* 3dde8d8 update packages

## 25.4.5 
* bb18823 Add some support for identifying sourceformat free Fix #357
* c94fffa fix a odd range end problem
* 6702fd0 add always_terminal option

## 25.4.3 
* 6c6701c add always_free option #357
* 65f9946 Bug Report: Outline Not Working When END PROGRAM Starts in First Column (Free-Format COBOL) #357
* 0b80c29 add "processing procedure" to snippets

## 25.3.29 
* a725b83 add hook for "xml parse... processing procedure" to bring up a section/paragraph

## 25.3.27 
* 439cc87 tweak end-xml to be the same as "xml parse"

## 25.3.16 
* 3cdee2f add pointer-32

## 25.3.9 

## 25.3.3 
* b82366c add interface to scanner, to aid testing

## 25.2.25 
* f3d0ecf change margin_73_80_comment to margin_identification_area

## 25.2.24 
* 5126c64 Add coboleditor.margin_73_80_comment to allow fixed format files to turn off 73-80 behavour #355
* 200e7d9 start to move fns into interface

## 25.2.12 
* 74fe8b0 fix col1-7 problem with +

## 25.2.6 
* 1c5a59d pass window state rather the field state
* a233197 Introduce some state management in window
* 637f08e rename to program info panel

## 25.2.1 
* 6e5dec1 fix bug in handling of malformed "pd/entry using..."

## 25.1.25 
* 12d88bd tidy up enable of view
* 7221d40 a little messy but its wip

## 25.1.21 
* d61931e add vscode-elements.css
* 6f052f6 add some node link support
* eb6ed05 link view graph to preview

## 25.1.18 
* 8d771bd only restore 01group level if changed in copybook and restore if not
* 845e8e5 Continue work on embedded copybook support #354

## 25.1.17 
* 9cd930b continue work on refactoring inline scanning

## 25.1.16 
* dc60eb0 simplify state use tweak meta symbol
* 096bae2 update link
* 28dcae1 add util ext's that don't work this extension

## 25.1.13 
* 952aa65 preserve 01-group info between embedded includes #354

## 25.1.11 
* 25f98f5 add panzoom
* 0588f98 preserve current 01 level for inline/scanned copybook #354

## 25.1.10 
* ec4b2ec add reference to a known variable, ignoring outline #353
* b50e060 add dir/README

## 25.1.9 
* 87a03f5 add pre-copy
* 436be31 add enablement for commands
* f6c116b add a variable reference for "to"  - could be related to #353 but no example or real detail given
* 280ab58 tweak
* 303978f drop the toolkit (for now)
* ba53af1 start to refactor webview code
* dfb8c88 move to an embedded version of mermaid (fingers crossed)
* a0c8f74 add refresh code
* 73920f4 wip - fix dark/light by using neutral theme (could be better)
* 1a52ed2 continue wip
* 6ae5d84 wip - use webview for mermaid graph  - colours not right
* 2b47365 update for new year
* 6e86535 update LICENSE date
* fb8595e tidy up and setup program-id, so we can re-use it

## 24.12.30 
* 3c4de8b interim release
* ae7f19b don't save the markdown, just use a untitled document
* 7771b6c shorten name of implicit name
* 1df1272 remove csp
* 8189ac3 diag dot file for calls diag command  - wip
* 3c38dd8 change to use the full description and include it in the refs and un-used area
* dff78e6 add top-level section/paragraph
* 1bee479 ensure section/paragraph are consistently set to undefined

## 24.12.18 
* 451d9fe add description, ensure name & name lower is okay

## 24.12.14 
* c88bcd2 add WIP comment
* 7dc0e57 add simple jit install of called by provider
* eedbdbf only use registerCallHierarchyProvider when enable_call_hierarchy is set
* 1e99c1a add enable_call_hierarchy property
* b9f63da add nearest section/method to aid calledby support
* 363238f a litte refactor and add first impl of both in/out but still wip
* fef5b2a tidy up
* 1f603d0 next step in 'called' support changer linter to use common reference counter
* ca55a5c add wip call heirarchy
* 1fb0fe4 add wip heirarchy provider

## 24.11.28 
* f71d285 update & enforce program_extensions scope

## 24.11.19 
* 0f4bffd tweak comment to work better, re-structure to make it cleaner

## 24.11.14 
* 374ae58 minor tweak
* 2472425 Fix #350
* 58cd00b test tweak

## 24.11.11 
* fa3bc1e start to simplify the conflicting extensions
* 95ad4e5 add ability to just give a warning rather than disabling the extension on conflict
* d473764 move extension check into common area  - add bloop ai extension as cause problems due to duplicate    intellisense
* d2d8f22 tweak comment processing
* dc675ea continue with the cleanup
* 48d199d remove used property and set the scope of settings that will have to be workspace
* 3d34ea5 start work on linter
* 021dd70 contine work on settings seperation / enable format_on_return
* 05e9bf0 re-introduce url path support
* f6735ee continue work on file copydir and settings
* 459b5b6 start to pass down settings
* 1f2533c split workspace get into two
* c360f2a move vs related values to vs settings
* 88385c3 rename cobolfolders to vscobolfolders
* 6567e53 add IVSCOBOLSettings and seperate vs settings v out of process settings
* ccc93c5 tidy up
* 63bd091 refresh all known settings on workspace change
* 627c3ea Update COBOL.tmLanguage.json
* 30a5571 move copybookdir handling to vscobolutils
* 06bb426 rename cobolutil to vscobolutils
* 946875c drop migrateCopybooksToWorkspace
* 1a61034 add more caching
* 00b4903 copy copybook dirs from non resource settings in prep to adding in resource folder dirs
* d610fdc start the process of moving global file search data to settings
* dcb9071 rename .get for settings
* ed10ecd rename cobolutils to vscobolutils change more .get to .get with textdocument
* 9b5c6a1 untested changes - more refactoring
* 4bb3cf0 start to use document to get resoure config  - may need to introduce a cache but get the basics working first

## 24.10.26 
* a467c9c use editor.tabstop from resource continue some refectoring
* 0bf3fcd start to centralise the config handling

## 24.10.22 
* cbd2162 harden method
* dad4d09 substring different to substr
* d83af2e fix typo and avoid directories
* e1d04a2 use specific flag for trim, tweak file reader

## 24.10.17 
* 1b5f885 amend url for community

## 24.10.15 
* c2b4b37 fix issue with scanning using with extended micro focus syntax

## 24.10.12 
* 3adeda2 ensure we don't bleed any hidden items into the codelens
* 8a61580 Update
* 2a3170a ensure references are only shown if the definition is in the current file

## 24.10.11 

## 24.10.10 
* 42fb0a0 if a var is defined more than 1 time, then do not include any reference count information
* ab97ead prep for tomorrow
* 2dff575 remove threshold hack and scan source references map (inline with variable) handling
* fc16102 Update
* df730f5 tweak exec token range check

## 24.10.9 
* 8ea2205 tweak exec sql only use source references if have something defined use source range for sql declare

## 24.10.8 
* ab9b2f3 move with the times and drop micro focus if we are not talk about the dialect.. let the Rocketeers rule
* abab92a Update
* b19c5d8 gnucobol is no-more

## 24.10.5 
* f55e831 add message about missing 'ignore ls'

## 24.10.2 
* b0384ff add ignore ls on/off to semantic scanner, so it can be marked as comment

## 24.10.1 
* 8f4526b add ignore section as "null" into the outline view
* e57dbfd rename link

## 24.9.30 
* a098a57 enable wip support for selective scanning
* 66efdc2 update readme
* 7396bcb fix a bug introduced with the rename variable support
* adb2efa continue work source dep filtering

## 24.9.29 
* ce67694 ensure program range includes program-id and last section/para does not include "end program"
* 5422872 update end of range for group item to last end known end of line
* 6681c98 include "01" in a group in the range
* 85b190b add range to COBOLToken  - fix odd range issue with source readers

## 24.9.26 
* 213debd comment out function until it is used
* 347ee27 Update
* f95dc9b add source for simple regex parsing of source-dep file using
* 99b03cb fix a stoken handler which may affect semantic token'ing wip - continue work on filesource filtering
* 6b518cc add wip, ls control token scanning
* ece6c4e some defensive coding

## 24.9.24 
* 65069b8 fix two odd bugs  - nx odd length  - multiple items per line split problem
* 178be87 filter out anything that can never be a var

## 24.9.23 
* 78e8502 add typedef to keywords turn on "sql declare" ref handling
* ffad3a0 fix bug with skip token in "pic x." partial statement
* 78eef89 continue work on modernising the reference handling, still more todo
* f317383 experimental code for add var/reference
* faaaf20 include section/para from source deps in references
* e41e117 add simple navigator for source-dep tag

## 24.9.22 
* c2fec20 avoid using non-viewable tokens with codelens

## 24.9.21 
* fcc7c9f add scan_comments_for_references and initially disable it
* 04bc06a add boolean, so we know that source dep copybook, can be processed differently

## 24.9.19 
* 5d52e78 Update

## 24.9.18 
* aec5def tweak make tokens
* c868fb9 Update publishit.sh

## 24.0.0 
* cfc45a0 revert recent change

## 18.9.7 
* b592baa dev: tweak search for references in pd

## 17.9.7 
* ca09b86 more 'rocket' related tweaks
* 7e8f38e updat readme
* a154870 tidy up use of "Micro Focus COBOL" when relating to the extension

## 16.9.7 
* ef66dae add support for the Rocket COBOL Extension Fix #341

## 10.9.7 
* f521c86 add sql from monaco sources, tweak to make it openesql labeled
* 57e8ef9 move "exec sql" into a custom openesql syntax, in-due of the changes coming from microsoft reguards changing sql to mssql

## 10.8.31 
* 012dc18 tidy up
* 0e0812c add a rename provider to exec sql cursors
* 8ea0a4f use sourceHandler from token
* 0ae1998 add config for turning off sql declare parsing

## 10.8.29 

## 10.8.28 
* 431b174 add a simple hover provider for sql declare items
* de300cc fix "sql include"
* 2cb3d80 Update
* e05c48f adjust source reference to more exact with the "exec sql" block #340
* fa79f5f refine the source refs, move the source ref list to the sql declare object, this should help later
* a26b24b try to restrict sql cursor look up to "exec statements"
* aeba4c6 add goto def for sql declare
* 034857b split SourceReference into two, one length base and the other extact (multi-line).
* 56e00b5 start to add some sqldeclare references info to the ref provider

## 10.8.25 
* 98a012c start to add in a simple "exec sql declare" scanner
* a2c8c7f tidy up and a little rename
* 0fb6c5f allow the symbol description of a "exec xxx bb" to be split over two lines ensure start of exec, it as the start of the verb exec
* fbab60f Update line information, capture end line number in tokens #340
* 95c852f Add support for remembering exec tokens in order - part of #340
* c561b2e add some support for common acu api to the hover support  - allow $ in words
* 31f37d7 index call target with document lang id  (more work required in this area)
* ceeaaf8 remove dud entries

## 10.8.14 
* 383afeb add markdown dividers between items better handle duplicates variables

## 10.8.13 
* 826fe98 add multi-line comments (upto 5 lines, may change or make configurable)

## 10.8.12 
* bb537c7 change colour comment tags to have a space prefix

## 10.8.9 
* 5396d2c add hover support for para's & sections with comments

## 10.8.6 
* 0d0e472 enable coloured comments don't complain about source files that contain few lines mark BroadcomMFD.cobol-language-support as an extension that performs the same features as this but can clash

## 10.8.4 
* 74498e2 dev: only use "*>" comment if on the same line
* 48492d1 add some experimental code that picks up comments for variables

## 10.8.3 
* e188f36 prep
* d620eea add missing region/end-region, as it makes my life easier
* b879cf4 fix $end
* 48cf85b tweak settings for language detection
* 23dc465 add 'New File'/Empty file and order entries
* 21737c6 Fix #337
* 58445d1 Fix #334

## 10.7.23 

## 10.7.22 
* 117b90f tweak perform snippets

## 10.7.9 

## 10.7.6 
* c17303d move up glob version
* 01a388e Update
* 0191584 update versions

## 10.7.1 
* b86e5ef tweak feedback links

## 10.6.28 

## 10.6.17 
* 0036b8d keep README.md in-sync with package.json
* 7a545b4 enable rmcobol/bitlang-cobol when using github co-pilot
* 9e23760 Minor tweak #334

## 10.6.16 
* f25c6cb parial docs update #334

## 10.6.15 
* f6cfed0 add info about lang server setting to console
* 3f87e49 remove unused
* 636316b remove enable_language_switcher it's impl is confused
* 061ac7f Fix 'just introduced bug' #334
* 3a1befd start to handle suppressFileAssociationsPrompt
* 0eae770 use global if ws not present #334
* e7ad5b0 Need to change active documents on previous lang id to the new one #334
* 7cafc88 Add support for updating non-workspace flags #334
* 44feec9 add extra config option to server control exec & checking is PLI lsp

## 10.6.11 
* 7f0c9f7 fix bug in the setting updates for lsp
* 46687b6 Add setting to enable/disable 'Micro Focus LSP' enable/disable behaviour  - part of #334

## 10.6.10 
* f0e9ace ensure 'flip' command is present everywhere

## 10.6.9 
* 17046b7 add fix for #334

## 10.5.29 
* 23431c2 add sql copybook file to references list - Fix #333

## 10.5.28 
* 9ab859b add temp "windows/mac" exclusion
* 901ff08 BMS: Missing items Fix #332
* 7fd0f93 slight refactor
* d158050 add some early support for my 'dotfiles' COBOL terminal
* b4e50f6 remove warning
* afc5e5e add get_dotfiles to build process
* 9c4ac21 remove eslint

## 10.4.11 
* e23b4cd add comments (COBOL and SQL), into exec sql/ado
* bc83f51 move vsce dep

## 10.4.9 

## 10.3.25 

## 10.3.24 
* 9b6da43 Update

## 10.3.10 
* 85331d4 dev: add $region/$end-region into symbol provider (helps with nav)
* 4a754a2 tweak

## 10.2.21 

## 10.2.5 
* c9b113c add a snippet

## 10.1.23 

## 10.1.15 
* c9a5463 Merge branch 'main' of github.com:spgennard/vscode_cobol
* a01a70b tweak "exec dli"
* b409c89 Update vsdirectivesconv.ts
* 290ede2 Update cobolsourcescanner.ts
* 423dec3 save away table

## 10.1.12 
* a362766 COBOL extension supplements pre-parse check
* 8c6771e standard is okay

## 10.1.1 
* b4f505f add config for the portation scanner
* b7a8a8c add "function sub" lint message
* 43e4d5e use cached linter severity in a consistent manor

## 9.12.28 
* dd7bf2b add >>elif to the non-portable bit
* ad81afa only enable directives convertor for COBOL/BITLANG-COBOL

## 9.12.25 
* b2fa865 change sub to warning
* 41c740a add source format directives changer
* 39afa8f fill into the other directives
* 2595568 add initial directives changer

## 9.12.21 
* 52fe2b1 add better copybook defaults
* 00f8e42 add java-sharable on/off
* f06c6ef add java interop reserved exception register

## 9.12.14 
* 02d7923 add java-callable

## 9.11.22 
* 10dd331 Update
* 4477ca3 change defaults for the linter for acucobol/dir

## 9.11.21 
* 0d3a19c Fix #331 - coboleditor.linter was not being honoured

## 9.11.10 
* 9e55388 add support for old cobol-lint tags
* 4cc7b85 add xml snippet
* 89c94f5 add more snippets

## 9.11.9 
* 76d2aed tweak

## 9.11.8 
* 23bae96 add $COBCPY if present and add some knowledge about known sys copybooks
* 5487208 tweak text
* 262c833 add the initial version of the copybook path fixer
* c0ba860 continue work on copybook finder
* f9662f2 tweak name
* 68caf30 typo
* bccc3d1 start the process of de-emphasising any related to net express/server express
* 68c6f89 add config
* 1c4ef74 add setting to enable tabstop anchors
* 91f9ede introduce interface for the anchor tabstops
* 3f5f5c2 expand anchor tab interface
* 1332a2e tweak tab handling avoid a slow-async startup exception (can occur when debugging ext)
* e50c0af add tabstop out of range configuration  - continue work on fixing odd edge-cases with tab/untab
* 0dd77b0 start to refactor tab/untab
* b0496ce update tests
* 4044336 add extra docomment snippets

## 9.10.16 
* a446695 allow both the formatters to work together

## 9.10.15 
* 1a8941a tidy

## 9.10.12 
* 3beb7b0 Update
* 1f08be2 now we have more tokens, only include readonly ones
* 30375d4 move
* c772007 add
* ea20a4b start to add conv

## 9.10.4 
* 929e455 update docs given devcontainer.json spec has changed
* 3496e63 cleanup
* a710d6d update versions

## 9.10.1 
* 04db34b cutdown on the verbose messages

## 9.9.28 
* cd64c5c simplify
* fac52d1 add ref provider

## 9.9.27 
* 2b9b144 tidy up the extension info api

## 9.9.26 
* 0cf45c2 github.dev returns this extension... so need to add it to he blessed list

## 9.9.21 

## 9.9.19 
* 4e9dc1d add support for "with debugging"

## 9.9.16 
* 8bded74 add simple search for subdir's directives.mf files  (might remove, as I can't get navigation to the file to work)

## 9.8.31 
* e730484 add symbol provider for directives.mf files

## 9.8.30 
* 38c4a43 add barebones directivesmf support
* 0391a93 update packages change timer to timeout
* 416b105 Update
* c8775da Simplify the README
* 34e02e3 tweak
* 14fdd58 tweak

## 9.7.23 
* 5a9f67d remove length of hover line for fixed format programs and remove excess spaces
* 354799f move hover provider into a seperate file

## 9.7.20 
* 88ef43e add MF 9.0 platform constants and extra directives

## 9.7.19 
* af4eff8 add a change log to inform users of the latest 9.0 release

## 9.7.15 
* 9e002f7 last set of changes
* 453c000 Update
* c593bf1 more tweaks to gen changelog script
* c9b654e Update
* ca88fe9 only apply margin if enabled
* e476e8d Update
* 5d318d7 add some diag information and add default for anyone using this extension with the Micro Focus TM COBOL extension
* 33ead97 tweak

## 9.7.14 mac osx/broke the changelog creation
* fa2a001 tweak for co-pilot

## 9.7.10 re-work for mac (lack of -d on xargs
* d99fcad add header
* 0624a4e tweak $if

## 9.7.1 
* 8e59b79 add space
* d76817e Add "TM"

## 9.6.25 
* 454ab27 make switcher configurable and off, until completed
* 936c3f2 add switcher support

## 9.6.20 

## 9.6.19 
* 3077211 add threshold for display'ing references and enable

## 9.6.18 
* 5d1113d add snippets

## 9.6.16 
* a4f8511 move iso snippets to programable snippets

## 9.6.15 
* dd776f0 Update
* ad8aedb don't activate default variable provider if $ is in use tweak snippets
* 2be095e Merge pull request #328 from spgennard/spgennard-patch-1
* fee21a8 (origin/spgennard-patch-1) Update README.md

## 9.6.13 
* 6ef85ba move $ snippets into snippet provider to try avoid some odd behavour
* 88380f4 tweaks for extra dialect (for remarks)
* 9f3fee1 remove duplicate which crept in with a regex/swap

## 9.6.10 
* 5b21835  Code coloring issue with REMARKS keyword in variable Fix #327

## 9.6.6 
* 54cb477 dev: avoid "" command message error when double clicking in int's etc in source view
* cbca9b7 update version

## 9.5.6 
* 78ecdfb refactor simple copybook drag/drop provider
* a96c24d add a simple copybook provider
* 3613a08 add ideas file
* f57326e tweak use time from file

## 9.4.29 
* 50996b3 tweak intellisense space handling add limit to overlay view

## 9.4.25 
* 380edf8 Update
* e4135ef add snippet
* 18711bf updaste

## 9.4.20 
* 8e33137 Update package.json

## 9.4.19 
* 7ee0fd5 remove close left menu

## 9.4.12 
* dae60f4 update to use later glob package
* 4a79947 tidy up
* cbcfcac update (without glob

## 9.3.19 
* f2ea18b add simple support for "Close left tab" context menu

## 9.3.2 
* 7900c38 update (keep glob with a lower v num)
* a67d774 tweak
* ba08678 Update
* 293be78 tidy up package.json in prep for new release

## 9.2.23 
* 9de6b95 expand to all COBOL dialects

## 9.2.20 
* 01cd990 add lang status bar

## 9.2.15 
* b2d2912 Merge branch 'main' of github.com:spgennard/vscode_cobol
* c33bbac move away from minimatch
* c0d61fe tweak matchers

## 9.1.28 
* 5231735 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* 4a37f85 tidy
* 95c9152 add some diag routines (temp)

## 9.1.15 
* b7aba96 add "exit paragraph"

## 9.1.11 
* 98d9c5c Update
* babb2dc new year... new major v
* b222f1a update cics json and remove extra double redirection of $1 (which does not make any difference)

## 8.12.20 
* 6ee99f6 add some really simple "exec dli" support

## 8.12.11 

## 8.12.2 

## 8.12.02 
* d23458f remove quotes when searching for copybooks

## 8.11.26 
* 3b7302a add known variables as references when in a "exec" block
* f1ca4c3 identify each "exec xx yy" in the outline view

## 8.11.25 
* 34986a4 Fix typo #323

## 8.11.20 
* ac7bb3b add 'exec's to the symbol providers
* cfc717a don't trim literal unless we need to

## 8.11.10 
* ea4d0f6 simplify the editor file nesting

## 8.10.28 
* 301547d cleanup
* 53c49c9 remove definitions from references provider
* 723be08 tweak for linting
* 6cd4d50 tidy up and ensure we pickup unused refs
* 872b201 add a couple of config flags for the experimential codelens
* 752337e explore use of codelens

## 8.10.15 
* ef129b6 add simple support for picking fields from the comms section
* 7db7976 move splittoken into sep file and tweak it
* 289d50c update versions

## 8.10.8 
* 1a43f71 Update
* 5ba7481 minor tweak to include MFUPD files
* 1d065e0 simplify source view

## 8.10.6 
* f3de30c tweak to pickup missed ref-mod items note to self - time to look at this closer
* 4c430c9 merge two classes and simplify

## 8.10.4 
* e1a03d1 handle duplicates better in flattened source view

## 8.10.3 
* 9e32dbb add markdown/pli injection
* e8716e3 add markdown/hlasm
* 1527321 add markdown/jcl injection
* 46498d0 move json files to a markdown subdir

## 8.10.2 
* f347915 move to a full range rather than a location (fixes odd empty hover)
* e417be4 scan inside brackets and pickup 78 items eg: pic x(MY-78-ITEM)
* c14ce69 add embedded markdown

## 8.9.26 
* b14241a allow filestrategy to overide margin

## 8.9.21 
* 0a99031 remove duplicates

## 8.9.20 
* c8c2f58 dev: fix bad url

## 8.9.19 
* faf70ea ensure we pickup all file.assoc
* 4b6d408 tweak
* 12c9361 bring back the old margin config option (like the jcl one)
* 3bb8d60 handle rename and drop field not used
* 0a440b3 remove unsed param rename file to be inline with others (gradual removal of this debt)

## 8.9.12 
* 286ed0a fixes for sourcetreeview

## 8.9.11 
* 2ae1e1b tweak sourceview
* 3571eb1 rename
* 96a16f5 add rec for linter

## 8.9.9 
* e48d428 allow intellisense style to be specific for user commands
* 8e7d015 start to add support for copybook open

## 8.9.3 
* eab2d9a Update
* 73decba use uri and avoid Uri.file
* f2021a1 tweak rename to use embedded urls

## 8.9.2 
* 923587d use known uri and avoid hardcode uri.file, should make find all references work better with non 'file' based schemas

## 8.9.1 
* fc11814 add some extra schemas/fs's
* f33bde6 add support for "copy in" with urls

## 8.8.31 
* 953ec28 add first part of open copybook via a non-file based URI
* c0d6178 continue the work on URL based copybook finder
* c51e777 add simple search for base copybook url directories  (not used)
* e300ccd add some more verbose messages
* 94fc49b start to prepare a place to put the non-file urls for the copybook search path
* 5e4f65b ensure JCL is registered to all the known schemas
* e807f50 add 'ftp' schema

## 8.8.25 
* cd08622 remove @'s as it did not work
* ff2fb62 tweak to allow search to copybook to work without parsing its content
* ded58aa prepare for next release
* 6ce2e1b merge format on return with intellisense code  - breaking property change, format_on_return is now a boolean
* daef7ca start to add support for custom intellisense rules
* 2f380e5 start to add support for configurable keywords snippets
* a133b4a tweak docs

## 8.8.14 
* d21bb4b allow enforce file associations to be used with ACUCOBOL & COBOL

## 8.8.13 
* 0deb921 add newFile for ACUCOBOL
* c6d89f0 add new unit test

## 8.8.11 
* db3d593 add extra keywords
* b737268 add support for change from COBOL to ACUCOBOL if AcuBench messages found in first two lines of the code

## 8.8.10 
* a052a98 move lks to ACUCOBOL
* 4c95a86 Update
* 693fe9c Update

## 8.8.8 
* 8e66ba4 refactor newFile to make it easier to introduce extra ones..
* 917cab1 Add mfunit ep snippet

## 8.8.2 
* a9a43e5 add trim
* a0b122c add hex-to-char
* 4add7ae add function hex-of:

## 8.7.25 
* 1e97718 minor updat
* bd82ee5 add missing exhibit from keyword list

## 8.7.15 
* 1140239 tweak
* 1023a4a make cics macros look better

## 8.7.14 
* db4a0e8 Update
* 720958f validate filename

## 8.7.13 
* dfce032 prompt for filename
* ddd1a02 tweak newfile

## 8.7.12 
* 3422214 add new file
* ab37907 add b"0" & b"1" support

## 8.6.21 
* 0553a48 add more feedback items

## 8.6.18 
* 0d55a90 fill in copybook name if recursive metadata is set

## 8.6.13 
* 85c2ce1 Update
* 92330a2 Update
* 4b2bf61 Update
* cf6682e tweak

## 8.6.10 

## 8.6.1 
* a754108 tweak feedbacktree
* cb58f86 add feedback tree and tweak config
* 4df6a4d Update
* d2fa046 add "lst" file to documents on sourceview
* 84a1e7a drop sdk ignore
* 73167aa minor package updates

## 8.5.26 
* 66c84b0 add uppercase ones
* 9aa6559 Merge branch 'main' of github.com:spgennard/vscode_cobol
* 9756ec3 add uppercase variant for internal cpybook extensions
* 4e093bc fix typo
* b8b9dc6 Add extensions: cblle, sqlcblle and cblcpy #316  nb: .cbl is already present
* 79e9655 drop module
* 028298a more tweaks for utf8

## 8.5.10 
* cbe2ffa add some pic u support

## 8.5.9 
* addd5cb Update
* 13f41da add .so/.dll support to the sourceview

## 8.5.8 
* 2243d9b start to add file explorer context items for .int
* 09c69bd refactor debug commands to utils

## 8.5.2 
* 039f0c7 add support for file nesting

## 8.4.26 
* c61f475 if source scanner is disable, turn off other features as well

## 8.4.25 
* dfb2ee9 add some basic debugger integration (for .int/.gnt) into the source tree

## 8.4.24 
* 88cf031 allow lowercase hex in hover's
* d5ab1fb experiment with generating .ts.d files
* 771309e increase strict'ness

## 8.4.23 
* 2e383f4 add >>evaluate
* 19dfb76 start to add some iso2002 conditional expression snippets

## 8.4.20 
* 644c3cd add some conditional compilation snippets

## 8.4.17 
* d3395ce continue support for nx utils
* 70dc0de change substr to substring
* 2ed8785 start to add nx support add some extra lint's

## 8.4.13 
* 73781cd simplify jcl keywords

## 8.4.11 
* 08c6920 refactor previous change to make is more generic
* 82fb632 don't include a space with the keyword "section"
* 7b55c1b tweak for DFHRESP
* c61085e start to remove Null object in preference to using undefined

## 8.4.2 
* cc17327 second april release
* c435a07 apply tweaks
* 1a6a5bd rename token.nextSTokenPlusOneOrBlank and param index
* ccc623f refine README

## 8.3.30 
* 7da1ca0 include prop to allow file format setting check to be done early or late
* 1779b9c tweak fileformatStrategy and make it an enum
* 46985ab allow file format settings to override sourceformat
* ebf88dd remove "exec" on label
* 3e45ee7 add check for "sql" exec for sql include

## 8.3.29 
* bb9a4a4 add some basic scanning for "exec sql include"

## 8.3.27 
* 1260dfb add config for hover support (hex)
* 6b4b312 add menu for text conversion tools

## 8.3.26 
* d0ad3aa add a simple hex to ascii hover and fix a case search for a api snippet
* 0099705 add config to allow 'tab' behavour to turn off (if required)
* 3cd7efc add support for using tab with inline snippets (github copilot)
* e09658d preserve case on copybook stash away type "exec", maybe useful later
* 7359095 add utils for selection to hex & back to ascii

## 8.3.25 
* f1d5e9b only reset decls if enabled

## 8.3.24 
* 31abde2 Remove margin.color support, as it not working as expected #315
* 40172a1 move "common" command to seperate file
* ec7b8af start to simplify it
* 10a0c70 refactor
* d72b6c4 add missed param
* 9bee4f0 drop another .get()
* eba9a5d remove a VSConfig.get()
* 394d6ec fix inclusion of bad copybook in the outline view when replacing is used
* 326dcee replace dep methods
* 45ec309 add two more command to web variant
* d45115d replace .substr with .substring
* 0e56f49 refactor
* 8a83a63 update keywords

## 8.3.21 
* 42a14b2 move external configs inline and protect against too early use of external when isDirectory is used via a config (to review)

## 8.3.20 
* e14edcb fix typo
* a2a06f6 partial sync

## 8.3.19 
* c5c9355 fix typo
* bb84c9b Update
* c1d138d start to make the prefix'es more consistent
* 8ee2a35 add section/paragraph rename
* 4760969 add support for renaming a symbol  - still more todo
* 8f49eb0 update versions

## 8.3.16 
* 8d4f103 add padto72
* ba3825f add extra ext exclude
* 5241b2b add support for making the margin colour configurable
* f83bcf9 break the margin support into easier methods
* e50c68d add config to enable column tags
* 152d79b add right margin tag support
* 9aec9e9 add initial support for left margin tags
* 7a0d91b add v0 of margin colouriser

## 8.3.13 
* b68f317 tweak
* 95ca364 empty package area out
* 6cff80f refactor part of comment support, so tags can be comsumed in the margin part
* 2626cb2 wire in more events
* d2493ed try using collapsed sections
* 4a231cd make embedded tab inline & margin's more function on the first margin area
* 66b17d1 update versions

## 8.3.12 
* ede4a06 tweak tab usage
* 1068a2a remove warning
* 0d832d2 be "more" specific

## 8.3.11 
* 075a3b2 Update
* 5448f4b use different url for the information

## 8.3.10 
* 9bcdc37 tweak README.. more todo..
* dbc9b13 add comment
* b575183 remove odd looking matrix
* d8b014e x 400
* 93defae x400
* c4e6888 add 200x

## 8.3.8 
* 76bab7d tweak enforce extensions
* 740f62d continue work on files.assocations
* 7d9fd6d fix over active colourisation if "test case" is used

## 8.3.7 
* a839272 tweak keywords to include intellisense if snippets exists
* 15d928b fix type and update skeleton
* f49888d remove diag and start work on file.associations code
* 467c23f add clear to ensure maps dont expand on re-init

## 8.3.6 
* 979ced6 continue with the function support
* 94a2ec6 move function's to snippet provider
* f82d3c6 start to refactor for "function" snippets

## 8.3.5 
* c6fe3b7 fix bug in hover
* c9185fc continue to simplify
* 2327887 move display to dynamic snippets
* ed876ce update engine
* 41c59a3 convert more snippets
* afe7f87 update versions

## 8.3.3 
* dbea506 continue to move more snippets to the snippet provider
* b2148b6 dev: allow debuggers to get breakpoints

## 8.3.2 
* e62f47c move a couple more to the snippet provider
* 2180e60 break snippet provider in two, allow for multiple of same keyword
* 4c41c7a start to add support for extended keyword snippets

## 8.2.27 
* 7f018fb add untested cobsql_dir (for future consideration)
* 7a1f103 add initial version of lang file for mfupp preprocessor mfupp.dir file
* 0cc295e add lowercase support for format on return.
* 17a4063 centralise intellisense style into one enum

## 8.2.21 
* a27e378 Update
* 3af6e2f fix
* 524556c Update
* f4634b3 update changelog
* a4f5756 ensure map is reloaded if format_on_return has changed
* e9059d6 reduce complexity of snippetprovider
* d13b119 Update LICENSE date
* 04b57f9 complete the support for 'folding' snippets
* 8ca9a8c add support for changing case of setting
* cd408cc sync extension to web.extension

## 8.2.19 add more help with conflicts
* 3ed97ba use default lang
* 994d645 move 'bitlang.cobol' to extensionDefaults
* 2a6f4b2 sync the two variants of extension
* a7e3e1c start to tighten up checking of debugger extensions that do not provide support for the dialects provided by this extension.
* f757ab0 fix typo
* fdc7fa7 centralise 'coboleditor' and default lang
* 87f7493 update for minmatch versions

## 8.2.12 change "full" to "long" in enum for snippet style
* cacd337 update packages, flip to new test package for vscode/electron

## 8.2.11 enable short hover and change from a boolean enabler to a enum

## 8.2.10 include decls in the example for the snippets

## 8.2.9 fix snippet config and showing of the example
* 5ff2dc6 add prop for snippets activation
* 9351d88 add schema/format for color entries
* 315ee87 add a bit more support for dynamic snippets
* 36bb5b4 save param info
* df48e96 add -const's, allow quotes to be replaced
* f9a5196 redo mf-cbl_apis after 'any' param fix
* 1b7b59f start to fix problem with "any" paramter
* 1e0688f add some static/defined prototypes
* 340bca0 move dynamic snippets into api lists
* b3989f4 refactor
* 8d5ad29 add v0 of the dynamic snippet provider
* 2376ad1 continue work on dynamic snippets
* 300912d remove snippet
* 85e62e0 remove item
* 3466a0e wip
* cf2425d quick prop rename
* 86e05ac move from the loose json impl to a class/interface
* 50acef9 add mfunit api to list of known apis
* c5087f9 add some diag code (which will be removed before publish)
* 4801f9f add more description messages
* 3616897 quick rename
* 5f7a81d tidy up
* afceb71 tweak attrs on colour
* 3adbe05 continue to build out the coloured comment support
* 27317ab add comment word tag
* f4aed2b tidy up
* 7fcf73a add config for coloured comments
* 75a8961 start to add comment support, that works will fixed, modern and acu style comments.
* 8484e2f remove anything todo with coboldoc
* 1af3567 update description
* 36593a7 remove 'old' coboldoc snippets and useless 'ruler'

## 8.1.25 add coboleditor.hover_show_known_api property to enable known api hover support.
* 541628c start to reenable the API hover support #313
* 19cae44 continue work on comment handling
* cfb0606 make on 'events' to be full async
* 402cb4f expand callback interface
* 1d37a00 start to setup support for multiple comment callback handlers
* 87de700 tweak
* f7ade4e apply a small opt that should help scanning performance
* e590709 fix typo
* f73603d add some crude "of" support

## 8.1.24 add member to know schemas
* 3b7ab37 reload window if outline view opt has changed
* 980e8c5 add streamfile
* cb1a52c fix bug, where fixed comments would not work with hint comments
* a3095cb continue the tidy up
* 7df8278 tidy up comment handler
* fa079f6 remove coboldoc support, it never processed after raising the initial issues, sad but that is opensource software... often started.. mostly never completed
* 8c329ff add pointer
* 36db9b4 tidy up

## 8.1.18 add couple of extra "align" split items
* 018b695 update packages

## 8.1.17 continue work on align storage items
* 8365aea add submenus
* ebe98c8 add align center
* 96043d4 sync extension
* 616fbe5 drop it
* c5c9d9a tweak 'program-id'

## 8.1.15 add "align storage wide" support
* c54b8df tweak
* fecee91 drop document symbol provider
* 9ec23ae simplify sourceformat get
* 30e00b8 remove VS from class, as it is missleading
* 559b3a9 introduce lite source reader
* 45bce3f substr to substring flip
* 2498e1e flip substr/substring
* c3d8e09 flip substr
* 27e4a75 change substr to substring
* 8986ba3 move function
* 9a92ef5 revert
* ae76574 flip to using a source handler (next, source format)
* bcde529 simplify ctor
* 87958cc flip substr to substring
* 0a831be improve align data items
* f952760 drop boolean and just do it

## 8.1.12 continue to refine the align storage items
* 23d57f6 add cblproj as xml

## 8.1.11 merge branch 'main' of github.com:spgennard/vscode_cobol
* b3be2f5 add "align storage items"

## 8.1.10 Merge branch 'main' of github.com:spgennard/vscode_cobol
* d260086 tweak README
* bc1aa14 tweak cond/set syntax

## 8.1.8 add snippets for bitlang-cobol
* 7419901 breakout cobolit snippets
* 71578c4 break cobol snippets out, so the acucobol one can evolve

## 8.1.2 prep
* d0c0752 add a missed one
* 738ff13 add some openesql types
* 924b1ba add 'sql' type
* 7898095 tweak $set items remove space with terminal format check

## 8.1.1 fix outstanding bug with tab/detab activation on ACUCOBOL/COBOLIT langs
* f0d2197 put a bit more colour into the pp syntax/simplify
* 768cbc6 version updates
* c44ef4f don't need this in git
* ea3894d fix some problems with regex's in bms and add color
* 354098f tweak bms to fix some obvious errors
* 21e579b add support for bmsmap files  - selected automatically when .map extension is used and it looks    like a listing file and has a bms definition in the first 10 lines
* 80a544f upd

## 7.12.20 add support for suggest_variables_when_context_unknown
* ef35284 ensure exec blocks are handled like other verbs
* b479501 break numbers out of picture clauses
* 841835c some tweaks for "exec cics" start to move some duplicated items to the repo (for use with exec' blocks)
* c31222e tweak for cobol-it
* 3fb6cbe add COBOL-IT keywords list

## 7.12.16  COBOL-IT functions

## 7.12.14 update engine
* 1bbb045 keep cobol-it keywords insync fix changes found by later typescript compiler

## 7.12.12 tweak settings
* 8e15dd3 add in-memory cache prop
* 2619b94 extend diag information
* c368826 tweaks
* a5b2d38 make prop consistent
* 2fc81c4 remove dep method use add line length prop (WIP)
* 650442c fix use of editor_maxTokenizationLineLength, start to include settings for file exclusions
* 35b3566 start to setup some core for aborting a source scan early
* 8bd3987 tweaks
* acfff9a add more functionality, hopefully avoiding everything depend on fs
* 80ecd45 add info message
* b865cf4 change scope
* 0f245e8 move function to class
* 893a417 move two function into util class
* f4fc515 refactor VSSourceFormat
* 3963081 refactor VSWorkspaceFolders
* 671991c refactor margindec into class add margin support to web ext

## 7.12.4 tweak
* 0e4ddec remove debug code
* 20bab31 remove reference to dep'ed vsix extension and reconsiders it use

## 7.12.3 remove line
* 393a3bd add more schema's in

## 7.12.2 add diag code
* 0d8c013 Update readme
* 64adc11 Tweaks to fix #308
* 9467360 remove use of path.normalise
* ce7699a put try/catch in place (to remove at somepoint)
* 2882f79 add external browser
* b16ecc5 add in untitled schema
* 0118ad6 move to simplier changelog, as the java version crashes too much

## 7.12.0 tweak and prep
* 8fb6320 continue tidyup
* d02d322 continue tidyup
* bcebf2f remove some td
* 1936aff cleanup
* 97ae287 remove manual build of cobscanner add missing symbol during scan
* c9c7e31 Fix a problem with editor.bracketPairColorization.enabled Fix duplicates in keywords list Simplify cobol provider
* c79dd71 tweak margin handling
* 8469bbb remove sdk, as the pp was not completed or a complete solution
* 1be8f0d continue to remove pp
* 8a23b16 remove unsed tables
* 02bf054 exports tidy up
* 37ecfbf update acu_cobol-warning-ccbl and add testcase
* c09b94b add testcase & tweak acucobol-ccbl
* 6ee722b move isFile to features
* c3b83f4 tighten code
* 92e2035 continue the sunday afternoon tidy up
* a2b1aac remove maintain_metadata_cache_single_folder, as it has not real purpose other than to disable caching and we have property for that
* 1e99a3d remove the ambigous coboleditor.process_metadata_cache_on_start property
* 5b85558 continue to remove cache directory handling code
* b5c577b reorder
* 7482cfc ensure a test is present for msbuild pm
* c1d6ee0 re-enable some old tests that had been turned off
* 1fc2a8c tweak ctrl+/
* b23539a Merge branch 'main' of github.com:spgennard/vscode_cobol
* 0c69946 tidy up deps
* 973a529 remove old-cache options
* 6f97f85 remove cache dir
* a0f9d11 remove dep'ed cobscanner
* 3d7b1ca start to remove depreciated features
* 0d02c7b Update stale.yml

## 7.11.1 add info about review
* ed59814 change work to be more forceful with reguards dep to be removed features
* 51259f2 tidy up import/drop "export defaults"
* f1b4e19 tidy up

## 7.10.30 remove empty impl, tidy up fs usage again
* eea72b4 tweak $set for ilusing
* c1bc020 flip quotes
* 263f940 move vsextutils into sep file
* 5541d58 add document symbol provider
* c8c636e move alway from browsify-fs
* 3802dc7 contine work on moving 'fs' use
* c58aec4 apply rename to export & add more polyfills
* 174c3f2 remove one use of fs
* 4f7cd35 start to move 'fs' related items into isolated area
* 478ac16 start to seperate out use of 'fs' move dep into sep classes
* 1138010 rename miss-leading method avoid using 'fs' in vs settings wire in tab/untab into web extension

## 7.10.21 missed one

## 7.10.20 add some experimental support for web extension
* 31b7012 disable "editor.bracketPairColorization.enabled" as it causes odd colourisation with string & unstring tokens
* 182d5c6 tidy up
* 20d16c2 enable editor.bracketPairColorization.enabled on a per language basis
* 9204f81 Fix bracket issue with ref mod items

## 7.10.15 add some docs for xedit'ish keys

## 7.10.14 add transpose
* d434d01 fix bug in "'perform" isense

## 7.10.13 add some simple keymaps for xedit enabled via config
* 2bec112 editor.renderIndentGuides has been removed in 1.61
* 876a46d fix alignment in README.md table

## 7.10.9 tweak
* 62e8bbd change adjust cursor to ctrl+alt+a and add adjust left margin ctrl+alt+l

## 7.10.8 Release 'indent' to cursor - ctrl+alt+i

## 7.10.4 add BITLANG-COBOL id

## 7.10.2 remove reference to switcher
* 89ceb8d new month
* 795ca4d remove possible security issue with npm package (nth-check)

## 7.9.21 tweak acu syntax
* 78dd575 tidy up/fix bug
* 3d8631e update acucobol keywords
* ee734a0 just-incase code to ensure we don't get duplicates in the lists
* 3aa8f79 update syntax
* e5b4dc4 start to make keywords language dialect aware
* 5a9f036 simplify changelog

## 7.9.15 update changelog
* b65a551 remove use of prefer_gnucobol_syntax
* 656379e update to next vscode engine

## 7.9.8 prep
* eaa8529 handle single line '/' - Fix #297
* 817ab58 dev: add some validation
* 1b04ff7 dev: remove last items related to lc_cobol enablement
* dd768dc add config to package.json and add reinit() and ensure init() is used when needed
* 9b6f149 move list of id's into config (wip) ensure first paragraph is picked up when we have a "fake" pd
* e3f1cfc Ensure '/' is treated as comment during file processing  Fix #297

## 7.9.2 upd

## 7.9.1 upd
* 54da32f prep for tomorrow
* 1a5a4a4 Tweak group items #296
* 7b56b7d tweak
* 6c89eba remove warning messages
* 63dd216 upfsyr

## 7.8.8  CHANGELOG.md
* b98e0a5 tidy
* 1c5441d remove markdown warnings

## 7.8.2 change to jsonc
* 1817227 remove dep'ed fuzzy search

## 7.8.1 refresh
* 283ef8e remove features that extended the 'Micro Focus COBOL Extension' to be compliant with its license.

## 7.7.22 update ver
* 72b9b13 upd deps

## 7.7.21  CHANGELOG.md
* 220f3b2 add config for debugger ext

## 7.7.19 prep
* 475200e disable it, for the moment
* a799b9a add start of the debugger extension for special registers scope
* 19832c7 tidy code
* 8262968 Do not display the margin, if the line contains tab Fix #293
* f079ada Apply fix under property - Fix #290
* 51a36c9 remove use of trie class
* 4dcd938 tweak provider
* 53b12ca upd
* 87186f8 Go To Definition with MF Cobol Extension Fix #290
* a535ae4 remove ";1", that did not affect compilation

## 7.7.3  CHANGELOG.md
* a4370f5 prep for delivery
* 9f8e1ea update version

## 7.6.30 add support for migrating tasks to mf
* 3853f26 flip fix
* c66f8b8 upd lock

## 7.6.24 ensure metadata is not used when extending the mf extension  - leave in memory cache in place
* 8257e28 enable a bit more, still wip.. so this might change...
* 0b92792 avoid the possible double flip scenario
* 94e04c3 add support for picking up source format from the mf settings  - this enables the commenter to be in-sync

## 7.6.22 prepare for delivery
* a42eaf7 fix compilation bug introduced by previous refactoring add support for using the 'real' Micro Focus extension
* ca8298a add dynamic context via settings (WIP)
* 9e534e1 add the building block of dual language id support
* 5cc5fbf add lc cobol enabler
* 78d48f8 refactor debugger check and allow people using gnucobol language to bypass the check
* 7d5fe7a flip scope
* 8b89b40 changed missed scope changes
* 173fb84 Update README.md  - make labels easier to read
* e73587a tweak
* ad077c1 remove non-existant ref to language
* 8d5f325 cleanup forgotten unused files
* 5141f52 upd lock

## 7.6.12 update related to limited functionality mode
* babde13 upd lock

## 7.6.11 add some untrusted editing support

## 7.6.10 update README remove unused property
* 49571be use cmd for dep'ed command
* e4506ee move getSourceFormat into a seperate file and add langid to doc, so it can be used with out of process scanner
* dac47cd change process_metadata_cache_on_start to be executed via a private command and change comparision Fix #286
* b44411d unify coboleditor.margin & coboleditor.fileformat_strategy
* 92bb604 some lint based tweaks
* c45593c refactor a little more
* 7b2baad continue refactoring
* fdd8df9 use alternative update mechanism that caused update events
* 207431e next method.. to vslogger
* 702b1aa move stuff to VSLogger
* cb778ef move one more function into the vslogger
* 0d32568 continue refactoring
* b41d041 move performance_now
* bec159e move isDirectory
* 8ed4bfc continue refactoring
* b8fbfcf merge function
* e49b252 update due to typescript changes
* b3d6c4d Margin setting not respected Fix #285  - blank lines caused fixed file format detection to break  - lines > 80 should case fixed file format detection to be considerred  - allow coboleditor.margin to overide file format

## 7.6.5 add more abbr to snippets add more information to the extension checks
* 28287b1 add quotes
* 7771853 continue the seperation of the depreciated caching support
* a7d5b1c continue refactor file handing
* e1863e2 continue refactoring the file apis, into vs based ones & non-vs

## 7.5.30 only show threading option on large workspaes
* 7aa4c38 remove check for ..x perms, as if used needs to be done everywhere...
* 3ff06e3 add option to enable recursive search but disable by default
* d759640 continue refactoring cobscanner/vscobscanner  - move file scanner to own mechanism  - use workspace findFile approach for scanner  - drop unused list of directories
* 4f53522 continue to refactor the vscobscanner
* 68874ba start to move vscobscanner code to seperate class, so the non-dep can be moved forward without the restrictions imposed by the old dep'ed code
* 415795e continue work cobscanner
* 1cdfab0 continue work on cobscanner
* d7e44f9 continue work on cobscanner  - fix bug with not updating a timestamp
* 9fee468 commit performance changes for cobscanner
* b52db85 unlink not required in !depmode
* 815f11a continue to work on scanner  - useenv mode setup

## 7.5.22 prep
* c8feba4 fix a 'replace' problem start to refactor cobscanner
* 7647f5d update ref
* 00ab8a7 start to make it obvious which bits of code are dep'ed
* a26ebb3 try to fix user's broken config
* c8de26b expand callable synbols to include the line number
* a4597c1 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* baa522f reduce the amount of items cached to the workspace at the expense of resolving them via the workspace
* 6b1bca1 Update stale.yml
* 1f93399 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* f675bc0 avoid bad entry in cache empty cache if depreciated setting is used
* c114271 Update publishit.sh
* 7c21aa6 use a inactivity based timer rather than a length of time timer  - inactivity is a lack of any message from the scanner

## 7.5.19 VSCode become unresponsible after run processAllFilesInWorkspace command in a huge worskspace Fix #282  - Wire up cache_metadata_time_limit, so processAllFilesInWorkspace can be cancelled early  - Change the process indicator to be finer
* 042414b remove npe

## 7.5.17  CHANGELOG.md
* bd39204 don't change unless it a COBOL doc (ie: pp output)
* ad90ab7 flip to COBOL for temp doc
* bc016e2 tidy up
* 1fa50e3 use range
* 74dbe6e continue work on copybook handling
* 1395bc5 only have word sub for the moment
* ea376a8 add a codelens for 'copy replacing' results
* f0f864a start to wire in existing token'ed copy statement  - fix start to be before 'copy' verb
* cb23430 rename replace boolean, to allow it to include replace/replacing
* 592ea5e add range for copy statement
* 55dd15f make replace/replacing work better
* 075d24f add lowercase cobol alias for the snippets
* 2f747eb move version forward
* 5dff120 tweak
* 1f682eb avoid security issue
* 632ccd0 continue work copybook handling
* ee4fc15 fix navigation of "copy of.." but this mechanism might be short-lived..
* 6ed3cae use only one style

## 7.5.2 prep
* e774594 add experimental replace verb passing under option
* 3dedc2a try to handle mf compound entry-statements
* ec4673e simplify code
* d619a8b tidy up token handling
* 93b2a03 add an initial impl of 'replace' functionality
* 71f14d0 fix offset for 'fake' filler items
* ba57b30 continue work on tweaking startColumn handling
* 753beb4 start to change COBOLToken/Token to be performant
* c8fa598 fix bug with paragraphs and continue token/refactoring
* 3ddf875 remove condition that is not required
* dc4c57d tweak token
* 131377b place the file system correctly
* d596bfb ensure pp turns off parse_copybooks_for_references
* 838a20b create new doc on pp codelens
* d2f368f add codelens for pp, so we can see easier what is happening
* 49be90d use the copybook scan tokens
* 922d811 warn about out of sync cache when debugging and don't reparse

## 7.4.15 tweak packages
* 6e58a11 add linter_ignore_missing_copybook
* ee0adca add linter_ignore_missing_copybooks config

## 7.4.14 use a good default, for ref type, ignore unknown style
* 973c3e4 remove unused code
* 28d41ac continue work on references change $end-region to match $region

## 7.4.13 performance tweaks
* fc17b8d prepare for delivery
* 71fa75b ignore warnings
* d6e5ea8 honour visability on references
* 5d90771 continue work on forward references
* c719f11 restrict picking up of forward references
* 0a50a98 tidy up
* 430c3d2 ensure we don't get duplicate interface, enums in the cache
* 3b3a8fb tweak $set and tidy up COBOL/Messages on startup
* 533c8bc more updates for $set and readonly semantic colouring
* 4f03953 pickup variables for references in value clauses

## 7.4.10 prep for delivery
* 816bfcb remove unused var
* e3ff120 remove duplicate find from source definition when "call xx" is found

## 7.4.8 comment out unused lines
* 28463c6 add extra diag messages
* 6f9730f delivery it
* 47511c4 add warning message for untested environments
* b140ac5 ensure OVSX envs are not set
* 6a0d018 tidy and add a warning message
* 85933ff don't blank but set it something bad
* 93aa195 tweak package.json to ensure publisher is set before hand
* 7660bb2 add pre-commit hooks
* cdf164f add pre-commit hook

## 7.4.7 prep
* 995db58 wire up
* ecb9561 prepare for delivery
* d54f821 add some support for references in level 66 lines
* 94adc04 version updates

## 7.4.6 continue work on references & prepare del
* 426c349 include schema and fix bugs shown up
* f21fc67 relax string to allow space for name elements
* f30cd82 tweaks
* 533b802 add schema

## 7.3.30 try to make the pp activation more robust
* 5e97ee3 add late sourcereference cat, as it is not a 2 pass scanner
* 60bbfd4 update packages

## 7.3.29 prepare for delivery  - only say the pp failed when it has fatally failed & log it
* 414647c re-enable "decl" section processing (hopefull does not break anything) tweak setting of fixed format
* 7889b57 fix problem with margin due to duplicated code
* c2a6a3b make the determination of fixed file format better don't register interest in tokens that are literals  (long standing silent bug)

## 7.3.27 add some bullet proofing
* a66f91d expand sourceref to include tokentype
* 2eeb7bf add variables into semantic provider & config option
* 99fbe95 add the section/paragrams as "label"/declaration semantic tokens
* d23aad6 make it safer
* e0b6825 drop alias
* d910312 tweak logic on how to flip to the COBOL lang id

## 7.3.26 resort to changing the document type rather than having an alias lang
* 2955204 remove aliases and ensure enablements are correct
* 5e89ce0 package updates
* b542a9c update readme:
* 10bc188 add some control about how a "single folder" workspace is updated

## 7.3.24.1 tweaks
* fc054d1 use of support.variable.cobol, was too gready, it colourised too much
* c3c3d03 protect update and log exception
* 0fcd1df Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* cfaac98 prep
* bd5c88b Tweak update() to happen on a workspace and if enabled Fix #270
* 3d00b2a enusre alias "cobol" works as well as "COBOL"

## 7.3.23  CHANGELOG.md
* 927df70 prep for delivery
* c749147 avoid using meta.symbol.cobol, as it is not friendly to some themes add config for alias, simplify lang use
* ef3a407 add lowercase cobol, so "alias" is configure the same way as the uppercase one
* 2e6e168 remove experimental_features, as it is not currently used
* 959b45f (from experimental feature)  - drop syntax task, as it does not work as good as I hoped it would
* 3a80f43 restrict commands to the right focus time
* 722fea3 fix enablement of command
* ac106e7 a little tidy up
* 1a774f6 remove unused code
* 12092b6 remove unused flags
* efc4695 package updates

## 7.3.18 comment out step
* 50a1ce9 prep
* cbf09e6 remove docs for depreciated caching
* e5efa67 allow metadata caching to be turned off
* 6d650c5 tidy up
* 0797b89 add "of" support to copybook open
* e3465c6 add support for hp cobol "COPY text-name [OF|IN] library-name"
* 7c820bf add version number
* ada4c83 update deps & tidy
* 9ae9b82 use same terminology
* 726204d make execute/debug submenus
* 054abd2 add debug submenu
* 40a6c44 fix bug in sourceview
* 6c48cc9 do not process anything until all pp are ready
* cc6b77c wire in the global sym file cleanup code
* ff6210c add objects (untested)
* 75fb1c3 continue work with pp
* b432d1c show pp info after the pp has been activated
* 83991c4 inc release number
* 80e658c add some diagnostics for the pp into the output channel on startup
* 2cda4ad fix serialisation issue with bigint
* c42fdce continue background work on workspace caching  - make workspace entries portable
* b6adafc don't need this now
* f8f5fa4 move to using one obj that will eventually contain portable/non-portable filename information
* 8bcd74c continue work on workspace cache
* d4fac48 retry on extension lookup, as it maybe still starting up
* 983da5c Syntax highligh wrong for `WHEN 78 MOVE` Fix #268
* a8681fe move to using bigint stats
* 5852fe2 add in the file cache
* db3dd1e start to add in the filename cache
* 648f259 use a temp area if no cache directory is available
* 5e62e0e start the process of re-introducing the workspace external process cache  - does not work at the moment :-(
* b5f7b7e continue the renaming
* e756e20 rename command to make it clear it is deprecated
* a4565fd move command to cobolplugin.deprecated.processAllFilesInWorkspace
* a27bdde add missing file
* e4e9180 move to new pp activation model
* 1ff9efc add missing method
* ac1a4a3 start to rework the pp to be enabled by a list of extensions, hopefully allow them to be full active at the same time as this ext.
* e5bf5bb handle in version num
* 456c2eb update to include sdk.zip in build
* 92f3316 continue work on pp  - alter start to include packageJson of package registering    so more detailed error handling/information can be generated    later  - add callback interface for extra info used while pp'ing
* 81910bf couple of teaks for the acucobol dialect
* f8285bd flip a couple of things out of experimental status
* 5ed0348 add file-control as a end-del for a remark
* dceae0a optimise use of settings obj
* e9df9bb add version id, for better comparison, use ms for files
* 88fa796 add support for handling a external file system for gotodef (alias of prg-id)
* c346793 remove message & remove unused field
* 3a8ce26 only clear specific parts of the cache
* f6e25eb add clear internal memory cache cmd continue work on pp
* 752b62a add crudge type back in
* 3e75ff4 add more scanner callbacks
* 27f26f3 add types
* ba72cca fix bug in use of config.
* e9257cb add more pp support add cross process support for ep/prg's to scanner
* c45a91d continue tweaking the pp interface
* 141de11 add more of the pp interface
* ae8219c continue work on pp
* 61e61c7 put a crudge process method in place
* 9328c33 start to put a framework in place for a external preprocessor interface
* 798ca9c change name to cob*api*ts
* ae82ce9 start the api experiment
* b64e943 continue breaking out the globals
* 429739d add simple call/cancel targets
* 1fce6b3 start to wrire back in ep data into the provider
* 693d848 load ep data on activation
* 1e6e2fa remvoe all entry-point symbols before parsing program
* 75b8a52 add more of the entrypoint support
* 2e028e9 tidy up code
* a390837 move external sym providers into sep file`
* c98fed4 continue to remove references to the non-file sym files
* 44029b6 wip - control work workspace metadata
* 2cfaffa add symbol load/setup
* 40ed5a3 start to re-wire the symbol use
* d10782b not ready but don't want to loose it
* b250adb continue work moving to workspace move
* e58fab8 pass symbols into scanner
* 1798cbc create metadata area in workspace for global symbols
* 612e736 move to next month
* ac4ebc8 just incase commit
* 6d4ec8a add onEnterRules for call/evaluate remove experimental example from readme, as it is not present

## 7.1.1 paragraphs missing in outline Fix #266   - parsing of comment lines via the scanner internals was changing     the behavour on multi-line scanning

## 7.1.0 introduce cobolit as a seperate language

## 7.0.13  CHANGELOG.md
* ee4154f fix script
* d0c9742 update vscode engine
* 5bd399a update packages, remove html parser as it is not used (was for the failed coboldoc support)
* d36d037 add gen_third_party.sh to the ignore file
* d8e03d1 add third party notices
* e5c4d93 Move away from using uuid, as it seems to cause a clash on Windows, other platforms are okay.

## 7.0.12  CHANGELOG.md
* 0989dd4 add in missing acucobol

## 7.0.11  CHANGELOG.md
* 6eaef5c tidy ignore extra .md
* 138ff44 tidy up build
* f4a73c1 tweak the format
* 14bce46 tweak

## 7.0.10  CHANGELOG.md
* 875bf52 tweak gen-changelog
* 2281745 change log is broken, so remove until I can resolve the issue
* e076f85 continue adding dep marker
* 3d87382 add "delimited by" snippets
* 8594685 add "end program" and fix lower-case program-id
* 6210544 update & bring inline with other uses of MIT license (.Net Core)
* c893b41 current tags and releases missing Fix #258  - include changelog generation
* 8ff07e6 tweak cobol provider
* 026a9d4 be cautious on the unlink
* 5351afe dump metadata to file Fix #254
* e4b54ee tweak message
* 37bf98d avoid warning message is only divs are in header of program
* 9a4bfe6 changes to better handle implicit program-ids fix problem with untrimmed token that do not end with a full stop
* 9eb4b99 Avoid picking up "entry" as a entry point - #138
* f7fa300 add depreciated tags to the settings that are scheduled for replacement or removal
* eaa09eb inc ver
* 920b10e Metadata parsing misses some sections #255  - tweak symbol catcher and event generator
* 302c9be fix typo
* 6624732 more cleanup, remove unused functionality
* 2f1ea99 add info about changing token colours
* 9d1a12c turn off git/release as it will now only be used for private non-public builds

## 7.0.0 prepare for first release of the new year
* d232f5e update readme with spell checker information
* 58129de breakout condition names
* 71e77e4 tidy
* 67383c7 tidy up
* 265cebd remove gnu list file from the file selectors
* c17b526 add simple formatter for camelcase/uppercase on return-code  - under experimential flag
* 07f72ab tidy up use of COBOLUtils, move to statics, as no instance data is required
* 2182656 Remove a missed part of the old coboldoc support
* ab16f47 remove "Open COBOL" reference add note, about contributions & gnucobol extension

## 6.11.11 prep/push
* cfcfb93 continue work on the task provider
* 3ee76f4 make it private
* a5fd855 expand the possible script names
* 1b8e44e add dbcs literals
* 2e7c36d add jnienvptr
* f4d5648 fix compilation issue
* 2deeb4f sort the table to aid readability
* 311608f tweak special registers
* 7954fe0 add in utf-8 literal delimiters
* 9c2c2bf Update stale.yml
* 968c979 add some discovered reserved words from netcobol, as known but illegal
* 00308c7 add more keywords from the iso2002 spec and add a config to ignore the first section if an entyr-point is next, its crudge needs more improvement

## 6.11.9 add cobol-it to the task provider

## 6.11.8 remove warning matcher is task provider for acu

## 6.11.7 remove the last the non-module gnucobol bits, the extension is now split into two.
* 5c1e095 continue the removal of anything GnuCOBOL related, so it can be pushed into its own extension.
* 53fe7d2 packages update & vscode

## 6.11.6 add problem matchers to the default config
* c51fe39 fix outstanding overlooked bug with the keyword provider

## 6.11.5 continue minor tweaks to task provider.. more work to do here... comment out subjective code in keyword provider, as I have send times it causes more harm than good (aka it prevents keywords from being returned
* 5f4072b start to wire up some options for the script provider
* 50337da bring back the bld script task

## 6.11.4 second fix to problem matcher (out of sync message

## 6.11.3 fix problem matcher

## 6.11.2 simplify previous change for catching different category of messages

## 6.11.1 add support Micro Focus ECM error categories: CICS, SQL, XDB, IMS and DB2
* ce067b3 let vscode decide where the problem matcher find files (when we can)
* e78175b use autoDetect on some of the problem matchers
* d628ffd further tweaks
* 2cf01a6 File is too long? Fix #249

## 6.10.24 add extra brackets & if,unstring,string into completion list

## 6.10.23 tweak the brackets support to handle if/end-if and a few more  (more may come, I just want to see who this pans out
* fd2f760 tweak $schema location
* f6eff34 not required
* 51e21bc add schema, as the original one has gone.. so include so I can reference it

## 6.10.22 add some cbltypes as support.function add 78 specific colouriser
* 58371aa add bot

## 6.10.21 tweak casing util and fix some storage items missed
* 8884af7 gnucobol related tidy up
* d26b4b3 tweaks for more field types for bms  - fix unrequired , in package.json
* a232fd7 start to think about web use of the extension  - can't do too much until I get codespace access
* abf4378 add support for "exec java" used in some java based COBOL dialects fix "-" in first column that affected some comment lines
* ef510bc ensure we don't change to a document type that does not exist
* f8fccf8 update changelog
* 9376786 fix links
* b39d122 changes related to master -> main branch name change
* 0c58f20 move to main
