# CHANGELOG

* 1811e0dd add more ILE APIs
* 3183834f ensure folding ranges can be turned on/off on the fly
* 7020540c ensure .cblle / .sqlcblle file extensions to ILECOBOL

## 26.2.27 
* d8a35c5c add some more support for ILECOBOL
* f6894791 add basic folding provider

## 26.2.23 

## 26.2.19 
* ef77eb01 drop 1
* d4af92c2 Update
* d2508605 remove unneeded async/await
* ba47494d fix a little memory leak and a accidential double register

## 26.2.16 
* c10cfc83 try to ensure activate() works as expected

## 26.2.9 
* f0d76db1 update README.md
* 0228f30f add support for user configurable alpha for minimaps

## 26.2.8 

## 26.2.3 
* e85ef00e default, enable minimap boundaries for divs/sections but not paras
* 0dac9c9a add finer grain control for the minimap
* 4b121ea3 ensure minimap can be enabled/disabled on the fly

## 26.1.31 

## 26.1.25 

## 26.1.21 
* a4c82d15 Add enhanced minimap feature

## 26.1.11 

## 26.1.9 
* 245dada0 use global vsce
* a69e18f6 Update
* fb3e1719 allow - in bms file as line cont char
* 688e0666 start to add support for a custom remove reserved word list

## 26.1.7 
* 22383d73 drop coboleditor.codelens_copybook_refresh in favour of copybook_refresh_search #366

## 26.1.6 
* cab4c1b8 Fix typo #366

## 26.1.4 
* 04d27309 limit #366

## 26.1.2 

## 25.12.25 
* a889780e add codelens_copybook_refresh
* 84e47264 add coboleditor.copybook_directory_aliases to allow library names to be changed
* 15b0b2ff rename extraInformation1 to extraInformation
* cbbd1f2d tweak font in preview panel
* e8b0ff0c add simple bms preview
* 93ab705c not happy with timeline, so remove it
* 9803e58e not happy with it
* 184d95ac tweak
* ff7039a6 Add timeline.. need to verify it is 100% okay

## 25.12.22 
* 0051f4dd add network_directory_prefixes
* 316a096e add network directory prefixes and add slow url check to be inline with local/network search
* 351cb167 add copybook_directory_speed_limit configuration
* 1bd783ff add basic rexx support
* b9212a69 tweak rexx
* 2a7072c7 add v0 of rexx support

## 25.12.18 
* ddd86bca Add copybook_refresh_search #366

## 25.12.16 
* 3330d9e5 Update
* 965edd05 tweak eror due to nodejs change

## 25.12.14 
* 94b362f1 Update
* 52a05093 Avoid relying on a catching an exception, return undefine on timestamp check if file not found and drop a file check  #366

## 25.12.5 

## 25.11.20 

## 25.10.27 
* f2c8f3fb Pickup default language setting #365

## 25.10.25 
* f861b3d4 tweak icons, use rocket item for 'Rocket Software' related links
* a7511296 Add support for language based setting #365

## 25.10.18 

## 25.10.11 tweak
* 7423c360 Update

## 25.10.6 

## 25.10.1 
* c65fbd4f tweak

## 25.9.29 
* 5df04f35 tweak name update packages

## 25.9.19 
* d6c50a15 tweak formatting of the README

## 25.9.17 
* 12145e52 turn off copybook missing messages fix webpack

## 25.9.15 
* 541655f9 add vpath support for .d files

## 25.9.14 
* 14342267 add simple copybook searcher in cobscanner

## 25.9.10 
* 128f0343 add config.makefile_dependency_prefix

## 25.9.9 
* 176771e5 fix typo in comment
* 70b40a3c ensure unresolved cobscanner unresolved copybook list is accurate

## 25.9.7 
* 84736a9a add config settings
* e54918e3 add more makedeps features  - start to include it in the scanner processing
* 0864ffe3 Drop unused file Build up copybook map in vscobscanner
* 3d31fab5 split vs part from generic part

## 25.9.3 
* 59fb77b5 add more ile api's descriptions

## 25.9.2 
* 16ad9e37 Add LOVAL,HIVAL,READC to ILECOBOL Add 'Switch to ILECOBOL'
* b0fd75ff add .envrc, so ncu can be found via direnv

## 25.9.1 
* f578aa07 remember unresolved copybooks

## 25.8.31 
* c3699ef0 add an experimential .d creator

## 25.8.27 
* 28a53ee7 ensure $region/$end-region works with upper-case
* d1683f10 fix typo and ensure $ is include in $region for range

## 25.8.20 

## 25.8.19 
* f0cfd8da Update
* 9e81632a Update
* 1ba25045 minor tidy up on hover provider for variable with one line
* 53a92346 keep in-sync with 'Rocket's extensions
* e3322be1 Update
* 7c982ec0 expand range for variables in the outline view
* 9602d917 re-locate code for missing copybook

## 25.8.18 
* 490bc15c Send overall copybook message to logger
* afb01418 make selection area better for non 01 group items
* 86c3e5fd use startCol for COPYBOOK symbols

## 25.8.17 
* ac414650 tweak dump symbols
* e6d7a216 add some copybook recursion checking, so we are not relying on the depth limit checks

## 25.8.13 
* d9b37597 move static depth info to copybook state
* 8695ffa3 add config for copybook depth limit

## 25.8.12 
* e846f855 tweak recursive handling limit on copybooks

## 25.8.11 
* 1e0c387d tweak to handle multiple copybooks with repalcing

## 25.8.6 
* 31e2e514 Ensure rename only works on visible fields/sections/paras

## 25.8.5 
* cb2f4565 Update
* ba78a931 Add found references in copybooks, when text replacement is okay, may need more tweaks #362

## 25.7.28 
* 408360fc Continue work on #362
* bb98f6ec fix regex for langs

## 25.7.25 
* ed45d2d4 Ensure ==xx== is one token #362

## 25.7.21 
* e6b110c8 Update
* ef53b65c add in-compatible formatter

## 25.7.16 
* 68643c0f Add back filenameDirname #361

## 25.7.15 
* a38ac000 check to see if the copybook is resolved to new location #361
* 264b8cee refactor #361

## 25.7.14 
* e4a9e910 Updates for #361
* c6c09b9b Change reference
* 60be46b3 Fix #361 - Use change filenameDirname to fileDirname

## 25.7.13 
* 0e7fa3d4 keep current

## 25.6.30 

## 25.6.14 
* 4f2d9bdb Continue work on #360 - basic search, need to review caching
* fcb9bccc move per file settings into a different list
* 87649e4d Expand copybook interface to include sourcehandler, in prep for #360

## 25.6.1 
* 714ef8b9 update for mac

## 25.5.23 

## 25.5.24 
* 0e0fd008 enough being silly and change name
* 5ba3460a fix publishit
* a1a09d19 Update
* 81545465 add yesterday

## 25.4.30 
* 35d9a008 add extra extensions

## 25.4.27 
* 2a663de0 update readme
* f7984ad6 Expand the ILECOBOL support

## 25.4.26 
* 5a4c525d remove duplicate lines, as these are not necessary for seeing the flow

## 25.4.18 
* 2021a856 Fix #358

## 25.4.16 
* 3dde8d88 update packages

## 25.4.5 
* bb18823b Add some support for identifying sourceformat free Fix #357
* c94fffaf fix a odd range end problem
* 6702fd08 add always_terminal option

## 25.4.3 
* 6c6701ce add always_free option #357
* 65f99464 Bug Report: Outline Not Working When END PROGRAM Starts in First Column (Free-Format COBOL) #357
* 0b80c29f add "processing procedure" to snippets

## 25.3.29 
* a725b837 add hook for "xml parse... processing procedure" to bring up a section/paragraph

## 25.3.27 
* 439cc876 tweak end-xml to be the same as "xml parse"

## 25.3.16 
* 3cdee2f6 add pointer-32

## 25.3.9 

## 25.3.3 
* b82366cd add interface to scanner, to aid testing

## 25.2.25 
* f3d0ecf9 change margin_73_80_comment to margin_identification_area

## 25.2.24 
* 5126c64f Add coboleditor.margin_73_80_comment to allow fixed format files to turn off 73-80 behavour #355
* 200e7d98 start to move fns into interface

## 25.2.12 
* 74fe8b0b fix col1-7 problem with +

## 25.2.6 
* 1c5a59d9 pass window state rather the field state
* a2331972 Introduce some state management in window
* 637f08ee rename to program info panel

## 25.2.1 
* 6e5dec19 fix bug in handling of malformed "pd/entry using..."

## 25.1.25 
* 12d88bd9 tidy up enable of view
* 7221d401 a little messy but its wip

## 25.1.21 
* d61931e9 add vscode-elements.css
* 6f052f68 add some node link support
* eb6ed050 link view graph to preview

## 25.1.18 
* 8d771bd3 only restore 01group level if changed in copybook and restore if not
* 845e8e55 Continue work on embedded copybook support #354

## 25.1.17 
* 9cd930b7 continue work on refactoring inline scanning

## 25.1.16 
* dc60eb00 simplify state use tweak meta symbol
* 096bae2a update link
* 28dcae18 add util ext's that don't work this extension

## 25.1.13 
* 952aa654 preserve 01-group info between embedded includes #354

## 25.1.11 
* 25f98f59 add panzoom
* 0588f98b preserve current 01 level for inline/scanned copybook #354

## 25.1.10 
* ec4b2ec6 add reference to a known variable, ignoring outline #353
* b50e060e add dir/README

## 25.1.9 
* 87a03f56 add pre-copy
* 436be31a add enablement for commands
* f6c116b2 add a variable reference for "to"  - could be related to #353 but no example or real detail given
* 280ab58f tweak
* 303978f4 drop the toolkit (for now)
* ba53af14 start to refactor webview code
* dfb8c88c move to an embedded version of mermaid (fingers crossed)
* a0c8f74c add refresh code
* 73920f40 wip - fix dark/light by using neutral theme (could be better)
* 1a52ed23 continue wip
* 6ae5d847 wip - use webview for mermaid graph  - colours not right
* 2b473658 update for new year
* 6e865358 update LICENSE date
* fb8595e4 tidy up and setup program-id, so we can re-use it

## 24.12.30 
* 3c4de8be interim release
* ae7f19b1 don't save the markdown, just use a untitled document
* 7771b6c1 shorten name of implicit name
* 1df12729 remove csp
* 8189ac3c diag dot file for calls diag command  - wip
* 3c38dd8f change to use the full description and include it in the refs and un-used area
* dff78e66 add top-level section/paragraph
* 1bee4792 ensure section/paragraph are consistently set to undefined

## 24.12.18 
* 451d9fea add description, ensure name & name lower is okay

## 24.12.14 
* c88bcd25 add WIP comment
* 7dc0e579 add simple jit install of called by provider
* eedbdbfa only use registerCallHierarchyProvider when enable_call_hierarchy is set
* 1e99c1a1 add enable_call_hierarchy property
* b9f63da2 add nearest section/method to aid calledby support
* 363238f8 a litte refactor and add first impl of both in/out but still wip
* fef5b2ad tidy up
* 1f603d0c next step in 'called' support changer linter to use common reference counter
* ca55a5ca add wip call heirarchy
* 1fb0fe43 add wip heirarchy provider

## 24.11.28 
* f71d2853 update & enforce program_extensions scope

## 24.11.19 
* 0f4bffd2 tweak comment to work better, re-structure to make it cleaner

## 24.11.14 
* 374ae58f minor tweak
* 24724258 Fix #350
* 58cd00b8 test tweak

## 24.11.11 
* fa3bc1e5 start to simplify the conflicting extensions
* 95ad4e54 add ability to just give a warning rather than disabling the extension on conflict
* d473764f move extension check into common area  - add bloop ai extension as cause problems due to duplicate    intellisense
* d2d8f22e tweak comment processing
* dc675eae continue with the cleanup
* 48d199da remove used property and set the scope of settings that will have to be workspace
* 3d34ea50 start work on linter
* 021dd701 contine work on settings seperation / enable format_on_return
* 05e9bf05 re-introduce url path support
* f6735ee3 continue work on file copydir and settings
* 459b5b66 start to pass down settings
* 1f2533cf split workspace get into two
* c360f2ae move vs related values to vs settings
* 88385c39 rename cobolfolders to vscobolfolders
* 6567e53c add IVSCOBOLSettings and seperate vs settings v out of process settings
* ccc93c5d tidy up
* 63bd0918 refresh all known settings on workspace change
* 627c3ea9 Update COBOL.tmLanguage.json
* 30a5571f move copybookdir handling to vscobolutils
* 06bb426b rename cobolutil to vscobolutils
* 946875c1 drop migrateCopybooksToWorkspace
* 1a610344 add more caching
* 00b4903e copy copybook dirs from non resource settings in prep to adding in resource folder dirs
* d610fdc3 start the process of moving global file search data to settings
* dcb90716 rename .get for settings
* ed10ecdf rename cobolutils to vscobolutils change more .get to .get with textdocument
* 9b5c6a18 untested changes - more refactoring
* 4bb3cf06 start to use document to get resoure config  - may need to introduce a cache but get the basics working first

## 24.10.26 
* a467c9cb use editor.tabstop from resource continue some refectoring
* 0bf3fcdc start to centralise the config handling

## 24.10.22 
* cbd21621 harden method
* dad4d096 substring different to substr
* d83af2e6 fix typo and avoid directories
* e1d04a2c use specific flag for trim, tweak file reader

## 24.10.17 
* 1b5f885c amend url for community

## 24.10.15 
* c2b4b37f fix issue with scanning using with extended micro focus syntax

## 24.10.12 
* 3adeda20 ensure we don't bleed any hidden items into the codelens
* 8a615809 Update
* 2a3170a3 ensure references are only shown if the definition is in the current file

## 24.10.11 

## 24.10.10 
* 42fb0a05 if a var is defined more than 1 time, then do not include any reference count information
* ab97eada prep for tomorrow
* 2dff5757 remove threshold hack and scan source references map (inline with variable) handling
* fc161026 Update
* df730f5e tweak exec token range check

## 24.10.9 
* 8ea22056 tweak exec sql only use source references if have something defined use source range for sql declare

## 24.10.8 
* ab9b2f36 move with the times and drop micro focus if we are not talk about the dialect.. let the Rocketeers rule
* abab92a1 Update
* b19c5d89 gnucobol is no-more

## 24.10.5 
* f55e8315 add message about missing 'ignore ls'

## 24.10.2 
* b0384ffd add ignore ls on/off to semantic scanner, so it can be marked as comment

## 24.10.1 
* 8f4526b1 add ignore section as "null" into the outline view
* e57dbfd2 rename link

## 24.9.30 
* a098a579 enable wip support for selective scanning
* 66efdc2b update readme
* 7396bcb7 fix a bug introduced with the rename variable support
* adb2efa9 continue work source dep filtering

## 24.9.29 
* ce67694a ensure program range includes program-id and last section/para does not include "end program"
* 54228724 update end of range for group item to last end known end of line
* 6681c98f include "01" in a group in the range
* 85b190b6 add range to COBOLToken  - fix odd range issue with source readers

## 24.9.26 
* 213debd5 comment out function until it is used
* 347ee275 Update
* f95dc9b1 add source for simple regex parsing of source-dep file using
* 99b03cb8 fix a stoken handler which may affect semantic token'ing wip - continue work on filesource filtering
* 6b518cc0 add wip, ls control token scanning
* ece6c4ed some defensive coding

## 24.9.24 
* 65069b8b fix two odd bugs  - nx odd length  - multiple items per line split problem
* 178be87a filter out anything that can never be a var

## 24.9.23 
* 78e85020 add typedef to keywords turn on "sql declare" ref handling
* ffad3a07 fix bug with skip token in "pic x." partial statement
* 78eef891 continue work on modernising the reference handling, still more todo
* f3173836 experimental code for add var/reference
* faaaf205 include section/para from source deps in references
* e41e117d add simple navigator for source-dep tag

## 24.9.22 
* c2fec207 avoid using non-viewable tokens with codelens

## 24.9.21 
* fcc7c9f9 add scan_comments_for_references and initially disable it
* 04bc06a0 add boolean, so we know that source dep copybook, can be processed differently

## 24.9.19 
* 5d52e782 Update

## 24.9.18 
* aec5def2 tweak make tokens
* c868fb94 Update publishit.sh

## 24.0.0 
* cfc45a0f revert recent change

## 18.9.7 
* b592baa4 dev: tweak search for references in pd

## 17.9.7 
* ca09b86e more 'rocket' related tweaks
* 7e8f38e2 updat readme
* a154870e tidy up use of "Micro Focus COBOL" when relating to the extension

## 16.9.7 
* ef66daea add support for the Rocket COBOL Extension Fix #341

## 10.9.7 
* f521c868 add sql from monaco sources, tweak to make it openesql labeled
* 57e8ef91 move "exec sql" into a custom openesql syntax, in-due of the changes coming from microsoft reguards changing sql to mssql

## 10.8.31 
* 012dc182 tidy up
* 0e0812ca add a rename provider to exec sql cursors
* 8ea0a4f2 use sourceHandler from token
* 0ae1998b add config for turning off sql declare parsing

## 10.8.29 

## 10.8.28 
* 431b174b add a simple hover provider for sql declare items
* de300cca fix "sql include"
* 2cb3d80d Update
* e05c48fb adjust source reference to more exact with the "exec sql" block #340
* fa79f5f7 refine the source refs, move the source ref list to the sql declare object, this should help later
* a26b24b0 try to restrict sql cursor look up to "exec statements"
* aeba4c6c add goto def for sql declare
* 034857b5 split SourceReference into two, one length base and the other extact (multi-line).
* 56e00b54 start to add some sqldeclare references info to the ref provider

## 10.8.25 
* 98a012c2 start to add in a simple "exec sql declare" scanner
* a2c8c7f9 tidy up and a little rename
* 0fb6c5f6 allow the symbol description of a "exec xxx bb" to be split over two lines ensure start of exec, it as the start of the verb exec
* fbab60fd Update line information, capture end line number in tokens #340
* 95c852ff Add support for remembering exec tokens in order - part of #340
* c561b2e3 add some support for common acu api to the hover support  - allow $ in words
* 31f37d75 index call target with document lang id  (more work required in this area)
* ceeaaf83 remove dud entries

## 10.8.14 
* 383afebc add markdown dividers between items better handle duplicates variables

## 10.8.13 
* 826fe988 add multi-line comments (upto 5 lines, may change or make configurable)

## 10.8.12 
* bb537c7a change colour comment tags to have a space prefix

## 10.8.9 
* 5396d2c5 add hover support for para's & sections with comments

## 10.8.6 
* 0d0e4724 enable coloured comments don't complain about source files that contain few lines mark BroadcomMFD.cobol-language-support as an extension that performs the same features as this but can clash

## 10.8.4 
* 74498e22 dev: only use "*>" comment if on the same line
* 48492d1b add some experimental code that picks up comments for variables

## 10.8.3 
* e188f369 prep
* d620eea5 add missing region/end-region, as it makes my life easier
* b879cf47 fix $end
* 48cf85b9 tweak settings for language detection
* 23dc465c add 'New File'/Empty file and order entries
* 21737c67 Fix #337
* 58445d1b Fix #334

## 10.7.23 

## 10.7.22 
* 117b90f3 tweak perform snippets

## 10.7.9 

## 10.7.6 
* c17303d7 move up glob version
* 01a388ec Update
* 01915840 update versions

## 10.7.1 
* b86e5ef9 tweak feedback links

## 10.6.28 

## 10.6.17 
* 0036b8d7 keep README.md in-sync with package.json
* 7a545b4f enable rmcobol/bitlang-cobol when using github co-pilot
* 9e237607 Minor tweak #334

## 10.6.16 
* f25c6cbc parial docs update #334

## 10.6.15 
* f6cfed0d add info about lang server setting to console
* 3f87e49f remove unused
* 636316be remove enable_language_switcher it's impl is confused
* 061ac7fd Fix 'just introduced bug' #334
* 3a1befde start to handle suppressFileAssociationsPrompt
* 0eae7705 use global if ws not present #334
* e7ad5b05 Need to change active documents on previous lang id to the new one #334
* 7cafc88e Add support for updating non-workspace flags #334
* 44feec9f add extra config option to server control exec & checking is PLI lsp

## 10.6.11 
* 7f0c9f7c fix bug in the setting updates for lsp
* 46687b69 Add setting to enable/disable 'Micro Focus LSP' enable/disable behaviour  - part of #334

## 10.6.10 
* f0e9ace9 ensure 'flip' command is present everywhere

## 10.6.9 
* 17046b70 add fix for #334

## 10.5.29 
* 23431c2b add sql copybook file to references list - Fix #333

## 10.5.28 
* 9ab859bd add temp "windows/mac" exclusion
* 901ff08c BMS: Missing items Fix #332
* 7fd0f93f slight refactor
* d1580501 add some early support for my 'dotfiles' COBOL terminal
* b4e50f62 remove warning
* afc5e5e6 add get_dotfiles to build process
* 9c4ac21d remove eslint

## 10.4.11 
* e23b4cda add comments (COBOL and SQL), into exec sql/ado
* bc83f517 move vsce dep

## 10.4.9 

## 10.3.25 

## 10.3.24 
* 9b6da430 Update

## 10.3.10 
* 85331d4b dev: add $region/$end-region into symbol provider (helps with nav)
* 4a754a21 tweak

## 10.2.21 

## 10.2.5 
* c9b113ca add a snippet

## 10.1.23 

## 10.1.15 
* c9a54630 Merge branch 'main' of github.com:spgennard/vscode_cobol
* a01a70bf tweak "exec dli"
* b409c897 Update vsdirectivesconv.ts
* 290ede2f Update cobolsourcescanner.ts
* 423dec36 save away table

## 10.1.12 
* a362766d COBOL extension supplements pre-parse check
* 8c6771e1 standard is okay

## 10.1.1 
* b4f505f9 add config for the portation scanner
* b7a8a8cf add "function sub" lint message
* 43e4d5e4 use cached linter severity in a consistent manor

## 9.12.28 
* dd7bf2bf add >>elif to the non-portable bit
* ad81afaf only enable directives convertor for COBOL/BITLANG-COBOL

## 9.12.25 
* b2fa865d change sub to warning
* 41c740ae add source format directives changer
* 39afa8f4 fill into the other directives
* 2595568e add initial directives changer

## 9.12.21 
* 52fe2b1e add better copybook defaults
* 00f8e42e add java-sharable on/off
* f06c6efc add java interop reserved exception register

## 9.12.14 
* 02d7923a add java-callable

## 9.11.22 
* 10dd331c Update
* 4477ca3e change defaults for the linter for acucobol/dir

## 9.11.21 
* 0d3a19c3 Fix #331 - coboleditor.linter was not being honoured

## 9.11.10 
* 9e553889 add support for old cobol-lint tags
* 4cc7b858 add xml snippet
* 89c94f54 add more snippets

## 9.11.9 
* 76d2aed7 tweak

## 9.11.8 
* 23bae969 add $COBCPY if present and add some knowledge about known sys copybooks
* 54872089 tweak text
* 262c8335 add the initial version of the copybook path fixer
* c0ba860c continue work on copybook finder
* f9662f28 tweak name
* 68caf30d typo
* bccc3d10 start the process of de-emphasising any related to net express/server express
* 68c6f89c add config
* 1c4ef742 add setting to enable tabstop anchors
* 91f9edea introduce interface for the anchor tabstops
* 3f5f5c22 expand anchor tab interface
* 1332a2ef tweak tab handling avoid a slow-async startup exception (can occur when debugging ext)
* e50c0af5 add tabstop out of range configuration  - continue work on fixing odd edge-cases with tab/untab
* 0dd77b0f start to refactor tab/untab
* b0496ce8 update tests
* 40443369 add extra docomment snippets

## 9.10.16 
* a4466959 allow both the formatters to work together

## 9.10.15 
* 1a8941ad tidy

## 9.10.12 
* 3beb7b0a Update
* 1f08be26 now we have more tokens, only include readonly ones
* 30375d47 move
* c7720072 add
* ea20a4b8 start to add conv

## 9.10.4 
* 929e4559 update docs given devcontainer.json spec has changed
* 3496e638 cleanup
* a710d6d4 update versions

## 9.10.1 
* 04db34b1 cutdown on the verbose messages

## 9.9.28 
* cd64c5cb simplify
* fac52d14 add ref provider

## 9.9.27 
* 2b9b1448 tidy up the extension info api

## 9.9.26 
* 0cf45c24 github.dev returns this extension... so need to add it to he blessed list

## 9.9.21 

## 9.9.19 
* 4e9dc1d1 add support for "with debugging"

## 9.9.16 
* 8bded749 add simple search for subdir's directives.mf files  (might remove, as I can't get navigation to the file to work)

## 9.8.31 
* e7304845 add symbol provider for directives.mf files

## 9.8.30 
* 38c4a433 add barebones directivesmf support
* 0391a932 update packages change timer to timeout
* 416b1052 Update
* c8775da1 Simplify the README
* 34e02e3d tweak
* 14fdd58c tweak

## 9.7.23 
* 5a9f67d8 remove length of hover line for fixed format programs and remove excess spaces
* 354799f3 move hover provider into a seperate file

## 9.7.20 
* 88ef43e3 add MF 9.0 platform constants and extra directives

## 9.7.19 
* af4eff83 add a change log to inform users of the latest 9.0 release

## 9.7.15 
* 9e002f79 last set of changes
* 453c0002 Update
* c593bf10 more tweaks to gen changelog script
* c9b654e3 Update
* ca88fe9e only apply margin if enabled
* e476e8d4 Update
* 5d318d7d add some diag information and add default for anyone using this extension with the Micro Focus TM COBOL extension
* 33ead978 tweak

## 9.7.14 mac osx/broke the changelog creation
* fa2a0017 tweak for co-pilot

## 9.7.10 re-work for mac (lack of -d on xargs
* d99fcad0 add header
* 0624a4ef tweak $if

## 9.7.1 
* 8e59b79d add space
* d76817e9 Add "TM"

## 9.6.25 
* 454ab273 make switcher configurable and off, until completed
* 936c3f2e add switcher support

## 9.6.20 

## 9.6.19 
* 30772119 add threshold for display'ing references and enable

## 9.6.18 
* 5d1113d3 add snippets

## 9.6.16 
* a4f85113 move iso snippets to programable snippets

## 9.6.15 
* dd776f06 Update
* ad8aedb1 don't activate default variable provider if $ is in use tweak snippets
* 2be095ed Merge pull request #328 from spgennard/spgennard-patch-1
* fee21a87 Update README.md

## 9.6.13 
* 6ef85bac move $ snippets into snippet provider to try avoid some odd behavour
* 88380f4b tweaks for extra dialect (for remarks)
* 9f3fee14 remove duplicate which crept in with a regex/swap

## 9.6.10 
* 5b218353  Code coloring issue with REMARKS keyword in variable Fix #327

## 9.6.6 
* 54cb4772 dev: avoid "" command message error when double clicking in int's etc in source view
* cbca9b75 update version

## 9.5.6 
* 78ecdfb3 refactor simple copybook drag/drop provider
* a96c24df add a simple copybook provider
* 3613a08b add ideas file
* f57326eb tweak use time from file

## 9.4.29 
* 50996b32 tweak intellisense space handling add limit to overlay view

## 9.4.25 
* 380edf8c Update
* e4135efb add snippet
* 18711bf7 updaste

## 9.4.20 
* 8e331378 Update package.json

## 9.4.19 
* 7ee0fd53 remove close left menu

## 9.4.12 
* dae60f48 update to use later glob package
* 4a799477 tidy up
* cbcfcac5 update (without glob

## 9.3.19 
* f2ea18ba add simple support for "Close left tab" context menu

## 9.3.2 
* 7900c38b update (keep glob with a lower v num)
* a67d774b tweak
* ba08678c Update
* 293be78e tidy up package.json in prep for new release

## 9.2.23 
* 9de6b95d expand to all COBOL dialects

## 9.2.20 
* 01cd9900 add lang status bar

## 9.2.15 
* b2d29122 Merge branch 'main' of github.com:spgennard/vscode_cobol
* c33bbace move away from minimatch
* c0d61fe0 tweak matchers

## 9.1.28 
* 52317350 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* 4a37f853 tidy
* 95c91527 add some diag routines (temp)

## 9.1.15 
* b7aba963 add "exit paragraph"

## 9.1.11 
* 98d9c5c6 Update
* babb2dc6 new year... new major v
* b222f1a3 update cics json and remove extra double redirection of $1 (which does not make any difference)

## 8.12.20 
* 6ee99f69 add some really simple "exec dli" support

## 8.12.11 

## 8.12.2 

## 8.12.02 
* d23458fe remove quotes when searching for copybooks

## 8.11.26 
* 3b7302a3 add known variables as references when in a "exec" block
* f1ca4c39 identify each "exec xx yy" in the outline view

## 8.11.25 
* 34986a46 Fix typo #323

## 8.11.20 
* ac7bb3bd add 'exec's to the symbol providers
* cfc717a4 don't trim literal unless we need to

## 8.11.10 
* ea4d0f61 simplify the editor file nesting

## 8.10.28 
* 301547db cleanup
* 53c49c9f remove definitions from references provider
* 723be087 tweak for linting
* 6cd4d502 tidy up and ensure we pickup unused refs
* 872b201f add a couple of config flags for the experimential codelens
* 752337e9 explore use of codelens

## 8.10.15 
* ef129b66 add simple support for picking fields from the comms section
* 7db7976f move splittoken into sep file and tweak it
* 289d50c4 update versions

## 8.10.8 
* 1a43f71c Update
* 5ba74810 minor tweak to include MFUPD files
* 1d065e07 simplify source view

## 8.10.6 
* f3de30c2 tweak to pickup missed ref-mod items note to self - time to look at this closer
* 4c430c93 merge two classes and simplify

## 8.10.4 
* e1a03d10 handle duplicates better in flattened source view

## 8.10.3 
* 9e32dbbd add markdown/pli injection
* e8716e3b add markdown/hlasm
* 15273218 add markdown/jcl injection
* 46498d07 move json files to a markdown subdir

## 8.10.2 
* f3479153 move to a full range rather than a location (fixes odd empty hover)
* e417be43 scan inside brackets and pickup 78 items eg: pic x(MY-78-ITEM)
* c14ce693 add embedded markdown

## 8.9.26 
* b14241aa allow filestrategy to overide margin

## 8.9.21 
* 0a990315 remove duplicates

## 8.9.20 
* c8c2f587 dev: fix bad url

## 8.9.19 
* faf70ea2 ensure we pickup all file.assoc
* 4b6d4080 tweak
* 12c93618 bring back the old margin config option (like the jcl one)
* 3bb8d60c handle rename and drop field not used
* 0a440b3b remove unsed param rename file to be inline with others (gradual removal of this debt)

## 8.9.12 
* 286ed0a0 fixes for sourcetreeview

## 8.9.11 
* 2ae1e1b8 tweak sourceview
* 3571eb1b rename
* 96a16f5a add rec for linter

## 8.9.9 
* e48d428c allow intellisense style to be specific for user commands
* 8e7d0150 start to add support for copybook open

## 8.9.3 
* eab2d9a7 Update
* 73decba7 use uri and avoid Uri.file
* f2021a1e tweak rename to use embedded urls

## 8.9.2 
* 923587d8 use known uri and avoid hardcode uri.file, should make find all references work better with non 'file' based schemas

## 8.9.1 
* fc118143 add some extra schemas/fs's
* f33bde6f add support for "copy in" with urls

## 8.8.31 
* 953ec289 add first part of open copybook via a non-file based URI
* c0d6178a continue the work on URL based copybook finder
* c51e7771 add simple search for base copybook url directories  (not used)
* e300ccd6 add some more verbose messages
* 94fc49b1 start to prepare a place to put the non-file urls for the copybook search path
* 5e4f65b7 ensure JCL is registered to all the known schemas
* e807f50f add 'ftp' schema

## 8.8.25 
* cd086227 remove @'s as it did not work
* ff2fb628 tweak to allow search to copybook to work without parsing its content
* ded58aac prepare for next release
* 6ce2e1b1 merge format on return with intellisense code  - breaking property change, format_on_return is now a boolean
* daef7ca0 start to add support for custom intellisense rules
* 2f380e56 start to add support for configurable keywords snippets
* a133b4a5 tweak docs

## 8.8.14 
* d21bb4b5 allow enforce file associations to be used with ACUCOBOL & COBOL

## 8.8.13 
* 0deb921c add newFile for ACUCOBOL
* c6d89f07 add new unit test

## 8.8.11 
* db3d593a add extra keywords
* b737268a add support for change from COBOL to ACUCOBOL if AcuBench messages found in first two lines of the code

## 8.8.10 
* a052a98d move lks to ACUCOBOL
* 4c95a86e Update
* 693fe9c7 Update

## 8.8.8 
* 8e66ba4f refactor newFile to make it easier to introduce extra ones..
* 917cab17 Add mfunit ep snippet

## 8.8.2 
* a9a43e58 add trim
* a0b122ce add hex-to-char
* 4add7ae6 add function hex-of:

## 8.7.25 
* 1e97718a minor updat
* bd82ee53 add missing exhibit from keyword list

## 8.7.15 
* 11402391 tweak
* 1023a4af make cics macros look better

## 8.7.14 
* db4a0e81 Update
* 720958f0 validate filename

## 8.7.13 
* dfce0329 prompt for filename
* ddd1a026 tweak newfile

## 8.7.12 
* 3422214d add new file
* ab379073 add b"0" & b"1" support

## 8.6.21 
* 0553a48f add more feedback items

## 8.6.18 
* 0d55a909 fill in copybook name if recursive metadata is set

## 8.6.13 
* 85c2ce1f Update
* 92330a22 Update
* 4b2bf619 Update
* cf6682ee tweak

## 8.6.10 

## 8.6.1 
* a7541084 tweak feedbacktree
* cb58f864 add feedback tree and tweak config
* 4df6a4d0 Update
* d2fa0468 add "lst" file to documents on sourceview
* 84a1e7a9 drop sdk ignore
* 73167aa7 minor package updates

## 8.5.26 
* 66c84b00 add uppercase ones
* 9aa65599 Merge branch 'main' of github.com:spgennard/vscode_cobol
* 9756ec36 add uppercase variant for internal cpybook extensions
* 4e093bc2 fix typo
* b8b9dc61 Add extensions: cblle, sqlcblle and cblcpy #316  nb: .cbl is already present
* 79e96556 drop module
* 028298af more tweaks for utf8

## 8.5.10 
* cbe2ffa2 add some pic u support

## 8.5.9 
* addd5cb9 Update
* 13f41da8 add .so/.dll support to the sourceview

## 8.5.8 
* 2243d9b7 start to add file explorer context items for .int
* 09c69bd8 refactor debug commands to utils

## 8.5.2 
* 039f0c72 add support for file nesting

## 8.4.26 
* c61f4755 if source scanner is disable, turn off other features as well

## 8.4.25 
* dfb2ee92 add some basic debugger integration (for .int/.gnt) into the source tree

## 8.4.24 
* 88cf0316 allow lowercase hex in hover's
* d5ab1fb5 experiment with generating .ts.d files
* 771309eb increase strict'ness

## 8.4.23 
* 2e383f40 add >>evaluate
* 19dfb76b start to add some iso2002 conditional expression snippets

## 8.4.20 
* 644c3cda add some conditional compilation snippets

## 8.4.17 
* d3395cea continue support for nx utils
* 70dc0ded change substr to substring
* 2ed8785e start to add nx support add some extra lint's

## 8.4.13 
* 73781cdd simplify jcl keywords

## 8.4.11 
* 08c69207 refactor previous change to make is more generic
* 82fb6320 don't include a space with the keyword "section"
* 7b55c1b9 tweak for DFHRESP
* c61085ed start to remove Null object in preference to using undefined

## 8.4.2 
* cc173272 second april release
* c435a070 apply tweaks
* 1a6a5bda rename token.nextSTokenPlusOneOrBlank and param index
* ccc623f4 refine README

## 8.3.30 
* 7da1ca09 include prop to allow file format setting check to be done early or late
* 1779b9cb tweak fileformatStrategy and make it an enum
* 46985ab4 allow file format settings to override sourceformat
* ebf88ddf remove "exec" on label
* 3e45ee7e add check for "sql" exec for sql include

## 8.3.29 
* bb9a4a42 add some basic scanning for "exec sql include"

## 8.3.27 
* 1260dfb7 add config for hover support (hex)
* 6b4b312c add menu for text conversion tools

## 8.3.26 
* d0ad3aae add a simple hex to ascii hover and fix a case search for a api snippet
* 0099705f add config to allow 'tab' behavour to turn off (if required)
* 3cd7efca add support for using tab with inline snippets (github copilot)
* e09658d0 preserve case on copybook stash away type "exec", maybe useful later
* 73590958 add utils for selection to hex & back to ascii

## 8.3.25 
* f1d5e9ba only reset decls if enabled

## 8.3.24 
* 31abde2c Remove margin.color support, as it not working as expected #315
* 40172a10 move "common" command to seperate file
* ec7b8af3 start to simplify it
* 10a0c70e refactor
* d72b6c4c add missed param
* 9bee4f04 drop another .get()
* eba9a5dc remove a VSConfig.get()
* 394d6ec7 fix inclusion of bad copybook in the outline view when replacing is used
* 326dcee0 replace dep methods
* 45ec3094 add two more command to web variant
* d45115d0 replace .substr with .substring
* 0e56f496 refactor
* 8a83a63a update keywords

## 8.3.21 
* 42a14b2e move external configs inline and protect against too early use of external when isDirectory is used via a config (to review)

## 8.3.20 
* e14edcba fix typo
* a2a06f61 partial sync

## 8.3.19 
* c5c9355e fix typo
* bb84c9ba Update
* c1d138d2 start to make the prefix'es more consistent
* 8ee2a350 add section/paragraph rename
* 47609699 add support for renaming a symbol  - still more todo
* 8f49eb03 update versions

## 8.3.16 
* 8d4f103e add padto72
* ba3825f7 add extra ext exclude
* 5241b2bc add support for making the margin colour configurable
* f83bcf9a break the margin support into easier methods
* e50c68de add config to enable column tags
* 152d79be add right margin tag support
* 9aec9e9a add initial support for left margin tags
* 7a0d91b3 add v0 of margin colouriser

## 8.3.13 
* b68f3175 tweak
* 95ca364d empty package area out
* 6cff80fb refactor part of comment support, so tags can be comsumed in the margin part
* 2626cb25 wire in more events
* d2493edb try using collapsed sections
* 4a231cd6 make embedded tab inline & margin's more function on the first margin area
* 66b17d11 update versions

## 8.3.12 
* ede4a06e tweak tab usage
* 1068a2a8 remove warning
* 0d832d2b be "more" specific

## 8.3.11 
* 075a3b2e Update
* 5448f4b1 use different url for the information

## 8.3.10 
* 9bcdc37e tweak README.. more todo..
* dbc9b138 add comment
* b5751836 remove odd looking matrix
* d8b014ed x 400
* 93defae6 x400
* c4e68884 add 200x

## 8.3.8 
* 76bab7db tweak enforce extensions
* 740f62dd continue work on files.assocations
* 7d9fd6de fix over active colourisation if "test case" is used

## 8.3.7 
* a8392727 tweak keywords to include intellisense if snippets exists
* 15d928bc fix type and update skeleton
* f49888dc remove diag and start work on file.associations code
* 467c23f5 add clear to ensure maps dont expand on re-init

## 8.3.6 
* 979ced60 continue with the function support
* 94a2ec6d move function's to snippet provider
* f82d3c6a start to refactor for "function" snippets

## 8.3.5 
* c6fe3b71 fix bug in hover
* c9185fc0 continue to simplify
* 23278877 move display to dynamic snippets
* ed876ce6 update engine
* 41c59a37 convert more snippets
* afe7f87a update versions

## 8.3.3 
* dbea506d continue to move more snippets to the snippet provider
* b2148b67 dev: allow debuggers to get breakpoints

## 8.3.2 
* e62f47ce move a couple more to the snippet provider
* 2180e605 break snippet provider in two, allow for multiple of same keyword
* 4c41c7af start to add support for extended keyword snippets

## 8.2.27 
* 7f018fb9 add untested cobsql_dir (for future consideration)
* 7a1f1038 add initial version of lang file for mfupp preprocessor mfupp.dir file
* 0cc295e2 add lowercase support for format on return.
* 17a40636 centralise intellisense style into one enum

## 8.2.21 
* a27e378c Update
* 3af6e2fe fix
* 524556cc Update
* f4634b39 update changelog
* a4f5756e ensure map is reloaded if format_on_return has changed
* e9059d65 reduce complexity of snippetprovider
* d13b119f Update LICENSE date
* 04b57f99 complete the support for 'folding' snippets
* 8ca9a8cf add support for changing case of setting
* cd408cc6 sync extension to web.extension

## 8.2.19 add more help with conflicts
* 3ed97bac use default lang
* 994d645f move 'bitlang.cobol' to extensionDefaults
* 2a6f4b27 sync the two variants of extension
* a7e3e1ca start to tighten up checking of debugger extensions that do not provide support for the dialects provided by this extension.
* f757ab04 fix typo
* fdc7fa79 centralise 'coboleditor' and default lang
* 87f74930 update for minmatch versions

## 8.2.12 change "full" to "long" in enum for snippet style
* cacd3371 update packages, flip to new test package for vscode/electron

## 8.2.11 enable short hover and change from a boolean enabler to a enum

## 8.2.10 include decls in the example for the snippets

## 8.2.9 fix snippet config and showing of the example
* 5ff2dc6b add prop for snippets activation
* 9351d885 add schema/format for color entries
* 315ee87a add a bit more support for dynamic snippets
* 36bb5b44 save param info
* df48e966 add -const's, allow quotes to be replaced
* f9a5196d redo mf-cbl_apis after 'any' param fix
* 1b7b59f7 start to fix problem with "any" paramter
* 1e0688fc add some static/defined prototypes
* 340bca07 move dynamic snippets into api lists
* b3989f48 refactor
* 8d5ad29e add v0 of the dynamic snippet provider
* 2376ad16 continue work on dynamic snippets
* 300912dd remove snippet
* 85e62e02 remove item
* 3466a0eb wip
* cf2425df quick prop rename
* 86e05ac8 move from the loose json impl to a class/interface
* 50acef9c add mfunit api to list of known apis
* c5087f93 add some diag code (which will be removed before publish)
* 4801f9f1 add more description messages
* 3616897f quick rename
* 5f7a81d3 tidy up
* afceb710 tweak attrs on colour
* 3adbe059 continue to build out the coloured comment support
* 27317abb add comment word tag
* f4aed2bc tidy up
* 7fcf73a6 add config for coloured comments
* 75a89615 start to add comment support, that works will fixed, modern and acu style comments.
* 8484e2f4 remove anything todo with coboldoc
* 1af35676 update description
* 36593a7e remove 'old' coboldoc snippets and useless 'ruler'

## 8.1.25 add coboleditor.hover_show_known_api property to enable known api hover support.
* 541628ce start to reenable the API hover support #313
* 19cae442 continue work on comment handling
* cfb0606d make on 'events' to be full async
* 402cb4f3 expand callback interface
* 1d37a007 start to setup support for multiple comment callback handlers
* 87de7002 tweak
* f7ade4e1 apply a small opt that should help scanning performance
* e5907096 fix typo
* f73603d7 add some crude "of" support

## 8.1.24 add member to know schemas
* 3b7ab372 reload window if outline view opt has changed
* 980e8c53 add streamfile
* cb1a52cc fix bug, where fixed comments would not work with hint comments
* a3095cb8 continue the tidy up
* 7df8278b tidy up comment handler
* fa079f64 remove coboldoc support, it never processed after raising the initial issues, sad but that is opensource software... often started.. mostly never completed
* 8c329ffb add pointer
* 36db9b48 tidy up

## 8.1.18 add couple of extra "align" split items
* 018b6958 update packages

## 8.1.17 continue work on align storage items
* 8365aeaf add submenus
* ebe98c80 add align center
* 96043d4e sync extension
* 616fbe5e drop it
* c5c9d9a6 tweak 'program-id'

## 8.1.15 add "align storage wide" support
* c54b8dfe tweak
* fecee918 drop document symbol provider
* 9ec23ae5 simplify sourceformat get
* 30e00b83 remove VS from class, as it is missleading
* 559b3a92 introduce lite source reader
* 45bce3f5 substr to substring flip
* 2498e1e4 flip substr/substring
* c3d8e096 flip substr
* 27e4a756 change substr to substring
* 8986ba31 move function
* 9a92ef54 revert
* ae76574d flip to using a source handler (next, source format)
* bcde5297 simplify ctor
* 87958cc7 flip substr to substring
* 0a831be3 improve align data items
* f9527606 drop boolean and just do it

## 8.1.12 continue to refine the align storage items
* 23d57f6c add cblproj as xml

## 8.1.11 merge branch 'main' of github.com:spgennard/vscode_cobol
* b3be2f58 add "align storage items"

## 8.1.10 Merge branch 'main' of github.com:spgennard/vscode_cobol
* d2600865 tweak README
* bc1aa14b tweak cond/set syntax

## 8.1.8 add snippets for bitlang-cobol
* 74199010 breakout cobolit snippets
* 71578c46 break cobol snippets out, so the acucobol one can evolve

## 8.1.2 prep
* d0c0752f add a missed one
* 738ff13e add some openesql types
* 924b1bab add 'sql' type
* 78980957 tweak $set items remove space with terminal format check

## 8.1.1 fix outstanding bug with tab/detab activation on ACUCOBOL/COBOLIT langs
* f0d21978 put a bit more colour into the pp syntax/simplify
* 768cbc65 version updates
* c44ef4fe don't need this in git
* ea3894df fix some problems with regex's in bms and add color
* 354098f0 tweak bms to fix some obvious errors
* 21e579ba add support for bmsmap files  - selected automatically when .map extension is used and it looks    like a listing file and has a bms definition in the first 10 lines
* 80a544f0 upd

## 7.12.20 add support for suggest_variables_when_context_unknown
* ef352842 ensure exec blocks are handled like other verbs
* b4795016 break numbers out of picture clauses
* 841835cc some tweaks for "exec cics" start to move some duplicated items to the repo (for use with exec' blocks)
* c31222ef tweak for cobol-it
* 3fb6cbef add COBOL-IT keywords list

## 7.12.16  COBOL-IT functions

## 7.12.14 update engine
* 1bbb045e keep cobol-it keywords insync fix changes found by later typescript compiler

## 7.12.12 tweak settings
* 8e15dd30 add in-memory cache prop
* 2619b94f extend diag information
* c3688263 tweaks
* a5b2d385 make prop consistent
* 2fc81c4d remove dep method use add line length prop (WIP)
* 650442c0 fix use of editor_maxTokenizationLineLength, start to include settings for file exclusions
* 35b3566e start to setup some core for aborting a source scan early
* 8bd3987a tweaks
* acfff9ab add more functionality, hopefully avoiding everything depend on fs
* 80ecd455 add info message
* b865cf45 change scope
* 0f245e8b move function to class
* 893a417a move two function into util class
* f4fc5159 refactor VSSourceFormat
* 39630816 refactor VSWorkspaceFolders
* 671991cc refactor margindec into class add margin support to web ext

## 7.12.4 tweak
* 0e4ddecf remove debug code
* 20bab315 remove reference to dep'ed vsix extension and reconsiders it use

## 7.12.3 remove line
* 393a3bdd add more schema's in

## 7.12.2 add diag code
* 0d8c013d Update readme
* 64adc112 Tweaks to fix #308
* 94673609 remove use of path.normalise
* ce7699a1 put try/catch in place (to remove at somepoint)
* 2882f796 add external browser
* b16ecc54 add in untitled schema
* 0118ad68 move to simplier changelog, as the java version crashes too much

## 7.12.0 tweak and prep
* 8fb6320d continue tidyup
* d02d3226 continue tidyup
* bcebf2ff remove some td
* 1936afff cleanup
* 97ae287a remove manual build of cobscanner add missing symbol during scan
* c9c7e31e Fix a problem with editor.bracketPairColorization.enabled Fix duplicates in keywords list Simplify cobol provider
* c79dd712 tweak margin handling
* 8469bbb1 remove sdk, as the pp was not completed or a complete solution
* 1be8f0dc continue to remove pp
* 8a23b162 remove unsed tables
* 02bf054b exports tidy up
* 37ecfbfd update acu_cobol-warning-ccbl and add testcase
* c09b94b2 add testcase & tweak acucobol-ccbl
* 6ee722b3 move isFile to features
* c3b83f42 tighten code
* 92e20350 continue the sunday afternoon tidy up
* a2b1aac1 remove maintain_metadata_cache_single_folder, as it has not real purpose other than to disable caching and we have property for that
* 1e99a3d3 remove the ambigous coboleditor.process_metadata_cache_on_start property
* 5b855583 continue to remove cache directory handling code
* b5c577b0 reorder
* 7482cfcb ensure a test is present for msbuild pm
* c1d6ee07 re-enable some old tests that had been turned off
* 1fc2a8c0 tweak ctrl+/
* b23539a3 Merge branch 'main' of github.com:spgennard/vscode_cobol
* 0c69946f tidy up deps
* 973a5298 remove old-cache options
* 6f97f85f remove cache dir
* a0f9d11f remove dep'ed cobscanner
* 3d7b1ca9 start to remove depreciated features
* 0d02c7be Update stale.yml

## 7.11.1 add info about review
* ed598149 change work to be more forceful with reguards dep to be removed features
* 51259f2d tidy up import/drop "export defaults"
* f1b4e192 tidy up

## 7.10.30 remove empty impl, tidy up fs usage again
* eea72b4a tweak $set for ilusing
* c1bc020a flip quotes
* 263f9403 move vsextutils into sep file
* 5541d581 add document symbol provider
* c8c636eb move alway from browsify-fs
* 3802dc79 contine work on moving 'fs' use
* c58aec44 apply rename to export & add more polyfills
* 174c3f2b remove one use of fs
* 4f7cd351 start to move 'fs' related items into isolated area
* 478ac162 start to seperate out use of 'fs' move dep into sep classes
* 11380104 rename miss-leading method avoid using 'fs' in vs settings wire in tab/untab into web extension

## 7.10.21 missed one

## 7.10.20 add some experimental support for web extension
* 31b70122 disable "editor.bracketPairColorization.enabled" as it causes odd colourisation with string & unstring tokens
* 182d5c6e tidy up
* 20d16c21 enable editor.bracketPairColorization.enabled on a per language basis
* 9204f81a Fix bracket issue with ref mod items

## 7.10.15 add some docs for xedit'ish keys

## 7.10.14 add transpose
* d434d01a fix bug in "'perform" isense

## 7.10.13 add some simple keymaps for xedit enabled via config
* 2bec1121 editor.renderIndentGuides has been removed in 1.61
* 876a46d7 fix alignment in README.md table

## 7.10.9 tweak
* 62e8bbdf change adjust cursor to ctrl+alt+a and add adjust left margin ctrl+alt+l

## 7.10.8 Release 'indent' to cursor - ctrl+alt+i

## 7.10.4 add BITLANG-COBOL id

## 7.10.2 remove reference to switcher
* 89ceb8d1 new month
* 795ca4d0 remove possible security issue with npm package (nth-check)

## 7.9.21 tweak acu syntax
* 78dd5750 tidy up/fix bug
* 3d8631e6 update acucobol keywords
* ee734a08 just-incase code to ensure we don't get duplicates in the lists
* 3aa8f797 update syntax
* e5b4dc43 start to make keywords language dialect aware
* 5a9f036b simplify changelog

## 7.9.15 update changelog
* b65a5517 remove use of prefer_gnucobol_syntax
* 656379e1 update to next vscode engine

## 7.9.8 prep
* eaa8529d handle single line '/' - Fix #297
* 817ab586 dev: add some validation
* 1b04ff79 dev: remove last items related to lc_cobol enablement
* dd768dcf add config to package.json and add reinit() and ensure init() is used when needed
* 9b6f1490 move list of id's into config (wip) ensure first paragraph is picked up when we have a "fake" pd
* e3f1cfc9 Ensure '/' is treated as comment during file processing  Fix #297

## 7.9.2 upd

## 7.9.1 upd
* 54da32fb prep for tomorrow
* 1a5a4a4c Tweak group items #296
* 7b56b7d7 tweak
* 6c89eba7 remove warning messages
* 63dd216b upfsyr

## 7.8.8  CHANGELOG.md
* b98e0a5a tidy
* 1c5441d4 remove markdown warnings

## 7.8.2 change to jsonc
* 18172274 remove dep'ed fuzzy search

## 7.8.1 refresh
* 283ef8ee remove features that extended the 'Micro Focus COBOL Extension' to be compliant with its license.

## 7.7.22 update ver
* 72b9b13a upd deps

## 7.7.21  CHANGELOG.md
* 220f3b27 add config for debugger ext

## 7.7.19 prep
* 475200e1 disable it, for the moment
* a799b9af add start of the debugger extension for special registers scope
* 19832c72 tidy code
* 82629684 Do not display the margin, if the line contains tab Fix #293
* f079adaa Apply fix under property - Fix #290
* 51a36c96 remove use of trie class
* 4dcd938c tweak provider
* 53b12ca3 upd
* 87186f84 Go To Definition with MF Cobol Extension Fix #290
* a535ae41 remove ";1", that did not affect compilation

## 7.7.3  CHANGELOG.md
* a4370f5f prep for delivery
* 9f8e1eab update version

## 7.6.30 add support for migrating tasks to mf
* 3853f264 flip fix
* c66f8b86 upd lock

## 7.6.24 ensure metadata is not used when extending the mf extension  - leave in memory cache in place
* 8257e289 enable a bit more, still wip.. so this might change...
* 0b92792e avoid the possible double flip scenario
* 94e04c35 add support for picking up source format from the mf settings  - this enables the commenter to be in-sync

## 7.6.22 prepare for delivery
* a42eaf74 fix compilation bug introduced by previous refactoring add support for using the 'real' Micro Focus extension
* ca8298a8 add dynamic context via settings (WIP)
* 9e534e13 add the building block of dual language id support
* 5cc5fbff add lc cobol enabler
* 78d48f8d refactor debugger check and allow people using gnucobol language to bypass the check
* 7d5fe7ac flip scope
* 8b89b40e changed missed scope changes
* 173fb843 Update README.md  - make labels easier to read
* e73587a1 tweak
* ad077c1b remove non-existant ref to language
* 8d5f325a cleanup forgotten unused files
* 5141f525 upd lock

## 7.6.12 update related to limited functionality mode
* babde13d upd lock

## 7.6.11 add some untrusted editing support

## 7.6.10 update README remove unused property
* 49571bee use cmd for dep'ed command
* e4506ee9 move getSourceFormat into a seperate file and add langid to doc, so it can be used with out of process scanner
* dac47cdd change process_metadata_cache_on_start to be executed via a private command and change comparision Fix #286
* b44411db unify coboleditor.margin & coboleditor.fileformat_strategy
* 92bb604b some lint based tweaks
* c45593c8 refactor a little more
* 7b2baad4 continue refactoring
* fdd8df9e use alternative update mechanism that caused update events
* 207431e0 next method.. to vslogger
* 702b1aa9 move stuff to VSLogger
* cb778ef2 move one more function into the vslogger
* 0d325689 continue refactoring
* b41d0418 move performance_now
* bec159e3 move isDirectory
* 8ed4bfcd continue refactoring
* b8fbfcf7 merge function
* e49b2526 update due to typescript changes
* b3d6c4df Margin setting not respected Fix #285  - blank lines caused fixed file format detection to break  - lines > 80 should case fixed file format detection to be considerred  - allow coboleditor.margin to overide file format

## 7.6.5 add more abbr to snippets add more information to the extension checks
* 28287b1b add quotes
* 77718535 continue the seperation of the depreciated caching support
* a7d5b1c2 continue refactor file handing
* e1863e29 continue refactoring the file apis, into vs based ones & non-vs

## 7.5.30 only show threading option on large workspaes
* 7aa4c384 remove check for ..x perms, as if used needs to be done everywhere...
* 3ff06e32 add option to enable recursive search but disable by default
* d759640d continue refactoring cobscanner/vscobscanner  - move file scanner to own mechanism  - use workspace findFile approach for scanner  - drop unused list of directories
* 4f53522a continue to refactor the vscobscanner
* 68874ba1 start to move vscobscanner code to seperate class, so the non-dep can be moved forward without the restrictions imposed by the old dep'ed code
* 415795e8 continue work cobscanner
* 1cdfab0f continue work on cobscanner
* d7e44f97 continue work on cobscanner  - fix bug with not updating a timestamp
* 9fee4680 commit performance changes for cobscanner
* b52db85e unlink not required in !depmode
* 815f11af continue to work on scanner  - useenv mode setup

## 7.5.22 prep
* c8feba4a fix a 'replace' problem start to refactor cobscanner
* 7647f5d5 update ref
* 00ab8a74 start to make it obvious which bits of code are dep'ed
* a26ebb3f try to fix user's broken config
* c8de26ba expand callable synbols to include the line number
* a4597c10 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* baa522f2 reduce the amount of items cached to the workspace at the expense of resolving them via the workspace
* 6b1bca15 Update stale.yml
* 1f933990 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* f675bc03 avoid bad entry in cache empty cache if depreciated setting is used
* c114271d Update publishit.sh
* 7c21aa63 use a inactivity based timer rather than a length of time timer  - inactivity is a lack of any message from the scanner

## 7.5.19 VSCode become unresponsible after run processAllFilesInWorkspace command in a huge worskspace Fix #282  - Wire up cache_metadata_time_limit, so processAllFilesInWorkspace can be cancelled early  - Change the process indicator to be finer
* 042414bf remove npe

## 7.5.17  CHANGELOG.md
* bd39204a don't change unless it a COBOL doc (ie: pp output)
* ad90ab7d flip to COBOL for temp doc
* bc016e26 tidy up
* 1fa50e3d use range
* 74dbe6ed continue work on copybook handling
* 1395bc59 only have word sub for the moment
* ea376a8d add a codelens for 'copy replacing' results
* f0f864ab start to wire in existing token'ed copy statement  - fix start to be before 'copy' verb
* cb23430a rename replace boolean, to allow it to include replace/replacing
* 592ea5e9 add range for copy statement
* 55dd15f1 make replace/replacing work better
* 075d24f2 add lowercase cobol alias for the snippets
* 2f747eb1 move version forward
* 5dff120a tweak
* 1f682ebe avoid security issue
* 632ccd06 continue work copybook handling
* ee4fc15d fix navigation of "copy of.." but this mechanism might be short-lived..
* 6ed3cae6 use only one style

## 7.5.2 prep
* e7745945 add experimental replace verb passing under option
* 3dedc2ae try to handle mf compound entry-statements
* ec4673ed simplify code
* d619a8bb tidy up token handling
* 93b2a034 add an initial impl of 'replace' functionality
* 71f14d0e fix offset for 'fake' filler items
* ba57b300 continue work on tweaking startColumn handling
* 753beb42 start to change COBOLToken/Token to be performant
* c8fa5989 fix bug with paragraphs and continue token/refactoring
* 3ddf8759 remove condition that is not required
* dc4c57d6 tweak token
* 131377b3 place the file system correctly
* d596bfb7 ensure pp turns off parse_copybooks_for_references
* 838a20bd create new doc on pp codelens
* d2f368f7 add codelens for pp, so we can see easier what is happening
* 49be90df use the copybook scan tokens
* 922d811f warn about out of sync cache when debugging and don't reparse

## 7.4.15 tweak packages
* 6e58a11d add linter_ignore_missing_copybook
* ee0adca0 add linter_ignore_missing_copybooks config

## 7.4.14 use a good default, for ref type, ignore unknown style
* 973c3e43 remove unused code
* 28d41ac6 continue work on references change $end-region to match $region

## 7.4.13 performance tweaks
* fc17b8d9 prepare for delivery
* 71fa75b3 ignore warnings
* d6e5ea87 honour visability on references
* 5d907713 continue work on forward references
* c719f114 restrict picking up of forward references
* 0a50a985 tidy up
* 430c3d25 ensure we don't get duplicate interface, enums in the cache
* 3b3a8fb9 tweak $set and tidy up COBOL/Messages on startup
* 533c8bca more updates for $set and readonly semantic colouring
* 4f039537 pickup variables for references in value clauses

## 7.4.10 prep for delivery
* 816bfcb6 remove unused var
* e3ff1209 remove duplicate find from source definition when "call xx" is found

## 7.4.8 comment out unused lines
* 28463c66 add extra diag messages
* 6f9730f7 delivery it
* 47511c46 add warning message for untested environments
* b140ac57 ensure OVSX envs are not set
* 6a0d0188 tidy and add a warning message
* 85933ffa don't blank but set it something bad
* 93aa195f tweak package.json to ensure publisher is set before hand
* 7660bb2d add pre-commit hooks
* cdf164f3 add pre-commit hook

## 7.4.7 prep
* 995db585 wire up
* ecb9561f prepare for delivery
* d54f8213 add some support for references in level 66 lines
* 94adc043 version updates

## 7.4.6 continue work on references & prepare del
* 426c3496 include schema and fix bugs shown up
* f21fc677 relax string to allow space for name elements
* f30cd82a tweaks
* 533b802b add schema

## 7.3.30 try to make the pp activation more robust
* 5e97ee30 add late sourcereference cat, as it is not a 2 pass scanner
* 60bbfd49 update packages

## 7.3.29 prepare for delivery  - only say the pp failed when it has fatally failed & log it
* 414647c7 re-enable "decl" section processing (hopefull does not break anything) tweak setting of fixed format
* 7889b579 fix problem with margin due to duplicated code
* c2a6a3b2 make the determination of fixed file format better don't register interest in tokens that are literals  (long standing silent bug)

## 7.3.27 add some bullet proofing
* a66f91d6 expand sourceref to include tokentype
* 2eeb7bfd add variables into semantic provider & config option
* 99fbe95c add the section/paragrams as "label"/declaration semantic tokens
* d23aad6f make it safer
* e0b6825b drop alias
* d910312d tweak logic on how to flip to the COBOL lang id

## 7.3.26 resort to changing the document type rather than having an alias lang
* 2955204d remove aliases and ensure enablements are correct
* 5e89ce0f package updates
* b542a9c8 update readme:
* 10bc1886 add some control about how a "single folder" workspace is updated

## 7.3.24.1 tweaks
* fc054d1a use of support.variable.cobol, was too gready, it colourised too much
* c3c3d035 protect update and log exception
* 0fcd1df9 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* cfaac98c prep
* bd5c88b9 Tweak update() to happen on a workspace and if enabled Fix #270
* 3d00b2ab enusre alias "cobol" works as well as "COBOL"

## 7.3.23  CHANGELOG.md
* 927df709 prep for delivery
* c7491479 avoid using meta.symbol.cobol, as it is not friendly to some themes add config for alias, simplify lang use
* ef3a4071 add lowercase cobol, so "alias" is configure the same way as the uppercase one
* 2e6e168c remove experimental_features, as it is not currently used
* 959b45f7 (from experimental feature)  - drop syntax task, as it does not work as good as I hoped it would
* 3a80f438 restrict commands to the right focus time
* 722fea39 fix enablement of command
* ac106e7e a little tidy up
* 1a774f69 remove unused code
* 12092b6d remove unused flags
* efc46950 package updates

## 7.3.18 comment out step
* 50a1ce94 prep
* cbf09e63 remove docs for depreciated caching
* e5efa673 allow metadata caching to be turned off
* 6d650c54 tidy up
* 0797b896 add "of" support to copybook open
* e3465c60 add support for hp cobol "COPY text-name [OF|IN] library-name"
* 7c820bf4 add version number
* ada4c833 update deps & tidy
* 9ae9b82e use same terminology
* 726204d3 make execute/debug submenus
* 054abd25 add debug submenu
* 40a6c44f fix bug in sourceview
* 6c48cc97 do not process anything until all pp are ready
* cc6b77cd wire in the global sym file cleanup code
* ff6210c4 add objects (untested)
* 75fb1c39 continue work with pp
* b432d1c8 show pp info after the pp has been activated
* 83991c4a inc release number
* 80e658c3 add some diagnostics for the pp into the output channel on startup
* 2cda4ad4 fix serialisation issue with bigint
* c42fdcea continue background work on workspace caching  - make workspace entries portable
* b6adafc4 don't need this now
* f8f5fa41 move to using one obj that will eventually contain portable/non-portable filename information
* 8bcd74c9 continue work on workspace cache
* d4fac48f retry on extension lookup, as it maybe still starting up
* 983da5c6 Syntax highligh wrong for `WHEN 78 MOVE` Fix #268
* a8681fe8 move to using bigint stats
* 5852fe23 add in the file cache
* db3dd1e2 start to add in the filename cache
* 648f259e use a temp area if no cache directory is available
* 5e62e0e3 start the process of re-introducing the workspace external process cache  - does not work at the moment :-(
* b5f7b7e9 continue the renaming
* e756e207 rename command to make it clear it is deprecated
* a4565fd7 move command to cobolplugin.deprecated.processAllFilesInWorkspace
* a27bdde7 add missing file
* e4e9180c move to new pp activation model
* 1ff9efcc add missing method
* ac1a4a39 start to rework the pp to be enabled by a list of extensions, hopefully allow them to be full active at the same time as this ext.
* e5bf5bb5 handle in version num
* 456c2eb5 update to include sdk.zip in build
* 92f33162 continue work on pp  - alter start to include packageJson of package registering    so more detailed error handling/information can be generated    later  - add callback interface for extra info used while pp'ing
* 81910bf4 couple of teaks for the acucobol dialect
* f8285bd2 flip a couple of things out of experimental status
* 5ed0348f add file-control as a end-del for a remark
* dceae0ae optimise use of settings obj
* e9df9bbe add version id, for better comparison, use ms for files
* 88fa796a add support for handling a external file system for gotodef (alias of prg-id)
* c3467931 remove message & remove unused field
* 3a8ce26c only clear specific parts of the cache
* f6e25eb5 add clear internal memory cache cmd continue work on pp
* 752b62a9 add crudge type back in
* 3e75ff4f add more scanner callbacks
* 27f26f38 add types
* ba72cca0 fix bug in use of config.
* e9257cbb add more pp support add cross process support for ep/prg's to scanner
* c45a91d5 continue tweaking the pp interface
* 141de11f add more of the pp interface
* ae8219c2 continue work on pp
* 61e61c75 put a crudge process method in place
* 9328c33a start to put a framework in place for a external preprocessor interface
* 798ca9c6 change name to cob*api*ts
* ae82ce96 start the api experiment
* b64e943d continue breaking out the globals
* 429739d9 add simple call/cancel targets
* 1fce6b33 start to wrire back in ep data into the provider
* 693d848b load ep data on activation
* 1e6e2fa2 remvoe all entry-point symbols before parsing program
* 75b8a52d add more of the entrypoint support
* 2e028e97 tidy up code
* a3908375 move external sym providers into sep file`
* c98fed42 continue to remove references to the non-file sym files
* 44029b61 wip - control work workspace metadata
* 2cfaffad add symbol load/setup
* 40ed5a37 start to re-wire the symbol use
* d10782b1 not ready but don't want to loose it
* b250adb9 continue work moving to workspace move
* e58fab8a pass symbols into scanner
* 1798cbc6 create metadata area in workspace for global symbols
* 612e736d move to next month
* ac4ebc8e just incase commit
* 6d4ec8ac add onEnterRules for call/evaluate remove experimental example from readme, as it is not present

## 7.1.1 paragraphs missing in outline Fix #266   - parsing of comment lines via the scanner internals was changing     the behavour on multi-line scanning

## 7.1.0 introduce cobolit as a seperate language

## 7.0.13  CHANGELOG.md
* ee4154f8 fix script
* d0c97428 update vscode engine
* 5bd399ab update packages, remove html parser as it is not used (was for the failed coboldoc support)
* d36d037f add gen_third_party.sh to the ignore file
* d8e03d10 add third party notices
* e5c4d936 Move away from using uuid, as it seems to cause a clash on Windows, other platforms are okay.

## 7.0.12  CHANGELOG.md
* 0989dd45 add in missing acucobol

## 7.0.11  CHANGELOG.md
* 6eaef5c9 tidy ignore extra .md
* 138ff44f tidy up build
* f4a73c12 tweak the format
* 14bce46d tweak

## 7.0.10  CHANGELOG.md
* 875bf522 tweak gen-changelog
* 2281745e change log is broken, so remove until I can resolve the issue
* e076f85a continue adding dep marker
* 3d873823 add "delimited by" snippets
* 85946853 add "end program" and fix lower-case program-id
* 62105448 update & bring inline with other uses of MIT license (.Net Core)
* c893b41e current tags and releases missing Fix #258  - include changelog generation
* 8ff07e61 tweak cobol provider
* 026a9d41 be cautious on the unlink
* 5351afea dump metadata to file Fix #254
* e4b54ee5 tweak message
* 37bf98d4 avoid warning message is only divs are in header of program
* 9a4bfe67 changes to better handle implicit program-ids fix problem with untrimmed token that do not end with a full stop
* 9eb4b996 Avoid picking up "entry" as a entry point - #138
* f7fa3003 add depreciated tags to the settings that are scheduled for replacement or removal
* eaa09ebf inc ver
* 920b10eb Metadata parsing misses some sections #255  - tweak symbol catcher and event generator
* 302c9be3 fix typo
* 66247321 more cleanup, remove unused functionality
* 2f1ea99c add info about changing token colours
* 9d1a12c5 turn off git/release as it will now only be used for private non-public builds

## 7.0.0 prepare for first release of the new year
* d232f5ee update readme with spell checker information
* 58129dec breakout condition names
* 71e77e46 tidy
* 67383c77 tidy up
* 265cebd6 remove gnu list file from the file selectors
* c17b526d add simple formatter for camelcase/uppercase on return-code  - under experimential flag
* 07f72abc tidy up use of COBOLUtils, move to statics, as no instance data is required
* 21826560 Remove a missed part of the old coboldoc support
* ab16f471 remove "Open COBOL" reference add note, about contributions & gnucobol extension

## 6.11.11 prep/push
* cfcfb937 continue work on the task provider
* 3ee76f41 make it private
* a5fd8550 expand the possible script names
* 1b8e44e0 add dbcs literals
* 2e7c36dd add jnienvptr
* f4d5648d fix compilation issue
* 2deeb4f6 sort the table to aid readability
* 311608f3 tweak special registers
* 7954fe04 add in utf-8 literal delimiters
* 9c2c2bfa Update stale.yml
* 968c9793 add some discovered reserved words from netcobol, as known but illegal
* 00308c7f add more keywords from the iso2002 spec and add a config to ignore the first section if an entyr-point is next, its crudge needs more improvement

## 6.11.9 add cobol-it to the task provider

## 6.11.8 remove warning matcher is task provider for acu

## 6.11.7 remove the last the non-module gnucobol bits, the extension is now split into two.
* 5c1e0958 continue the removal of anything GnuCOBOL related, so it can be pushed into its own extension.
* 53fe7d2b packages update & vscode

## 6.11.6 add problem matchers to the default config
* c51fe393 fix outstanding overlooked bug with the keyword provider

## 6.11.5 continue minor tweaks to task provider.. more work to do here... comment out subjective code in keyword provider, as I have send times it causes more harm than good (aka it prevents keywords from being returned
* 5f4072b7 start to wire up some options for the script provider
* 50337da7 bring back the bld script task

## 6.11.4 second fix to problem matcher (out of sync message

## 6.11.3 fix problem matcher

## 6.11.2 simplify previous change for catching different category of messages

## 6.11.1 add support Micro Focus ECM error categories: CICS, SQL, XDB, IMS and DB2
* ce067b34 let vscode decide where the problem matcher find files (when we can)
* e78175b9 use autoDetect on some of the problem matchers
* d628ffdf further tweaks
* 2cf01a62 File is too long? Fix #249

## 6.10.24 add extra brackets & if,unstring,string into completion list

## 6.10.23 tweak the brackets support to handle if/end-if and a few more  (more may come, I just want to see who this pans out
* fd2f7609 tweak $schema location
* f6eff348 not required
* 51e21bc9 add schema, as the original one has gone.. so include so I can reference it

## 6.10.22 add some cbltypes as support.function add 78 specific colouriser
* 58371aaa add bot

## 6.10.21 tweak casing util and fix some storage items missed
* 8884af7e gnucobol related tidy up
* d26b4b3d tweaks for more field types for bms  - fix unrequired , in package.json
* a232fd75 start to think about web use of the extension  - can't do too much until I get codespace access
* abf43784 add support for "exec java" used in some java based COBOL dialects fix "-" in first column that affected some comment lines
* ef510bc1 ensure we don't change to a document type that does not exist
* f8fccf89 update changelog
* 93767869 fix links
* b39d1229 changes related to master -> main branch name change
* 0c58f20b move to main
