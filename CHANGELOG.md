# CHANGELOG

* 11b326b (HEAD -> main, tag: 7.12.14, origin/main) update engine
* ed612d8 Update CHANGELOG.md
* d9806dc bump
* 1bbb045 keep cobol-it keywords insync fix changes found by later typescript compiler
* fd60b81 update
* e7986d9 Update CHANGELOG.md
* 2d08278 (tag: 7.12.12) tweak settings
* 8e15dd3 add in-memory cache prop
* 2619b94 extend diag information
* 41e3d70 bump
* c368826 tweaks
* a5b2d38 make prop consistent
* 2fc81c4 remove dep method use add line length prop (WIP)
* 650442c fix use of editor_maxTokenizationLineLength, start to include settings for file exclusions
* 35b3566 start to setup some core for aborting a source scan early
* 8bd3987 tweaks
* f075d40 bump versions
* 65b3d7f Update CHANGELOG.md
* a3b143b (tag: 7.12.6) bump and publish
* acfff9a add more functionality, hopefully avoiding everything depend on fs
* 80ecd45 add info message
* b865cf4 change scope
* 0f245e8 move function to class
* 893a417 move two function into util class
* f4fc515 refactor VSSourceFormat
* 3963081 refactor VSWorkspaceFolders
* 671991c refactor margindec into class add margin support to web ext
* e1430f7 Update CHANGELOG.md
* cf0ab6b (tag: 7.12.4) tweak
* c458f87 Update CHANGELOG.md
* 0e4ddec remove debug code
* 20bab31 remove reference to dep'ed vsix extension and reconsiders it use
* c8eb085 version bumps
* 0a1a604 Update CHANGELOG.md
* 27a0d3a (tag: 7.12.3) remove line
* 376e3d1 Update CHANGELOG.md
* 393a3bd add more schema's in
* df39ba2 Update CHANGELOG.md
* 37fc7d1 (tag: 7.12.2) add diag code
* 0d8c013 Update readme
* a0f31f4 Update CHANGELOG.md
* b646ff7 (tag: 7.12.1) bump
* 64adc11 Tweaks to fix #308
* 9467360 remove use of path.normalise
* ce7699a put try/catch in place (to remove at somepoint)
* 2882f79 add external browser
* b16ecc5 add in untitled schema
* 0118ad6 move to simplier changelog, as the java version crashes too much
* ec8b334 update
* 9473e20 Update CHANGELOG.md
* 671ed58 (tag: 7.12.0) tweak and prep
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
* fd31ab9 bump
* 0c69946 tidy up deps
* 973a529 remove old-cache options
* 6f97f85 remove cache dir
* a0f9d11 remove dep'ed cobscanner
* 3d7b1ca start to remove depreciated features
* 0d02c7b Update stale.yml
* ac56474 Update CHANGELOG.md
* 0d24368 (tag: 7.11.1) add info about review
* ed59814 change work to be more forceful with reguards dep to be removed features
* 51259f2 tidy up import/drop "export defaults"
* f1b4e19 tidy up
* b8ac4a3 Update CHANGELOG.md
* 56ea601 (tag: 7.10.30) remove empty impl, tidy up fs usage again
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
* c0f29e4 Update CHANGELOG.md
* 371ed1a (tag: 7.10.21) missed one
* 826c7e9 Update CHANGELOG.md
* d0e2fae bump and disable various bits when used from a browser
* 5f4c5af update
* 01ea9ba Update CHANGELOG.md
* dae3eb2 (tag: 7.10.20) add some experimental support for web extension
* b362701 Update CHANGELOG.md
* 3710ead (tag: 7.10.19) bump
* 31b7012 disable "editor.bracketPairColorization.enabled" as it causes odd colourisation with string & unstring tokens
* 182d5c6 tidy up
* 5b16841 Update CHANGELOG.md
* 3598dc5 (tag: 7.10.17) bump
* 20d16c2 enable editor.bracketPairColorization.enabled on a per language basis
* a8cd2a2 Update CHANGELOG.md
* e9bfb89 (tag: 7.10.16) bump
* 9204f81 Fix bracket issue with ref mod items
* 19af0a8 Update CHANGELOG.md
* 4d047a7 (tag: 7.10.15) add some docs for xedit'ish keys
* e140f50 update
* 2bb4fc6 Update CHANGELOG.md
* 755e5c5 (tag: 7.10.14) add transpose
* d434d01 fix bug in "'perform" isense
* 44aab1b Update CHANGELOG.md
* 57fbaea (tag: 7.10.13) add some simple keymaps for xedit enabled via config
* 2bec112 editor.renderIndentGuides has been removed in 1.61
* 620a3d1 update
* ec521d7 Update CHANGELOG.md
* ce958c2 (tag: 7.10.11) bump
* 876a46d fix alignment in README.md table
* a1732c2 Update CHANGELOG.md
* 0f88e59 (tag: 7.10.9) tweak
* c862416 Update CHANGELOG.md
* 62e8bbd change adjust cursor to ctrl+alt+a and add adjust left margin ctrl+alt+l
* e4c122b update
* 81b8ac8 Update CHANGELOG.md
* 48bfcb8 (tag: 7.10.8) Release 'indent' to cursor - ctrl+alt+i
* 98a2276 Update CHANGELOG.md
* 1b9b5ec (tag: 7.10.4) add BITLANG-COBOL id
* 241dfcd Update CHANGELOG.md
* 01a8548 (tag: 7.10.2) remove reference to switcher
* f438f4d Update CHANGELOG.md
* 5125b8f (tag: 7.10.1) update
* e6f8a57 Update CHANGELOG.md
* 89ceb8d new month
* a2b7db6 Update CHANGELOG.md
* 2fd530e (tag: 7.9.27) bump
* fb95f0b bump
* 795ca4d remove possible security issue with npm package (nth-check)
* c0be380 Update CHANGELOG.md
* 2959a57 (tag: 7.9.21) tweak acu syntax
* ba57b34 Update CHANGELOG.md
* 716706a (tag: 7.9.20) bump
* 78dd575 tidy up/fix bug
* fc7962b bump up versions
* 084ff05 Update CHANGELOG.md
* 21ad2d0 (tag: 7.9.19) bump
* 3d8631e update acucobol keywords
* ee734a0 just-incase code to ensure we don't get duplicates in the lists
* 3aa8f79 update syntax
* 8e0ce26 Update CHANGELOG.md
* 8d5684c (tag: 7.9.18) bump
* e5b4dc4 start to make keywords language dialect aware
* 5a9f036 simplify changelog
* b8e10bf Update CHANGELOG.md
* 919fc8a (tag: 7.9.16) bump
* 732922b Update CHANGELOG.md
* e9db791 (tag: 7.9.15) update changelog
* d0fc0d3 Update CHANGELOG.md
* 91d7b56 (tag: 7.9.9) bump
* b65a551 remove use of prefer_gnucobol_syntax
* 656379e update to next vscode engine
* 9d21ae5 Update CHANGELOG.md
* 20ea088 (tag: 7.9.8) prep
* eaa8529 handle single line '/' - Fix #297
* bc7d465 Update CHANGELOG.md
* 0aa15e8 (tag: 7.9.5) bump
* 817ab58 dev: add some validation
* 1b04ff7 dev: remove last items related to lc_cobol enablement
* dd768dc add config to package.json and add reinit() and ensure init() is used when needed
* 9b6f149 move list of id's into config (wip) ensure first paragraph is picked up when we have a "fake" pd
* e3f1cfc Ensure '/' is treated as comment during file processing  Fix #297
* c13e9d2 Update CHANGELOG.md
* 386d67a (tag: 7.9.2) upd
* 61edf55 (tag: 7.9.1) upd
* 1067cc8 Update CHANGELOG.md
* b409563 bump and setup for sept
* 54da32f prep for tomorrow
* f8b4554 Update CHANGELOG.md
* 78d1ecb (tag: 7.8.27) bump
* 1a5a4a4 Tweak group items #296
* 7b56b7d tweak
* 8c11cd2 update
* 53f2987 Update CHANGELOG.md
* a5e642e (tag: 7.8.19) update
* 6c89eba remove warning messages
* 63dd216 upfsyr
* ae2a0a7 Update CHANGELOG.md
* f0c5e68 (tag: 7.8.8) Update CHANGELOG.md
* ac2b2f5 update
* b98e0a5 tidy
* bc68f18 Update CHANGELOG.md
* ebb5dfb (tag: 7.8.7) update
* ca013e5 Update CHANGELOG.md
* ff497db update
* cfdad41 update
* 9ec89b4 Update CHANGELOG.md
* b861162 (tag: 7.8.6) bump
* 1c5441d remove markdown warnings
* 8eac8ef Update CHANGELOG.md
* 71503aa (tag: 7.8.2) change to jsonc
* 167b7cd Update CHANGELOG.md
* 346db92 update
* 1817227 remove dep'ed fuzzy search
* 21a852a Update CHANGELOG.md
* 43dbe82 (tag: 7.8.1) refresh
* 55c4702 Update CHANGELOG.md
* 03c1f1d (tag: 7.7.28) update
* 03e2f15 Update CHANGELOG.md
* 283ef8e remove features that extended the 'Micro Focus COBOL Extension' to be compliant with its license.
* 8805329 Update CHANGELOG.md
* d2b2c32 (tag: 7.7.22) update ver
* 67289e1 Update CHANGELOG.md
* 72b9b13 upd deps
* bfb7952 Update CHANGELOG.md
* 3520458 (tag: 7.7.21) Update CHANGELOG.md
* 94dcb91 bump
* 220f3b2 add config for debugger ext
* 5f73a5e Update CHANGELOG.md
* aeea7ac (tag: 7.7.19) prep
* 475200e disable it, for the moment
* a799b9a add start of the debugger extension for special registers scope
* 19832c7 tidy code
* 8262968 Do not display the margin, if the line contains tab Fix #293
* da28390 Update CHANGELOG.md
* a9ce1af (tag: 7.7.9) bump version
* f079ada Apply fix under property - Fix #290
* 51a36c9 remove use of trie class
* 4dcd938 tweak provider
* dbfe9ad Update CHANGELOG.md
* d523b53 (tag: 7.7.8) bump
* cd1d247 Update CHANGELOG.md
* 368fd9d Update CHANGELOG.md
* 53b12ca upd
* 87186f8 Go To Definition with MF Cobol Extension Fix #290
* a535ae4 remove ";1", that did not affect compilation
* 572bb6d update
* 45641b5 Update CHANGELOG.md
* db00a0b (tag: 7.7.3) Update CHANGELOG.md
* a4370f5 prep for delivery
* 9f8e1ea update version
* 492271e Update CHANGELOG.md
* cf04d54 (tag: 7.6.30) add support for migrating tasks to mf
* 3853f26 flip fix
* c66f8b8 upd lock
* 9065bcc Update CHANGELOG.md
* cfd932e (tag: 7.6.24) ensure metadata is not used when extending the mf extension  - leave in memory cache in place
* a94ba23 Update CHANGELOG.md
* 9c7bbd6 (tag: 7.6.23) bump
* 8257e28 enable a bit more, still wip.. so this might change...
* 0b92792 avoid the possible double flip scenario
* 94e04c3 add support for picking up source format from the mf settings  - this enables the commenter to be in-sync
* 33cbd8f update
* f5e8d4a Update CHANGELOG.md
* 17bcf6b (tag: 7.6.22) prepare for delivery
* a42eaf7 fix compilation bug introduced by previous refactoring add support for using the 'real' Micro Focus extension
* ca8298a add dynamic context via settings (WIP)
* 9e534e1 add the building block of dual language id support
* 5cc5fbf add lc cobol enabler
* 1e8ea29 update
* 9d34bb7 Update CHANGELOG.md
* 2e21298 (tag: 7.6.15) bump
* 78d48f8 refactor debugger check and allow people using gnucobol language to bypass the check
* 7d5fe7a flip scope
* 8b89b40 changed missed scope changes
* 173fb84 Update README.md  - make labels easier to read
* e73587a tweak
* ad077c1 remove non-existant ref to language
* 8d5f325 cleanup forgotten unused files
* 5141f52 upd lock
* fe0bc3a Update CHANGELOG.md
* 63bf6bd (tag: 7.6.12) update related to limited functionality mode
* babde13 upd lock
* 40eac93 Update CHANGELOG.md
* ce105fd (tag: 7.6.11) add some untrusted editing support
* 1559a31 update
* 8004e1f Update CHANGELOG.md
* 4907965 (tag: 7.6.10) update README remove unused property
* 73bdffd Update CHANGELOG.md
* 9113c93 (tag: 7.6.9) bump
* 49571be use cmd for dep'ed command
* 994544a Update CHANGELOG.md
* 73d1e56 (tag: 7.6.8) update
* e4506ee move getSourceFormat into a seperate file and add langid to doc, so it can be used with out of process scanner
* dac47cd change process_metadata_cache_on_start to be executed via a private command and change comparision Fix #286
* b44411d unify coboleditor.margin & coboleditor.fileformat_strategy
* 92bb604 some lint based tweaks
* c45593c refactor a little more
* 7b2baad continue refactoring
* 2608228 update
* 0e40e3c Update CHANGELOG.md
* 253a254 (tag: 7.6.7) bump
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
* 925834e Update CHANGELOG.md
* 668aefc (tag: 7.6.6) bump
* b3d6c4d Margin setting not respected Fix #285  - blank lines caused fixed file format detection to break  - lines > 80 should case fixed file format detection to be considerred  - allow coboleditor.margin to overide file format
* ba0d806 Update CHANGELOG.md
* 1c71f15 (tag: 7.6.5) add more abbr to snippets add more information to the extension checks
* 28287b1 add quotes
* 0c93e2d update
* 4b80c19 Update CHANGELOG.md
* 44f6d75 (tag: 7.5.31) update
* 05b5290 Update CHANGELOG.md
* 7771853 continue the seperation of the depreciated caching support
* a7d5b1c continue refactor file handing
* e1863e2 continue refactoring the file apis, into vs based ones & non-vs
* 26bed67 update
* d642ca7 Update CHANGELOG.md
* 8535c41 (tag: 7.5.30) only show threading option on large workspaes
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
* 733f507 Update CHANGELOG.md
* 3fe920f (tag: 7.5.23) bump
* d0458d9 Update CHANGELOG.md
* aec2564 (tag: 7.5.22) prep
* c8feba4 fix a 'replace' problem start to refactor cobscanner
* 7647f5d update ref
* 00ab8a7 start to make it obvious which bits of code are dep'ed
* a26ebb3 try to fix user's broken config
* c8de26b expand callable synbols to include the line number
* 19819d2 update
* 54c7551 Update CHANGELOG.md
* cbdac47 bump
* a4597c1 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* baa522f reduce the amount of items cached to the workspace at the expense of resolving them via the workspace
* 6b1bca1 Update stale.yml
* 50b3caa update
* 6a415b7 Update CHANGELOG.md
* 63e8945 (tag: 7.5.20) bump
* 1f93399 Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* f675bc0 avoid bad entry in cache empty cache if depreciated setting is used
* c114271 Update publishit.sh
* 7c21aa6 use a inactivity based timer rather than a length of time timer  - inactivity is a lack of any message from the scanner
* 33511bc Update CHANGELOG.md
* ee9384f (tag: 7.5.19) VSCode become unresponsible after run processAllFilesInWorkspace command in a huge worskspace Fix #282  - Wire up cache_metadata_time_limit, so processAllFilesInWorkspace can be cancelled early  - Change the process indicator to be finer
* 1c6f4e1 update
* 98ae406 Update CHANGELOG.md
* 6f36d0f (tag: 7.5.18) bump and tweak info
* 042414b remove npe
* c40d377 update
* fbe16e2 Update CHANGELOG.md
* 99fcf59 (tag: 7.5.17) Update CHANGELOG.md
* 9589f15 bump
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
* 82922fc update
* 632ccd0 continue work copybook handling
* ee4fc15 fix navigation of "copy of.." but this mechanism might be short-lived..
* 6ed3cae use only one style
* 454aa5d bump versions
* c423be5 Update CHANGELOG.md
* b5ba480 (tag: 7.5.2) prep
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
* e736c87 update
* 838a20b create new doc on pp codelens
* d2f368f add codelens for pp, so we can see easier what is happening
* 49be90d use the copybook scan tokens
* 922d811 warn about out of sync cache when debugging and don't reparse
* c2f6eaa package update
* 3e26e8c Update CHANGELOG.md
* ac3cea0 (tag: 7.4.15) tweak packages
* 6e58a11 add linter_ignore_missing_copybook
* aa257e8 bump version
* ee0adca add linter_ignore_missing_copybooks config
* ed46a2c Update CHANGELOG.md
* 6fb3309 (tag: 7.4.14) use a good default, for ref type, ignore unknown style
* 973c3e4 remove unused code
* 28d41ac continue work on references change $end-region to match $region
* c0e5975 Update CHANGELOG.md
* 3ed3fb1 (tag: 7.4.13) performance tweaks
* fc17b8d prepare for delivery
* 71fa75b ignore warnings
* d6e5ea8 honour visability on references
* 3e4bf8d Update CHANGELOG.md
* fcc25a1 (tag: 7.4.12) bump version
* 5d90771 continue work on forward references
* c719f11 restrict picking up of forward references
* d3258bf Update CHANGELOG.md
* c6df53d (tag: 7.4.11) bump version
* 0a50a98 tidy up
* 430c3d2 ensure we don't get duplicate interface, enums in the cache
* 3b3a8fb tweak $set and tidy up COBOL/Messages on startup
* 533c8bc more updates for $set and readonly semantic colouring
* 4f03953 pickup variables for references in value clauses
* 9478b60 Update CHANGELOG.md
* d6d80b6 (tag: 7.4.10) prep for delivery
* 816bfcb remove unused var
* e3ff120 remove duplicate find from source definition when "call xx" is found
* 14000fa Update CHANGELOG.md
* d1c974e (tag: 7.4.8) comment out unused lines
* 67ef83e Update CHANGELOG.md
* 28463c6 add extra diag messages
* 6f9730f delivery it
* 47511c4 add warning message for untested environments
* b140ac5 ensure OVSX envs are not set
* c070aa4 update
* 6a0d018 tidy and add a warning message
* 85933ff don't blank but set it something bad
* 93aa195 tweak package.json to ensure publisher is set before hand
* 7660bb2 add pre-commit hooks
* cdf164f add pre-commit hook
* 872bdf2 Update CHANGELOG.md
* 4e212a4 (tag: 7.4.7) prep
* 2266e21 Update CHANGELOG.md
* 995db58 wire up
* ecb9561 prepare for delivery
* d54f821 add some support for references in level 66 lines
* 94adc04 version updates
* 6eb6ca8 Update CHANGELOG.md
* 63192c2 (tag: 7.4.6) continue work on references & prepare del
* 426c349 include schema and fix bugs shown up
* f21fc67 relax string to allow space for name elements
* f30cd82 tweaks
* 533b802 add schema
* ae683af Update CHANGELOG.md
* acf35cd (tag: 7.3.30) try to make the pp activation more robust
* 5e97ee3 add late sourcereference cat, as it is not a 2 pass scanner
* 60bbfd4 update packages
* 408b29b Update CHANGELOG.md
* edb7a09 (tag: 7.3.29) prepare for delivery  - only say the pp failed when it has fatally failed & log it
* 414647c re-enable "decl" section processing (hopefull does not break anything) tweak setting of fixed format
* 7889b57 fix problem with margin due to duplicated code
* 0dbe716 package update
* 78bd21d Update CHANGELOG.md
* 15cf704 (tag: 7.3.28) bump and deliver
* c2a6a3b make the determination of fixed file format better don't register interest in tokens that are literals  (long standing silent bug)
* a6bea1e Update CHANGELOG.md
* ffd5a50 (tag: 7.3.27) add some bullet proofing
* a66f91d expand sourceref to include tokentype
* 2eeb7bf add variables into semantic provider & config option
* 99fbe95 add the section/paragrams as "label"/declaration semantic tokens
* d23aad6 make it safer
* e0b6825 drop alias
* d910312 tweak logic on how to flip to the COBOL lang id
* 6ccf760 Update CHANGELOG.md
* 193083e (tag: 7.3.26) resort to changing the document type rather than having an alias lang
* 2955204 remove aliases and ensure enablements are correct
* 5e89ce0 package updates
* 2ee8521 Update CHANGELOG.md
* bf184e1 bump
* b542a9c update readme:
* 10bc188 add some control about how a "single folder" workspace is updated
* 2ac7f89 Update CHANGELOG.md
* 56e8803 (tag: 7.3.25) update
* 12774ff Update CHANGELOG.md
* 38b11be (tag: 7.3.24) update
* 37e9d91 Update CHANGELOG.md
* 1488097 (tag: 7.3.24.1) tweaks
* fc054d1 use of support.variable.cobol, was too gready, it colourised too much
* c3c3d03 protect update and log exception
* 87ec503 Update CHANGELOG.md
* 0fcd1df Merge branch 'main' of github.com:spgennard/vscode_cobol into main
* cfaac98 prep
* bd5c88b Tweak update() to happen on a workspace and if enabled Fix #270
* 3d00b2a enusre alias "cobol" works as well as "COBOL"
* 09ae55e Update CHANGELOG.md
* e00b9ea (tag: 7.3.23) Update CHANGELOG.md
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
* 0557fd3 Update CHANGELOG.md
* 600923d (tag: 7.3.18) comment out step
* 514c456 Update CHANGELOG.md
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
* 5cb0296 reduce refresh rate of config update
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
* 4202bf7 Update CHANGELOG.md
* e75e75c (tag: 7.1.1) paragraphs missing in outline Fix #266   - parsing of comment lines via the scanner internals was changing     the behavour on multi-line scanning
* 096e69f Update CHANGELOG.md
* 9c0f1b2 (tag: 7.1.0) introduce cobolit as a seperate language
* ff3fc85 Update CHANGELOG.md
* 15b9786 (tag: 7.0.14) bump
* 45dd4c9 (tag: 7.0.13) Update CHANGELOG.md
* ee4154f fix script
* b168461 bump
* d0c9742 update vscode engine
* 5bd399a update packages, remove html parser as it is not used (was for the failed coboldoc support)
* d36d037 add gen_third_party.sh to the ignore file
* d8e03d1 add third party notices
* e5c4d93 Move away from using uuid, as it seems to cause a clash on Windows, other platforms are okay.
* bef35f9 (tag: 7.0.12) Update CHANGELOG.md
* 13ca76a bump
* 0989dd4 add in missing acucobol
* 2d4b18d (tag: 7.0.11) Update CHANGELOG.md
* 0f0f9df bump
* 6eaef5c tidy ignore extra .md
* 138ff44 tidy up build
* f4a73c1 tweak the format
* 59f6b78 update
* 14bce46 tweak
* 834837e (tag: 7.0.10) Update CHANGELOG.md
* 875bf52 tweak gen-changelog
* 2281745 change log is broken, so remove until I can resolve the issue
* 58d534f Update CHANGELOG.md
* 003d0d9 bump
* e076f85 continue adding dep marker
* 3d87382 add "delimited by" snippets
* 8594685 add "end program" and fix lower-case program-id
* 6210544 update & bring inline with other uses of MIT license (.Net Core)
* c893b41 current tags and releases missing Fix #258  - include changelog generation
* 0af5df6 bump
* 8ff07e6 tweak cobol provider
* 026a9d4 be cautious on the unlink
* 5351afe dump metadata to file Fix #254
* e164f0d bump
* e4b54ee tweak message
* 37bf98d avoid warning message is only divs are in header of program
* 9a4bfe6 changes to better handle implicit program-ids fix problem with untrimmed token that do not end with a full stop
* bcf28f6 bump
* 9eb4b99 Avoid picking up "entry" as a entry point - #138
* f7fa300 add depreciated tags to the settings that are scheduled for replacement or removal
* eaa09eb inc ver
* 920b10e Metadata parsing misses some sections #255  - tweak symbol catcher and event generator
* 302c9be fix typo
* 6624732 more cleanup, remove unused functionality
* 2f1ea99 add info about changing token colours
* 9d1a12c turn off git/release as it will now only be used for private non-public builds
* d8f6dfd date update
* 0a1bd70 (tag: 7.0.0) prepare for first release of the new year
* d232f5e update readme with spell checker information
* 58129de breakout condition names
* 71e77e4 tidy
* 67383c7 tidy up
* 265cebd remove gnu list file from the file selectors
* c17b526 add simple formatter for camelcase/uppercase on return-code  - under experimential flag
* 07f72ab tidy up use of COBOLUtils, move to statics, as no instance data is required
* 2182656 Remove a missed part of the old coboldoc support
* ab16f47 remove "Open COBOL" reference add note, about contributions & gnucobol extension
* 8272e2b (tag: 6.11.11) prep/push
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
* 15b135b (tag: 6.11.9) add cobol-it to the task provider
* 8e1cfc3 (tag: 6.11.8) remove warning matcher is task provider for acu
* 6fb56e7 (tag: 6.11.7) remove the last the non-module gnucobol bits, the extension is now split into two.
* 5c1e095 continue the removal of anything GnuCOBOL related, so it can be pushed into its own extension.
* 53fe7d2 packages update & vscode
* 4a4399a (tag: 6.11.6) add problem matchers to the default config
* c51fe39 fix outstanding overlooked bug with the keyword provider
* 5d2db1e (tag: 6.11.5) continue minor tweaks to task provider.. more work to do here... comment out subjective code in keyword provider, as I have send times it causes more harm than good (aka it prevents keywords from being returned)
* 5f4072b start to wire up some options for the script provider
* 50337da bring back the bld script task
* 111ff4c (tag: 6.11.4) second fix to problem matcher (out of sync message)
* d567c13 (tag: 6.11.3) fix problem matcher
* eb67395 (tag: 6.11.2) simplify previous change for catching different category of messages
* cef6126 (tag: 6.11.1) add support Micro Focus ECM error categories: CICS, SQL, XDB, IMS and DB2
* ce067b3 let vscode decide where the problem matcher find files (when we can)
* e78175b use autoDetect on some of the problem matchers
* d628ffd further tweaks
* 2cf01a6 File is too long? Fix #249
* bf87255 update
* 88a2368 (tag: 6.10.24) add extra brackets & if,unstring,string into completion list
* 0e2b616 (tag: 6.10.23) tweak the brackets support to handle if/end-if and a few more  (more may come, I just want to see who this pans out)
* fd2f760 tweak $schema location
* f6eff34 not required
* 51e21bc add schema, as the original one has gone.. so include so I can reference it
* bcccd26 (tag: 6.10.22) add some cbltypes as support.function add 78 specific colouriser
* 58371aa add bot
* c241c42 (tag: 6.10.21) tweak casing util and fix some storage items missed
* 5cec68a update
* e373cfc (tag: 6.10.20) bump
* 8884af7 gnucobol related tidy up
* 0b9ec35 bump
* d26b4b3 tweaks for more field types for bms  - fix unrequired , in package.json
* a232fd7 start to think about web use of the extension  - can't do too much until I get codespace access
* 6cff853 bump
* abf4378 add support for "exec java" used in some java based COBOL dialects fix "-" in first column that affected some comment lines
* ef510bc ensure we don't change to a document type that does not exist
* f8fccf8 update changelog
* 9376786 fix links
* b39d122 changes related to master -> main branch name change
* e7e3633 bump
* 0c58f20 move to main
