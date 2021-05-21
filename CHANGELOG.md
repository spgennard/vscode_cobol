
# Changelog

Changelog for spgennardvscode_cobol.

## 7.5.22

**reduce the amount of items cached to the workspace at the expense of**

 * resolving them via the workspace
* [baa522f2e186a3a](https://github.com/spgennard/vscode_cobol/commit/baa522f2e186a3a) *2021-05-21 23:23:57*

**Update stale.yml**

* [6b1bca15d1a4258](https://github.com/spgennard/vscode_cobol/commit/6b1bca15d1a4258) *2021-05-21 15:04:38*

**update**

* [50b3caa39ae226d](https://github.com/spgennard/vscode_cobol/commit/50b3caa39ae226d) *2021-05-20 21:37:56*

**Update CHANGELOG.md**

* [6a415b7edc40ca2](https://github.com/spgennard/vscode_cobol/commit/6a415b7edc40ca2) *2021-05-20 21:34:18*


## 7.5.20

**avoid bad entry in cache**

 * empty cache if depreciated setting is used
* [f675bc032ee9b10](https://github.com/spgennard/vscode_cobol/commit/f675bc032ee9b10) *2021-05-20 21:32:16*

**Update publishit.sh**

* [c114271d864fe08](https://github.com/spgennard/vscode_cobol/commit/c114271d864fe08) *2021-05-20 10:41:37*

**use a inactivity based timer rather than a length of time timer**

 * - inactivity is a lack of any message from the scanner
* [7c21aa63b18a19b](https://github.com/spgennard/vscode_cobol/commit/7c21aa63b18a19b) *2021-05-19 22:23:28*

**Update CHANGELOG.md**

* [33511bcdb5f9cf9](https://github.com/spgennard/vscode_cobol/commit/33511bcdb5f9cf9) *2021-05-19 21:25:11*


## 7.5.19
### GitHub [#282](https://github.com/spgennard/vscode_cobol/issues/282) VSCode become unresponsible after run processAllFilesInWorkspace command in a huge worskspace  

**VSCode become unresponsible after run processAllFilesInWorkspace command in a huge worskspace Fix #282**

 * - Wire up cache_metadata_time_limit, so processAllFilesInWorkspace can
 * be cancelled early
 * - Change the process indicator to be finer
* [ee9384fe6fb7a88](https://github.com/spgennard/vscode_cobol/commit/ee9384fe6fb7a88) *2021-05-19 21:22:48*



**update**

* [1c6f4e1437402c9](https://github.com/spgennard/vscode_cobol/commit/1c6f4e1437402c9) *2021-05-18 19:20:10*

**Update CHANGELOG.md**

* [98ae406474e5022](https://github.com/spgennard/vscode_cobol/commit/98ae406474e5022) *2021-05-18 19:18:38*


## 7.5.18

**remove npe**

* [042414bf3d6b16b](https://github.com/spgennard/vscode_cobol/commit/042414bf3d6b16b) *2021-05-18 16:10:47*

**update**

* [c40d37715c5888d](https://github.com/spgennard/vscode_cobol/commit/c40d37715c5888d) *2021-05-18 16:05:14*

**Update CHANGELOG.md**

* [fbe16e28baf1ac0](https://github.com/spgennard/vscode_cobol/commit/fbe16e28baf1ac0) *2021-05-18 16:02:47*


## 7.5.17

**Update CHANGELOG.md**

* [99fcf59f3c624bd](https://github.com/spgennard/vscode_cobol/commit/99fcf59f3c624bd) *2021-05-18 15:59:42*

**don't change unless it a COBOL doc (ie: pp output)**

* [bd39204aefea630](https://github.com/spgennard/vscode_cobol/commit/bd39204aefea630) *2021-05-18 13:51:47*

**flip to COBOL for temp doc**

* [ad90ab7d981cb76](https://github.com/spgennard/vscode_cobol/commit/ad90ab7d981cb76) *2021-05-18 13:48:30*

**tidy up**

* [bc016e26bf4f4f4](https://github.com/spgennard/vscode_cobol/commit/bc016e26bf4f4f4) *2021-05-18 10:47:30*

**use range**

* [1fa50e3d2777d7d](https://github.com/spgennard/vscode_cobol/commit/1fa50e3d2777d7d) *2021-05-17 23:22:12*

**continue work on copybook handling**

* [74dbe6edbfc06a9](https://github.com/spgennard/vscode_cobol/commit/74dbe6edbfc06a9) *2021-05-17 21:48:13*

**only have word sub for the moment**

* [1395bc5921cac06](https://github.com/spgennard/vscode_cobol/commit/1395bc5921cac06) *2021-05-16 23:39:43*

**add a codelens for 'copy replacing' results**

* [ea376a8d2151a0e](https://github.com/spgennard/vscode_cobol/commit/ea376a8d2151a0e) *2021-05-16 20:48:42*

**start to wire in existing token'ed copy statement**

 * - fix start to be before &#x27;copy&#x27; verb
* [f0f864ab69411f7](https://github.com/spgennard/vscode_cobol/commit/f0f864ab69411f7) *2021-05-16 15:11:15*

**rename replace boolean, to allow it to include replace/replacing**

* [cb23430ae6b0cbb](https://github.com/spgennard/vscode_cobol/commit/cb23430ae6b0cbb) *2021-05-15 22:40:34*

**add range for copy statement**

* [592ea5e9d397f92](https://github.com/spgennard/vscode_cobol/commit/592ea5e9d397f92) *2021-05-15 20:40:05*

**make replace/replacing work better**

* [55dd15f1b39905f](https://github.com/spgennard/vscode_cobol/commit/55dd15f1b39905f) *2021-05-15 20:08:18*

**add lowercase cobol alias for the snippets**

* [075d24f21c6c8a1](https://github.com/spgennard/vscode_cobol/commit/075d24f21c6c8a1) *2021-05-14 07:45:23*

**move version forward**

* [2f747eb130676c5](https://github.com/spgennard/vscode_cobol/commit/2f747eb130676c5) *2021-05-13 22:12:11*

**tweak**

* [5dff120a67396e0](https://github.com/spgennard/vscode_cobol/commit/5dff120a67396e0) *2021-05-11 21:27:35*

**avoid security issue**

* [1f682ebe9c93738](https://github.com/spgennard/vscode_cobol/commit/1f682ebe9c93738) *2021-05-11 20:52:15*

**update**

* [82922fcd6d0816c](https://github.com/spgennard/vscode_cobol/commit/82922fcd6d0816c) *2021-05-11 20:48:01*

**continue work copybook handling**

* [632ccd0601d4c6f](https://github.com/spgennard/vscode_cobol/commit/632ccd0601d4c6f) *2021-05-09 21:13:52*

**fix navigation of "copy of.." but this mechanism might be short-lived..**

* [ee4fc15db462396](https://github.com/spgennard/vscode_cobol/commit/ee4fc15db462396) *2021-05-09 16:06:12*

**use only one style**

* [6ed3cae6d10a43d](https://github.com/spgennard/vscode_cobol/commit/6ed3cae6d10a43d) *2021-05-04 20:54:12*

**Update CHANGELOG.md**

* [c423be5df06a1d2](https://github.com/spgennard/vscode_cobol/commit/c423be5df06a1d2) *2021-05-03 17:57:13*


## 7.5.2

**prep**

* [b5ba4804b2fe5e7](https://github.com/spgennard/vscode_cobol/commit/b5ba4804b2fe5e7) *2021-05-03 17:56:38*

**add experimental replace verb passing under option**

* [e7745945e990cc8](https://github.com/spgennard/vscode_cobol/commit/e7745945e990cc8) *2021-05-03 16:09:16*

**try to handle mf compound entry-statements**

* [3dedc2aedc7fe4f](https://github.com/spgennard/vscode_cobol/commit/3dedc2aedc7fe4f) *2021-05-02 15:40:33*

**simplify code**

* [ec4673ed35eece5](https://github.com/spgennard/vscode_cobol/commit/ec4673ed35eece5) *2021-05-02 11:06:51*

**tidy up token handling**

* [d619a8bb2955e8d](https://github.com/spgennard/vscode_cobol/commit/d619a8bb2955e8d) *2021-05-01 23:45:45*

**add an initial impl of 'replace' functionality**

* [93b2a034801f4d6](https://github.com/spgennard/vscode_cobol/commit/93b2a034801f4d6) *2021-05-01 21:02:25*

**fix offset for 'fake' filler items**

* [71f14d0ebd82891](https://github.com/spgennard/vscode_cobol/commit/71f14d0ebd82891) *2021-05-01 07:30:05*

**continue work on tweaking startColumn handling**

* [ba57b300d565141](https://github.com/spgennard/vscode_cobol/commit/ba57b300d565141) *2021-04-30 22:28:13*

**start to change COBOLToken/Token to be performant**

* [753beb429294f10](https://github.com/spgennard/vscode_cobol/commit/753beb429294f10) *2021-04-30 19:09:37*

**fix bug with paragraphs and continue token/refactoring**

* [c8fa5989dcd1ecb](https://github.com/spgennard/vscode_cobol/commit/c8fa5989dcd1ecb) *2021-04-29 23:30:50*

**remove condition that is not required**

* [3ddf8759f44ac97](https://github.com/spgennard/vscode_cobol/commit/3ddf8759f44ac97) *2021-04-25 15:29:09*

**tweak token**

* [dc4c57d62efb506](https://github.com/spgennard/vscode_cobol/commit/dc4c57d62efb506) *2021-04-25 15:12:28*

**place the file system correctly**

* [131377b31c666d8](https://github.com/spgennard/vscode_cobol/commit/131377b31c666d8) *2021-04-24 21:58:52*

**ensure pp turns off parse_copybooks_for_references**

* [d596bfb77f4fa91](https://github.com/spgennard/vscode_cobol/commit/d596bfb77f4fa91) *2021-04-24 17:15:52*

**update**

* [e736c87f928027a](https://github.com/spgennard/vscode_cobol/commit/e736c87f928027a) *2021-04-24 16:55:54*

**create new doc on pp codelens**

* [838a20bdc2a3b17](https://github.com/spgennard/vscode_cobol/commit/838a20bdc2a3b17) *2021-04-24 16:40:23*

**add codelens for pp, so we can see easier what is happening**

* [d2f368f70a56abf](https://github.com/spgennard/vscode_cobol/commit/d2f368f70a56abf) *2021-04-24 09:10:09*

**use the copybook scan tokens**

* [49be90df5d118aa](https://github.com/spgennard/vscode_cobol/commit/49be90df5d118aa) *2021-04-18 21:22:58*

**warn about out of sync cache when debugging and don't reparse**

* [922d811f3029cb7](https://github.com/spgennard/vscode_cobol/commit/922d811f3029cb7) *2021-04-16 19:18:35*

**package update**

* [c2f6eaa82de8d54](https://github.com/spgennard/vscode_cobol/commit/c2f6eaa82de8d54) *2021-04-14 21:05:08*

**Update CHANGELOG.md**

* [3e26e8c465e3d35](https://github.com/spgennard/vscode_cobol/commit/3e26e8c465e3d35) *2021-04-14 21:02:55*


## 7.4.15

**tweak packages**

* [ac3cea06e3def24](https://github.com/spgennard/vscode_cobol/commit/ac3cea06e3def24) *2021-04-14 20:47:47*

**add linter_ignore_missing_copybook**

* [6e58a11d0625581](https://github.com/spgennard/vscode_cobol/commit/6e58a11d0625581) *2021-04-14 20:26:38*

**add linter_ignore_missing_copybooks config**

* [ee0adca0f99b613](https://github.com/spgennard/vscode_cobol/commit/ee0adca0f99b613) *2021-04-14 19:50:49*

**Update CHANGELOG.md**

* [ed46a2c6da2d19c](https://github.com/spgennard/vscode_cobol/commit/ed46a2c6da2d19c) *2021-04-13 21:55:48*


## 7.4.14

**use a good default, for ref type, ignore unknown style**

* [6fb33090b39dc73](https://github.com/spgennard/vscode_cobol/commit/6fb33090b39dc73) *2021-04-13 21:49:52*

**remove unused code**

* [973c3e43fcbf4ab](https://github.com/spgennard/vscode_cobol/commit/973c3e43fcbf4ab) *2021-04-13 21:29:48*

**continue work on references**

 * change $end-region to match $region
* [28d41ac613d5ce4](https://github.com/spgennard/vscode_cobol/commit/28d41ac613d5ce4) *2021-04-13 17:06:55*

**Update CHANGELOG.md**

* [c0e597565838351](https://github.com/spgennard/vscode_cobol/commit/c0e597565838351) *2021-04-12 22:32:09*


## 7.4.13

**performance tweaks**

* [3ed3fb1448ab90f](https://github.com/spgennard/vscode_cobol/commit/3ed3fb1448ab90f) *2021-04-12 22:29:11*

**prepare for delivery**

* [fc17b8d9fca2e56](https://github.com/spgennard/vscode_cobol/commit/fc17b8d9fca2e56) *2021-04-12 21:32:08*

**ignore warnings**

* [71fa75b3eda254d](https://github.com/spgennard/vscode_cobol/commit/71fa75b3eda254d) *2021-04-12 21:22:09*

**honour visability on references**

* [d6e5ea875d2c305](https://github.com/spgennard/vscode_cobol/commit/d6e5ea875d2c305) *2021-04-12 21:06:27*

**Update CHANGELOG.md**

* [3e4bf8d6c916cb1](https://github.com/spgennard/vscode_cobol/commit/3e4bf8d6c916cb1) *2021-04-12 20:14:53*


## 7.4.12

**continue work on forward references**

* [5d907713dc95a5b](https://github.com/spgennard/vscode_cobol/commit/5d907713dc95a5b) *2021-04-12 20:13:37*

**restrict picking up of forward references**

* [c719f11449f712d](https://github.com/spgennard/vscode_cobol/commit/c719f11449f712d) *2021-04-12 19:02:35*

**Update CHANGELOG.md**

* [d3258bfaf9e3265](https://github.com/spgennard/vscode_cobol/commit/d3258bfaf9e3265) *2021-04-11 21:23:44*


## 7.4.11

**tidy up**

* [0a50a985be0ef2a](https://github.com/spgennard/vscode_cobol/commit/0a50a985be0ef2a) *2021-04-11 20:24:51*

**ensure we don't get duplicate interface, enums in the cache**

* [430c3d25811f8a3](https://github.com/spgennard/vscode_cobol/commit/430c3d25811f8a3) *2021-04-11 20:20:22*

**tweak $set and tidy up COBOL/Messages on startup**

* [3b3a8fb9a86f61c](https://github.com/spgennard/vscode_cobol/commit/3b3a8fb9a86f61c) *2021-04-11 19:56:28*

**more updates for $set and readonly semantic colouring**

* [533c8bca06bfdc4](https://github.com/spgennard/vscode_cobol/commit/533c8bca06bfdc4) *2021-04-11 11:39:25*

**pickup variables for references in value clauses**

* [4f0395372233dd5](https://github.com/spgennard/vscode_cobol/commit/4f0395372233dd5) *2021-04-10 22:20:17*

**Update CHANGELOG.md**

* [9478b604bbd5a8a](https://github.com/spgennard/vscode_cobol/commit/9478b604bbd5a8a) *2021-04-10 20:36:05*


## 7.4.10

**prep for delivery**

* [d6d80b609c90066](https://github.com/spgennard/vscode_cobol/commit/d6d80b609c90066) *2021-04-10 20:35:37*

**remove unused var**

* [816bfcb6539ca96](https://github.com/spgennard/vscode_cobol/commit/816bfcb6539ca96) *2021-04-10 20:22:49*

**remove duplicate find from source definition when "call xx" is found**

* [e3ff120999928c9](https://github.com/spgennard/vscode_cobol/commit/e3ff120999928c9) *2021-04-10 20:20:29*

**Update CHANGELOG.md**

* [14000fac19d8502](https://github.com/spgennard/vscode_cobol/commit/14000fac19d8502) *2021-04-08 22:58:51*


## 7.4.8

**comment out unused lines**

* [d1c974eb1ac064a](https://github.com/spgennard/vscode_cobol/commit/d1c974eb1ac064a) *2021-04-08 22:58:19*

**Update CHANGELOG.md**

* [67ef83e93a293fc](https://github.com/spgennard/vscode_cobol/commit/67ef83e93a293fc) *2021-04-08 22:55:29*

**add extra diag messages**

* [28463c6608dcdb3](https://github.com/spgennard/vscode_cobol/commit/28463c6608dcdb3) *2021-04-08 22:55:00*

**delivery it**

* [6f9730f74eedce6](https://github.com/spgennard/vscode_cobol/commit/6f9730f74eedce6) *2021-04-08 22:16:23*

**add warning message for untested environments**

* [47511c466e974cf](https://github.com/spgennard/vscode_cobol/commit/47511c466e974cf) *2021-04-08 22:15:56*

**ensure OVSX envs are not set**

* [b140ac573d04970](https://github.com/spgennard/vscode_cobol/commit/b140ac573d04970) *2021-04-08 21:37:07*

**update**

* [c070aa4d1b8bddc](https://github.com/spgennard/vscode_cobol/commit/c070aa4d1b8bddc) *2021-04-08 20:53:56*

**tidy and add a warning message**

* [6a0d01881c74516](https://github.com/spgennard/vscode_cobol/commit/6a0d01881c74516) *2021-04-08 18:32:50*

**don't blank but set it something bad**

* [85933ffacd596f9](https://github.com/spgennard/vscode_cobol/commit/85933ffacd596f9) *2021-04-07 21:07:15*

**tweak package.json to ensure publisher is set before hand**

* [93aa195fe6850a2](https://github.com/spgennard/vscode_cobol/commit/93aa195fe6850a2) *2021-04-07 21:04:48*

**add pre-commit hooks**

* [7660bb2dbea8126](https://github.com/spgennard/vscode_cobol/commit/7660bb2dbea8126) *2021-04-07 21:00:51*

**add pre-commit hook**

* [cdf164f3cf54263](https://github.com/spgennard/vscode_cobol/commit/cdf164f3cf54263) *2021-04-07 20:56:19*

**Update CHANGELOG.md**

* [872bdf248d15729](https://github.com/spgennard/vscode_cobol/commit/872bdf248d15729) *2021-04-07 20:35:47*


## 7.4.7

**prep**

* [4e212a42d5f5faf](https://github.com/spgennard/vscode_cobol/commit/4e212a42d5f5faf) *2021-04-07 20:34:23*

**Update CHANGELOG.md**

* [2266e219c4be578](https://github.com/spgennard/vscode_cobol/commit/2266e219c4be578) *2021-04-07 20:00:49*

**wire up**

* [995db58506cb220](https://github.com/spgennard/vscode_cobol/commit/995db58506cb220) *2021-04-07 20:00:07*

**prepare for delivery**

* [ecb9561f2365de3](https://github.com/spgennard/vscode_cobol/commit/ecb9561f2365de3) *2021-04-07 19:51:31*

**add some support for references in level 66 lines**

* [d54f82131d9b27a](https://github.com/spgennard/vscode_cobol/commit/d54f82131d9b27a) *2021-04-07 17:50:00*

**version updates**

* [94adc043512acdd](https://github.com/spgennard/vscode_cobol/commit/94adc043512acdd) *2021-04-06 21:40:40*

**Update CHANGELOG.md**

* [6eb6ca8f4e917d0](https://github.com/spgennard/vscode_cobol/commit/6eb6ca8f4e917d0) *2021-04-06 21:35:35*


## 7.4.6

**continue work on references & prepare del**

* [63192c2c40666c4](https://github.com/spgennard/vscode_cobol/commit/63192c2c40666c4) *2021-04-06 21:34:26*

**include schema and fix bugs shown up**

* [426c34961276862](https://github.com/spgennard/vscode_cobol/commit/426c34961276862) *2021-04-01 16:19:06*

**relax string to allow space for name elements**

* [f21fc6770c8f74c](https://github.com/spgennard/vscode_cobol/commit/f21fc6770c8f74c) *2021-04-01 16:15:02*

**tweaks**

* [f30cd82aa4e13f7](https://github.com/spgennard/vscode_cobol/commit/f30cd82aa4e13f7) *2021-04-01 15:56:58*

**add schema**

* [533b802b3613dd4](https://github.com/spgennard/vscode_cobol/commit/533b802b3613dd4) *2021-04-01 15:44:01*

**Update CHANGELOG.md**

* [ae683af882412f9](https://github.com/spgennard/vscode_cobol/commit/ae683af882412f9) *2021-03-29 22:49:29*


## 7.3.30

**try to make the pp activation more robust**

* [acf35cd52cb98a1](https://github.com/spgennard/vscode_cobol/commit/acf35cd52cb98a1) *2021-03-29 22:39:58*

**add late sourcereference cat, as it is not a 2 pass scanner**

* [5e97ee306462fc1](https://github.com/spgennard/vscode_cobol/commit/5e97ee306462fc1) *2021-03-29 20:39:32*

**update packages**

* [60bbfd49b69a8fc](https://github.com/spgennard/vscode_cobol/commit/60bbfd49b69a8fc) *2021-03-29 16:34:39*

**Update CHANGELOG.md**

* [408b29b94bce0e7](https://github.com/spgennard/vscode_cobol/commit/408b29b94bce0e7) *2021-03-29 16:27:12*


## 7.3.29

**prepare for delivery**

 * - only say the pp failed when it has fatally failed &amp; log it
* [edb7a09303fb560](https://github.com/spgennard/vscode_cobol/commit/edb7a09303fb560) *2021-03-29 16:25:30*

**re-enable "decl" section processing (hopefull does not break anything)**

 * tweak setting of fixed format
* [414647c7f576b7a](https://github.com/spgennard/vscode_cobol/commit/414647c7f576b7a) *2021-03-29 16:13:07*

**fix problem with margin due to duplicated code**

* [7889b579f8b3a17](https://github.com/spgennard/vscode_cobol/commit/7889b579f8b3a17) *2021-03-29 13:28:05*

**package update**

* [0dbe716214265ad](https://github.com/spgennard/vscode_cobol/commit/0dbe716214265ad) *2021-03-28 09:17:05*

**Update CHANGELOG.md**

* [78bd21d809298ed](https://github.com/spgennard/vscode_cobol/commit/78bd21d809298ed) *2021-03-28 09:05:00*


## 7.3.28

**make the determination of fixed file format better**

 * don&#x27;t register interest in tokens that are literals
 * (long standing silent bug)
* [c2a6a3b2d8de0e8](https://github.com/spgennard/vscode_cobol/commit/c2a6a3b2d8de0e8) *2021-03-27 17:17:09*

**Update CHANGELOG.md**

* [a6bea1e5e193a2a](https://github.com/spgennard/vscode_cobol/commit/a6bea1e5e193a2a) *2021-03-27 14:30:36*


## 7.3.27

**add some bullet proofing**

* [ffd5a5010abff8c](https://github.com/spgennard/vscode_cobol/commit/ffd5a5010abff8c) *2021-03-27 14:30:00*

**expand sourceref to include tokentype**

* [a66f91d67af0a4f](https://github.com/spgennard/vscode_cobol/commit/a66f91d67af0a4f) *2021-03-27 12:33:17*

**add variables into semantic provider & config option**

* [2eeb7bfd63b6130](https://github.com/spgennard/vscode_cobol/commit/2eeb7bfd63b6130) *2021-03-27 10:29:46*

**add the section/paragrams as "label"/declaration semantic tokens**

* [99fbe95c459faf6](https://github.com/spgennard/vscode_cobol/commit/99fbe95c459faf6) *2021-03-27 01:15:10*

**make it safer**

* [d23aad6f8ef8633](https://github.com/spgennard/vscode_cobol/commit/d23aad6f8ef8633) *2021-03-26 20:00:22*

**drop alias**

* [e0b6825b61df2ff](https://github.com/spgennard/vscode_cobol/commit/e0b6825b61df2ff) *2021-03-26 19:58:26*

**tweak logic on how to flip to the COBOL lang id**

* [d910312de183a85](https://github.com/spgennard/vscode_cobol/commit/d910312de183a85) *2021-03-26 18:51:53*

**Update CHANGELOG.md**

* [6ccf760a62c0fad](https://github.com/spgennard/vscode_cobol/commit/6ccf760a62c0fad) *2021-03-26 18:05:39*


## 7.3.26

**resort to changing the document type rather than having an alias lang**

* [193083ec12e3804](https://github.com/spgennard/vscode_cobol/commit/193083ec12e3804) *2021-03-26 18:04:39*

**remove aliases and ensure enablements are correct**

* [2955204dac02728](https://github.com/spgennard/vscode_cobol/commit/2955204dac02728) *2021-03-26 17:02:54*

**package updates**

* [5e89ce0f1e841be](https://github.com/spgennard/vscode_cobol/commit/5e89ce0f1e841be) *2021-03-26 09:23:14*

**Update CHANGELOG.md**

* [2ee8521b9842643](https://github.com/spgennard/vscode_cobol/commit/2ee8521b9842643) *2021-03-26 09:13:08*

**update readme:**

* [b542a9c8362a23e](https://github.com/spgennard/vscode_cobol/commit/b542a9c8362a23e) *2021-03-25 21:34:18*

**add some control about how a "single folder" workspace is updated**

* [10bc18869f248d2](https://github.com/spgennard/vscode_cobol/commit/10bc18869f248d2) *2021-03-25 08:42:05*

**Update CHANGELOG.md**

* [2ac7f891360c290](https://github.com/spgennard/vscode_cobol/commit/2ac7f891360c290) *2021-03-25 07:57:04*


## 7.3.25

**update**

* [56e88037ca3beff](https://github.com/spgennard/vscode_cobol/commit/56e88037ca3beff) *2021-03-25 07:56:29*

**Update CHANGELOG.md**

* [12774ff593bc5ab](https://github.com/spgennard/vscode_cobol/commit/12774ff593bc5ab) *2021-03-24 21:48:05*


## 7.3.24

**update**

* [38b11be6778dba7](https://github.com/spgennard/vscode_cobol/commit/38b11be6778dba7) *2021-03-24 21:46:49*

**Update CHANGELOG.md**

* [37e9d91624eab6b](https://github.com/spgennard/vscode_cobol/commit/37e9d91624eab6b) *2021-03-24 21:45:01*


## 7.3.24.1
### GitHub [#270](https://github.com/spgennard/vscode_cobol/issues/270) 7.3.23 is forcibly changing &#x60;.vscode/settings.json&#x60;  

**Tweak update() to happen on a workspace and if enabled Fix #270**

* [bd5c88b913911a8](https://github.com/spgennard/vscode_cobol/commit/bd5c88b913911a8) *2021-03-24 18:22:42*



**tweaks**

* [1488097c331263a](https://github.com/spgennard/vscode_cobol/commit/1488097c331263a) *2021-03-24 21:44:24*

**use of support.variable.cobol, was too gready, it colourised too much**

 * remove margin statusbar as it is pointless
* [fc054d1a70a6d93](https://github.com/spgennard/vscode_cobol/commit/fc054d1a70a6d93) *2021-03-24 21:05:16*

**protect update and log exception**

* [c3c3d035b58b424](https://github.com/spgennard/vscode_cobol/commit/c3c3d035b58b424) *2021-03-24 18:57:05*

**Update CHANGELOG.md**

* [87ec5038aaee75a](https://github.com/spgennard/vscode_cobol/commit/87ec5038aaee75a) *2021-03-24 18:38:49*

**prep**

* [cfaac98c7158747](https://github.com/spgennard/vscode_cobol/commit/cfaac98c7158747) *2021-03-24 18:37:57*

**enusre alias "cobol" works as well as "COBOL"**

* [3d00b2ab363df3d](https://github.com/spgennard/vscode_cobol/commit/3d00b2ab363df3d) *2021-03-24 17:30:39*

**Update CHANGELOG.md**

* [09ae55eae068fae](https://github.com/spgennard/vscode_cobol/commit/09ae55eae068fae) *2021-03-23 23:35:39*


## 7.3.23

**Update CHANGELOG.md**

* [e00b9ea3c0c5b5d](https://github.com/spgennard/vscode_cobol/commit/e00b9ea3c0c5b5d) *2021-03-23 20:12:38*

**prep for delivery**

* [927df70963b3ebf](https://github.com/spgennard/vscode_cobol/commit/927df70963b3ebf) *2021-03-23 20:12:05*

**avoid using meta.symbol.cobol, as it is not friendly to some themes**

 * add config for alias, simplify lang use
* [c7491479d1395ea](https://github.com/spgennard/vscode_cobol/commit/c7491479d1395ea) *2021-03-23 18:05:24*

**add lowercase cobol, so "alias" is configure the same way as the**

 * uppercase one
* [ef3a4071c43f405](https://github.com/spgennard/vscode_cobol/commit/ef3a4071c43f405) *2021-03-21 16:19:39*

**remove experimental_features, as it is not currently used**

* [2e6e168c8e8d85e](https://github.com/spgennard/vscode_cobol/commit/2e6e168c8e8d85e) *2021-03-20 23:00:44*

**(from experimental feature)**

 * - drop syntax task, as it does not work as good as I hoped it would
* [959b45f7455daa0](https://github.com/spgennard/vscode_cobol/commit/959b45f7455daa0) *2021-03-20 22:09:31*

**restrict commands to the right focus time**

* [3a80f438bbac038](https://github.com/spgennard/vscode_cobol/commit/3a80f438bbac038) *2021-03-20 12:01:20*

**fix enablement of command**

* [722fea3983ad6df](https://github.com/spgennard/vscode_cobol/commit/722fea3983ad6df) *2021-03-20 11:55:29*

**a little tidy up**

* [ac106e7eefed71b](https://github.com/spgennard/vscode_cobol/commit/ac106e7eefed71b) *2021-03-18 23:12:57*

**remove unused code**

* [1a774f69b45e97a](https://github.com/spgennard/vscode_cobol/commit/1a774f69b45e97a) *2021-03-18 22:59:32*

**remove unused flags**

* [12092b6dab15a55](https://github.com/spgennard/vscode_cobol/commit/12092b6dab15a55) *2021-03-18 22:03:46*

**package updates**

* [efc4695015392da](https://github.com/spgennard/vscode_cobol/commit/efc4695015392da) *2021-03-18 00:54:50*

**Update CHANGELOG.md**

* [0557fd3c3fce459](https://github.com/spgennard/vscode_cobol/commit/0557fd3c3fce459) *2021-03-18 00:52:10*


## 7.3.18
### GitHub [#268](https://github.com/spgennard/vscode_cobol/issues/268) Syntax highligh wrong for &#x60;WHEN 78 MOVE&#x60;  

**Syntax highligh wrong for `WHEN 78 MOVE` Fix #268**

* [983da5c6f0b67f5](https://github.com/spgennard/vscode_cobol/commit/983da5c6f0b67f5) *2021-03-05 21:35:38*



**comment out step**

* [600923d7470bc69](https://github.com/spgennard/vscode_cobol/commit/600923d7470bc69) *2021-03-18 00:51:27*

**Update CHANGELOG.md**

* [514c4565ebd4916](https://github.com/spgennard/vscode_cobol/commit/514c4565ebd4916) *2021-03-18 00:49:20*

**prep**

* [50a1ce945d69316](https://github.com/spgennard/vscode_cobol/commit/50a1ce945d69316) *2021-03-18 00:48:47*

**remove docs for depreciated caching**

* [cbf09e63d19b5e7](https://github.com/spgennard/vscode_cobol/commit/cbf09e63d19b5e7) *2021-03-17 21:28:41*

**allow metadata caching to be turned off**

* [e5efa673f81e45d](https://github.com/spgennard/vscode_cobol/commit/e5efa673f81e45d) *2021-03-17 21:13:46*

**tidy up**

* [6d650c54d7a06d3](https://github.com/spgennard/vscode_cobol/commit/6d650c54d7a06d3) *2021-03-16 22:29:57*

**add "of" support to copybook open**

* [0797b896119a5db](https://github.com/spgennard/vscode_cobol/commit/0797b896119a5db) *2021-03-16 21:40:08*

**add support for hp cobol "COPY text-name [OF|IN] library-name"**

* [e3465c60e39fda9](https://github.com/spgennard/vscode_cobol/commit/e3465c60e39fda9) *2021-03-16 21:24:26*

**add version number**

* [7c820bf4a199555](https://github.com/spgennard/vscode_cobol/commit/7c820bf4a199555) *2021-03-15 18:10:20*

**update deps & tidy**

* [ada4c833f064384](https://github.com/spgennard/vscode_cobol/commit/ada4c833f064384) *2021-03-14 23:55:50*

**use same terminology**

* [9ae9b82ebae3240](https://github.com/spgennard/vscode_cobol/commit/9ae9b82ebae3240) *2021-03-14 21:46:00*

**make execute/debug submenus**

* [726204d32b36621](https://github.com/spgennard/vscode_cobol/commit/726204d32b36621) *2021-03-14 21:19:32*

**add debug submenu**

* [054abd25fbbfbb0](https://github.com/spgennard/vscode_cobol/commit/054abd25fbbfbb0) *2021-03-14 20:47:25*

**fix bug in sourceview**

* [40a6c44f0eaeb13](https://github.com/spgennard/vscode_cobol/commit/40a6c44f0eaeb13) *2021-03-14 18:26:56*

**do not process anything until all pp are ready**

* [6c48cc97cefc140](https://github.com/spgennard/vscode_cobol/commit/6c48cc97cefc140) *2021-03-14 00:12:45*

**wire in the global sym file cleanup code**

* [cc6b77cda11da71](https://github.com/spgennard/vscode_cobol/commit/cc6b77cda11da71) *2021-03-13 18:45:26*

**add objects (untested)**

* [ff6210c4c474b9c](https://github.com/spgennard/vscode_cobol/commit/ff6210c4c474b9c) *2021-03-13 09:56:14*

**continue work with pp**

* [75fb1c39ccaba1e](https://github.com/spgennard/vscode_cobol/commit/75fb1c39ccaba1e) *2021-03-13 09:31:12*

**show pp info after the pp has been activated**

* [b432d1c87fe811f](https://github.com/spgennard/vscode_cobol/commit/b432d1c87fe811f) *2021-03-09 23:41:49*

**inc release number**

* [83991c4a8990186](https://github.com/spgennard/vscode_cobol/commit/83991c4a8990186) *2021-03-09 23:30:10*

**add some diagnostics for the pp into the output channel on startup**

* [80e658c36013e47](https://github.com/spgennard/vscode_cobol/commit/80e658c36013e47) *2021-03-09 23:28:35*

**fix serialisation issue with bigint**

* [2cda4ad46fc9ad7](https://github.com/spgennard/vscode_cobol/commit/2cda4ad46fc9ad7) *2021-03-09 18:50:54*

**continue background work on workspace caching**

 * - make workspace entries portable
* [c42fdcea60a2857](https://github.com/spgennard/vscode_cobol/commit/c42fdcea60a2857) *2021-03-08 23:50:54*

**don't need this now**

* [b6adafc4c50182d](https://github.com/spgennard/vscode_cobol/commit/b6adafc4c50182d) *2021-03-06 22:40:35*

**move to using one obj that will eventually contain portable/non-portable**

 * filename information
* [f8f5fa410b0ce34](https://github.com/spgennard/vscode_cobol/commit/f8f5fa410b0ce34) *2021-03-06 21:52:01*

**continue work on workspace cache**

* [8bcd74c96346699](https://github.com/spgennard/vscode_cobol/commit/8bcd74c96346699) *2021-03-06 13:02:45*

**retry on extension lookup, as it maybe still starting up**

* [d4fac48ff6e3ef9](https://github.com/spgennard/vscode_cobol/commit/d4fac48ff6e3ef9) *2021-03-05 22:25:22*

**move to using bigint stats**

* [a8681fe865ef6d9](https://github.com/spgennard/vscode_cobol/commit/a8681fe865ef6d9) *2021-03-05 00:54:32*

**add in the file cache**

* [5852fe23317a57d](https://github.com/spgennard/vscode_cobol/commit/5852fe23317a57d) *2021-03-04 22:28:17*

**start to add in the filename cache**

* [db3dd1e249102f5](https://github.com/spgennard/vscode_cobol/commit/db3dd1e249102f5) *2021-03-03 23:59:36*

**use a temp area if no cache directory is available**

* [648f259e15e3125](https://github.com/spgennard/vscode_cobol/commit/648f259e15e3125) *2021-03-03 23:08:56*

**start the process of re-introducing the workspace external process cache**

 * - does not work at the moment :-(
* [5e62e0e3667a84c](https://github.com/spgennard/vscode_cobol/commit/5e62e0e3667a84c) *2021-03-03 08:52:10*

**continue the renaming**

* [b5f7b7e910b13bf](https://github.com/spgennard/vscode_cobol/commit/b5f7b7e910b13bf) *2021-03-02 22:54:28*

**rename command to make it clear it is deprecated**

* [e756e207888dc49](https://github.com/spgennard/vscode_cobol/commit/e756e207888dc49) *2021-03-02 22:50:32*

**move command to cobolplugin.deprecated.processAllFilesInWorkspace**

* [a4565fd764e3b88](https://github.com/spgennard/vscode_cobol/commit/a4565fd764e3b88) *2021-03-02 22:48:09*

**add missing file**

* [a27bdde72719749](https://github.com/spgennard/vscode_cobol/commit/a27bdde72719749) *2021-03-02 10:10:27*

**move to new pp activation model**

* [e4e9180cb207e01](https://github.com/spgennard/vscode_cobol/commit/e4e9180cb207e01) *2021-03-01 23:20:29*

**add missing method**

* [1ff9efcc0f9e0de](https://github.com/spgennard/vscode_cobol/commit/1ff9efcc0f9e0de) *2021-03-01 19:19:49*

**start to rework the pp to be enabled by a list of extensions,**

 * hopefully allow them to be full active at the same time as this ext.
* [ac1a4a39880c1c3](https://github.com/spgennard/vscode_cobol/commit/ac1a4a39880c1c3) *2021-03-01 01:13:36*

**handle in version num**

* [e5bf5bb58168336](https://github.com/spgennard/vscode_cobol/commit/e5bf5bb58168336) *2021-02-28 21:26:51*

**update to include sdk.zip in build**

* [456c2eb5de00e69](https://github.com/spgennard/vscode_cobol/commit/456c2eb5de00e69) *2021-02-28 10:47:06*

**continue work on pp**

 * - alter start to include packageJson of package registering
 * so more detailed error handling/information can be generated
 * later
 * - add callback interface for extra info used while pp&#x27;ing
* [92f33162051180b](https://github.com/spgennard/vscode_cobol/commit/92f33162051180b) *2021-02-27 22:58:42*

**couple of teaks for the acucobol dialect**

* [81910bf458aa206](https://github.com/spgennard/vscode_cobol/commit/81910bf458aa206) *2021-02-26 23:03:19*

**flip a couple of things out of experimental status**

* [f8285bd22704849](https://github.com/spgennard/vscode_cobol/commit/f8285bd22704849) *2021-02-26 22:37:27*

**add file-control as a end-del for a remark**

* [5ed0348ff0a311b](https://github.com/spgennard/vscode_cobol/commit/5ed0348ff0a311b) *2021-02-26 21:34:22*

**optimise use of settings obj**

* [dceae0ae7c5fdc5](https://github.com/spgennard/vscode_cobol/commit/dceae0ae7c5fdc5) *2021-02-25 23:21:18*

**add version id, for better comparison, use ms for files**

* [e9df9bbe5fb0ee1](https://github.com/spgennard/vscode_cobol/commit/e9df9bbe5fb0ee1) *2021-02-25 23:06:21*

**add support for handling a external file system for gotodef (alias of**

 * prg-id)
* [88fa796a871cfc9](https://github.com/spgennard/vscode_cobol/commit/88fa796a871cfc9) *2021-02-24 22:58:40*

**remove message & remove unused field**

* [c34679314f09e12](https://github.com/spgennard/vscode_cobol/commit/c34679314f09e12) *2021-02-24 08:25:52*

**only clear specific parts of the cache**

* [3a8ce26caaa1cb9](https://github.com/spgennard/vscode_cobol/commit/3a8ce26caaa1cb9) *2021-02-24 08:14:41*

**reduce refresh rate of config update**

* [5cb0296b11935d3](https://github.com/spgennard/vscode_cobol/commit/5cb0296b11935d3) *2021-02-24 01:16:56*

**add clear internal memory cache cmd**

 * continue work on pp
* [f6e25eb5a45d889](https://github.com/spgennard/vscode_cobol/commit/f6e25eb5a45d889) *2021-02-24 00:32:13*

**add crudge type back in**

* [752b62a9ffd7e2a](https://github.com/spgennard/vscode_cobol/commit/752b62a9ffd7e2a) *2021-02-21 22:16:51*

**add more scanner callbacks**

* [3e75ff4fc700a6e](https://github.com/spgennard/vscode_cobol/commit/3e75ff4fc700a6e) *2021-02-21 22:01:36*

**add types**

* [27f26f38f718f22](https://github.com/spgennard/vscode_cobol/commit/27f26f38f718f22) *2021-02-21 20:54:45*

**fix bug in use of config.**

* [ba72cca006aeff0](https://github.com/spgennard/vscode_cobol/commit/ba72cca006aeff0) *2021-02-20 23:08:03*

**add more pp support**

 * add cross process support for ep/prg&#x27;s to scanner
* [e9257cbb298008a](https://github.com/spgennard/vscode_cobol/commit/e9257cbb298008a) *2021-02-20 23:02:00*

**continue tweaking the pp interface**

* [c45a91d56b11e6a](https://github.com/spgennard/vscode_cobol/commit/c45a91d56b11e6a) *2021-02-19 00:22:26*

**add more of the pp interface**

* [141de11f75fe099](https://github.com/spgennard/vscode_cobol/commit/141de11f75fe099) *2021-02-18 00:06:51*

**continue work on pp**

* [ae8219c29389a98](https://github.com/spgennard/vscode_cobol/commit/ae8219c29389a98) *2021-02-17 21:23:16*

**put a crudge process method in place**

* [61e61c7552d2f37](https://github.com/spgennard/vscode_cobol/commit/61e61c7552d2f37) *2021-02-17 00:57:40*

**start to put a framework in place for a external preprocessor interface**

* [9328c33ae1b75f5](https://github.com/spgennard/vscode_cobol/commit/9328c33ae1b75f5) *2021-02-16 22:43:07*

**change name to cob*api*ts**

* [798ca9c67c78f02](https://github.com/spgennard/vscode_cobol/commit/798ca9c67c78f02) *2021-02-16 20:32:48*

**start the api experiment**

* [ae82ce969849563](https://github.com/spgennard/vscode_cobol/commit/ae82ce969849563) *2021-02-16 19:39:32*

**continue breaking out the globals**

* [b64e943dcb48469](https://github.com/spgennard/vscode_cobol/commit/b64e943dcb48469) *2021-02-16 08:03:50*

**add simple call/cancel targets**

* [429739d9af5633d](https://github.com/spgennard/vscode_cobol/commit/429739d9af5633d) *2021-02-15 00:10:18*

**start to wrire back in ep data into the provider**

* [1fce6b3383352e5](https://github.com/spgennard/vscode_cobol/commit/1fce6b3383352e5) *2021-02-14 23:35:18*

**load ep data on activation**

* [693d848b82cd433](https://github.com/spgennard/vscode_cobol/commit/693d848b82cd433) *2021-02-14 23:28:45*

**remvoe all entry-point symbols before parsing program**

* [1e6e2fa26ca1028](https://github.com/spgennard/vscode_cobol/commit/1e6e2fa26ca1028) *2021-02-14 23:07:37*

**add more of the entrypoint support**

* [75b8a52d1dbcfdf](https://github.com/spgennard/vscode_cobol/commit/75b8a52d1dbcfdf) *2021-02-14 21:40:29*

**tidy up code**

* [2e028e974d170a4](https://github.com/spgennard/vscode_cobol/commit/2e028e974d170a4) *2021-02-14 18:42:28*

**move external sym providers into sep file`**

* [a39083750693591](https://github.com/spgennard/vscode_cobol/commit/a39083750693591) *2021-02-14 16:23:10*

**continue to remove references to the non-file sym files**

* [c98fed423437da1](https://github.com/spgennard/vscode_cobol/commit/c98fed423437da1) *2021-02-14 14:33:08*

**wip - control work workspace metadata**

* [44029b614d1eb0c](https://github.com/spgennard/vscode_cobol/commit/44029b614d1eb0c) *2021-02-14 13:56:33*

**add symbol load/setup**

* [2cfaffad681c80c](https://github.com/spgennard/vscode_cobol/commit/2cfaffad681c80c) *2021-02-13 20:58:59*

**start to re-wire the symbol use**

* [40ed5a3703c3f36](https://github.com/spgennard/vscode_cobol/commit/40ed5a3703c3f36) *2021-02-13 16:37:05*

**not ready but don't want to loose it**

* [d10782b106ccf98](https://github.com/spgennard/vscode_cobol/commit/d10782b106ccf98) *2021-02-13 13:07:11*

**continue work moving to workspace move**

* [b250adb9c097649](https://github.com/spgennard/vscode_cobol/commit/b250adb9c097649) *2021-02-13 13:05:50*

**pass symbols into scanner**

* [e58fab8a07a235d](https://github.com/spgennard/vscode_cobol/commit/e58fab8a07a235d) *2021-02-11 22:16:34*

**create metadata area in workspace for global symbols**

* [1798cbc6f376430](https://github.com/spgennard/vscode_cobol/commit/1798cbc6f376430) *2021-02-10 23:20:24*

**move to next month**

* [612e736d662af91](https://github.com/spgennard/vscode_cobol/commit/612e736d662af91) *2021-02-10 09:02:49*

**just incase commit**

* [ac4ebc8e1dffe75](https://github.com/spgennard/vscode_cobol/commit/ac4ebc8e1dffe75) *2021-02-10 09:02:11*

**add onEnterRules for call/evaluate**

 * remove experimental example from readme, as it is not present
* [6d4ec8acd856414](https://github.com/spgennard/vscode_cobol/commit/6d4ec8acd856414) *2021-02-06 20:27:58*

**Update CHANGELOG.md**

* [4202bf784dff20b](https://github.com/spgennard/vscode_cobol/commit/4202bf784dff20b) *2021-02-02 23:47:39*


## 7.1.1
### GitHub [#266](https://github.com/spgennard/vscode_cobol/issues/266) paragraphs missing in outline  

**paragraphs missing in outline Fix #266**

 * - parsing of comment lines via the scanner internals was changing
 * the behavour on multi-line scanning
* [e75e75ce1104748](https://github.com/spgennard/vscode_cobol/commit/e75e75ce1104748) *2021-02-02 23:46:28*



**Update CHANGELOG.md**

* [096e69f0584f0b6](https://github.com/spgennard/vscode_cobol/commit/096e69f0584f0b6) *2021-02-01 22:30:28*


## 7.1.0

**introduce cobolit as a seperate language**

* [9c0f1b27f819540](https://github.com/spgennard/vscode_cobol/commit/9c0f1b27f819540) *2021-02-01 22:29:39*

**Update CHANGELOG.md**

* [ff3fc85af4e332c](https://github.com/spgennard/vscode_cobol/commit/ff3fc85af4e332c) *2021-01-22 23:58:31*


## 7.0.13

**Update CHANGELOG.md**

* [45dd4c967e28b82](https://github.com/spgennard/vscode_cobol/commit/45dd4c967e28b82) *2021-01-22 23:53:05*

**fix script**

* [ee4154f87f5ee1c](https://github.com/spgennard/vscode_cobol/commit/ee4154f87f5ee1c) *2021-01-22 23:52:44*

**update vscode engine**

* [d0c97428a433000](https://github.com/spgennard/vscode_cobol/commit/d0c97428a433000) *2021-01-22 19:51:57*

**update packages, remove html parser as it is not used (was for the**

 * failed coboldoc support)
* [5bd399ab925a3fe](https://github.com/spgennard/vscode_cobol/commit/5bd399ab925a3fe) *2021-01-22 19:48:34*

**add gen_third_party.sh to the ignore file**

* [d36d037f56db90b](https://github.com/spgennard/vscode_cobol/commit/d36d037f56db90b) *2021-01-22 19:43:41*

**add third party notices**

* [d8e03d1011a0564](https://github.com/spgennard/vscode_cobol/commit/d8e03d1011a0564) *2021-01-22 18:42:40*

**Move away from using uuid, as it seems to cause a clash on Windows,**

 * other platforms are okay.
* [e5c4d9365b465fe](https://github.com/spgennard/vscode_cobol/commit/e5c4d9365b465fe) *2021-01-22 18:21:55*


## 7.0.12

**Update CHANGELOG.md**

* [bef35f968d337d9](https://github.com/spgennard/vscode_cobol/commit/bef35f968d337d9) *2021-01-21 22:15:13*

**add in missing acucobol**

* [0989dd45e76c550](https://github.com/spgennard/vscode_cobol/commit/0989dd45e76c550) *2021-01-21 21:44:08*


## 7.0.11

**Update CHANGELOG.md**

* [2d4b18de951b018](https://github.com/spgennard/vscode_cobol/commit/2d4b18de951b018) *2021-01-20 22:44:18*

**tidy ignore extra .md**

* [6eaef5c942053b2](https://github.com/spgennard/vscode_cobol/commit/6eaef5c942053b2) *2021-01-20 22:29:14*

**tidy up build**

* [138ff44f03e6366](https://github.com/spgennard/vscode_cobol/commit/138ff44f03e6366) *2021-01-20 22:27:06*

**tweak the format**

* [f4a73c1276d25f7](https://github.com/spgennard/vscode_cobol/commit/f4a73c1276d25f7) *2021-01-20 22:20:19*

**update**

* [59f6b7865a13ee8](https://github.com/spgennard/vscode_cobol/commit/59f6b7865a13ee8) *2021-01-20 22:07:17*

**tweak**

* [14bce46d1b590a2](https://github.com/spgennard/vscode_cobol/commit/14bce46d1b590a2) *2021-01-20 22:06:34*


## 7.0.10
### GitHub [#138](https://github.com/spgennard/vscode_cobol/issues/138) FR: option to open CALLed programs  

**Avoid picking up "entry" as a entry point - #138**

* [9eb4b9960c5f0e5](https://github.com/spgennard/vscode_cobol/commit/9eb4b9960c5f0e5) *2021-01-16 11:29:39*


### GitHub [#254](https://github.com/spgennard/vscode_cobol/issues/254) dump metadata to file  

**dump metadata to file Fix #254**

* [5351afeaca81967](https://github.com/spgennard/vscode_cobol/commit/5351afeaca81967) *2021-01-16 20:35:36*


### GitHub [#255](https://github.com/spgennard/vscode_cobol/issues/255) Metadata parsing misses some sections  

**Metadata parsing misses some sections #255**

 * - tweak symbol catcher and event generator
* [920b10ebc649c44](https://github.com/spgennard/vscode_cobol/commit/920b10ebc649c44) *2021-01-15 18:20:54*


### GitHub [#258](https://github.com/spgennard/vscode_cobol/issues/258) current tags and releases missing  

**current tags and releases missing Fix #258**

 * - include changelog generation
* [c893b41ef50bf40](https://github.com/spgennard/vscode_cobol/commit/c893b41ef50bf40) *2021-01-18 08:22:05*



**Update CHANGELOG.md**

* [834837e40295b22](https://github.com/spgennard/vscode_cobol/commit/834837e40295b22) *2021-01-20 22:03:28*

**tweak gen-changelog**

* [875bf522efe40f4](https://github.com/spgennard/vscode_cobol/commit/875bf522efe40f4) *2021-01-20 22:02:02*

**change log is broken, so remove until I can resolve the issue**

* [2281745e3f52d17](https://github.com/spgennard/vscode_cobol/commit/2281745e3f52d17) *2021-01-19 00:05:14*

**Update CHANGELOG.md**

* [58d534f68509e73](https://github.com/spgennard/vscode_cobol/commit/58d534f68509e73) *2021-01-18 23:49:41*

**continue adding dep marker**

* [e076f85a4a558b7](https://github.com/spgennard/vscode_cobol/commit/e076f85a4a558b7) *2021-01-18 22:36:00*

**add "delimited by" snippets**

* [3d873823e8fd37b](https://github.com/spgennard/vscode_cobol/commit/3d873823e8fd37b) *2021-01-18 22:23:53*

**add "end program" and fix lower-case program-id**

* [85946853f7e3e7b](https://github.com/spgennard/vscode_cobol/commit/85946853f7e3e7b) *2021-01-18 22:05:40*

**update & bring inline with other uses of MIT license (.Net Core)**

* [6210544810aa949](https://github.com/spgennard/vscode_cobol/commit/6210544810aa949) *2021-01-18 21:44:48*

**tweak cobol provider**

* [8ff07e61a33cf76](https://github.com/spgennard/vscode_cobol/commit/8ff07e61a33cf76) *2021-01-17 22:57:54*

**be cautious on the unlink**

* [026a9d41f7c38cc](https://github.com/spgennard/vscode_cobol/commit/026a9d41f7c38cc) *2021-01-17 07:32:44*

**tweak message**

* [e4b54ee5f9d20f0](https://github.com/spgennard/vscode_cobol/commit/e4b54ee5f9d20f0) *2021-01-16 20:09:16*

**avoid warning message is only divs are in header of program**

* [37bf98d4ea6998f](https://github.com/spgennard/vscode_cobol/commit/37bf98d4ea6998f) *2021-01-16 17:25:23*

**changes to better handle implicit program-ids**

 * fix problem with untrimmed token that do not end with a full stop
* [9a4bfe67bd52e71](https://github.com/spgennard/vscode_cobol/commit/9a4bfe67bd52e71) *2021-01-16 17:04:43*

**add depreciated tags to the settings that are scheduled for**

 * replacement or removal
* [f7fa3003f6c9ae5](https://github.com/spgennard/vscode_cobol/commit/f7fa3003f6c9ae5) *2021-01-16 00:02:59*

**inc ver**

* [eaa09ebf6d375ed](https://github.com/spgennard/vscode_cobol/commit/eaa09ebf6d375ed) *2021-01-15 21:37:44*

**fix typo**

* [302c9be30be11e2](https://github.com/spgennard/vscode_cobol/commit/302c9be30be11e2) *2021-01-15 18:20:40*

**more cleanup, remove unused functionality**

* [66247321d186d3e](https://github.com/spgennard/vscode_cobol/commit/66247321d186d3e) *2021-01-14 21:42:07*

**add info about changing token colours**

* [2f1ea99c30b23ad](https://github.com/spgennard/vscode_cobol/commit/2f1ea99c30b23ad) *2021-01-12 21:48:49*

**turn off git/release as it will now only be used for private non-public**

 * builds
* [9d1a12c52135a44](https://github.com/spgennard/vscode_cobol/commit/9d1a12c52135a44) *2020-01-02 17:10:15*

**date update**

* [d8f6dfd7b188d2c](https://github.com/spgennard/vscode_cobol/commit/d8f6dfd7b188d2c) *2020-01-01 14:31:05*


## 7.0.0

**prepare for first release of the new year**

* [0a1bd708316f2b5](https://github.com/spgennard/vscode_cobol/commit/0a1bd708316f2b5) *2020-01-01 13:45:33*

**update readme with spell checker information**

* [d232f5ee1c8032b](https://github.com/spgennard/vscode_cobol/commit/d232f5ee1c8032b) *2020-12-31 11:31:55*

**breakout condition names**

* [58129dec12f287c](https://github.com/spgennard/vscode_cobol/commit/58129dec12f287c) *2020-12-30 23:01:54*

**tidy**

* [71e77e46a3eb086](https://github.com/spgennard/vscode_cobol/commit/71e77e46a3eb086) *2020-12-30 00:53:30*

**tidy up**

* [67383c771dabae7](https://github.com/spgennard/vscode_cobol/commit/67383c771dabae7) *2020-12-29 23:26:43*

**remove gnu list file from the file selectors**

* [265cebd6cf569a4](https://github.com/spgennard/vscode_cobol/commit/265cebd6cf569a4) *2020-12-29 22:55:44*

**add simple formatter for camelcase/uppercase on return-code**

 * - under experimential flag
* [c17b526ded6ea6b](https://github.com/spgennard/vscode_cobol/commit/c17b526ded6ea6b) *2020-12-29 22:54:52*

**tidy up use of COBOLUtils, move to statics, as no instance data is**

 * required
* [07f72abc9ffab90](https://github.com/spgennard/vscode_cobol/commit/07f72abc9ffab90) *2020-12-29 11:20:27*

**Remove a missed part of the old coboldoc support**

* [2182656088abe8e](https://github.com/spgennard/vscode_cobol/commit/2182656088abe8e) *2020-12-29 10:42:44*

**remove "Open COBOL" reference**

 * add note, about contributions &amp; gnucobol extension
* [ab16f471731f08d](https://github.com/spgennard/vscode_cobol/commit/ab16f471731f08d) *2020-12-22 22:53:12*


## 6.11.11
### Jira utf-8   

**add in utf-8 literal delimiters**

* [7954fe04ff05e53](https://github.com/spgennard/vscode_cobol/commit/7954fe04ff05e53) *2020-12-21 21:06:00*



**prep/push**

* [8272e2b8b21a8a6](https://github.com/spgennard/vscode_cobol/commit/8272e2b8b21a8a6) *2020-12-22 22:30:15*

**continue work on the task provider**

* [cfcfb937135ca50](https://github.com/spgennard/vscode_cobol/commit/cfcfb937135ca50) *2020-12-22 22:29:47*

**make it private**

* [3ee76f411be0217](https://github.com/spgennard/vscode_cobol/commit/3ee76f411be0217) *2020-12-22 00:24:05*

**expand the possible script names**

* [a5fd8550be7e4e9](https://github.com/spgennard/vscode_cobol/commit/a5fd8550be7e4e9) *2020-12-22 00:19:13*

**add dbcs literals**

* [1b8e44e04711777](https://github.com/spgennard/vscode_cobol/commit/1b8e44e04711777) *2020-12-21 22:54:48*

**add jnienvptr**

* [2e7c36dd01e7722](https://github.com/spgennard/vscode_cobol/commit/2e7c36dd01e7722) *2020-12-21 21:54:34*

**fix compilation issue**

* [f4d5648da6dd5d7](https://github.com/spgennard/vscode_cobol/commit/f4d5648da6dd5d7) *2020-12-21 21:50:42*

**sort the table to aid readability**

* [2deeb4f6fccbeef](https://github.com/spgennard/vscode_cobol/commit/2deeb4f6fccbeef) *2020-12-21 21:39:48*

**tweak special registers**

* [311608f3af6cde1](https://github.com/spgennard/vscode_cobol/commit/311608f3af6cde1) *2020-12-21 21:25:40*

**Update stale.yml**

 * tweak
* [9c2c2bfa62a24aa](https://github.com/spgennard/vscode_cobol/commit/9c2c2bfa62a24aa) *2020-12-21 19:24:53*

**add some discovered reserved words from netcobol, as known but illegal**

* [968c97938df504e](https://github.com/spgennard/vscode_cobol/commit/968c97938df504e) *2020-12-20 00:09:16*

**add more keywords from the iso2002 spec and add a config to ignore the**

 * first section if an entyr-point is next, its crudge needs more
 * improvement
* [00308c7fd59bd26](https://github.com/spgennard/vscode_cobol/commit/00308c7fd59bd26) *2020-12-19 11:18:38*


## 6.11.9

**add cobol-it to the task provider**

* [15b135bf235f623](https://github.com/spgennard/vscode_cobol/commit/15b135bf235f623) *2020-12-11 23:01:29*


## 6.11.8

**remove warning matcher is task provider for acu**

* [8e1cfc3132f00a8](https://github.com/spgennard/vscode_cobol/commit/8e1cfc3132f00a8) *2020-12-11 22:56:19*


## 6.11.7

**remove the last the non-module gnucobol bits, the extension is now split**

 * into two.
* [6fb56e743b4de1b](https://github.com/spgennard/vscode_cobol/commit/6fb56e743b4de1b) *2020-12-11 21:25:43*

**continue the removal of anything GnuCOBOL related, so it can be**

 * pushed into its own extension.
 * plus tidy up in utils
* [5c1e09584945aa8](https://github.com/spgennard/vscode_cobol/commit/5c1e09584945aa8) *2020-12-11 18:19:32*

**packages update & vscode**

* [53fe7d2b89a68aa](https://github.com/spgennard/vscode_cobol/commit/53fe7d2b89a68aa) *2020-12-08 23:10:57*


## 6.11.6

**add problem matchers to the default config**

* [4a4399a24157942](https://github.com/spgennard/vscode_cobol/commit/4a4399a24157942) *2020-12-08 23:05:20*

**fix outstanding overlooked bug with the keyword provider**

* [c51fe393428b0c4](https://github.com/spgennard/vscode_cobol/commit/c51fe393428b0c4) *2020-12-08 20:45:59*


## 6.11.5

**continue minor tweaks to task provider.. more work to do here...**

 * comment out subjective code in keyword provider, as I have send times it
 * causes more harm than good (aka it prevents keywords from being
 * returned)
* [5d2db1e6f392212](https://github.com/spgennard/vscode_cobol/commit/5d2db1e6f392212) *2020-12-07 23:36:08*

**start to wire up some options for the script provider**

* [5f4072b74b9aa94](https://github.com/spgennard/vscode_cobol/commit/5f4072b74b9aa94) *2020-12-06 23:32:30*

**bring back the bld script task**

* [50337da7ce0250f](https://github.com/spgennard/vscode_cobol/commit/50337da7ce0250f) *2020-12-06 22:01:09*


## 6.11.4

**second fix to problem matcher (out of sync message)**

* [111ff4c51359419](https://github.com/spgennard/vscode_cobol/commit/111ff4c51359419) *2020-12-04 14:24:22*


## 6.11.3

**fix problem matcher**

* [d567c13e267e7cd](https://github.com/spgennard/vscode_cobol/commit/d567c13e267e7cd) *2020-12-04 11:48:29*


## 6.11.2

**simplify previous change for catching different category of messages**

* [eb67395abd3661a](https://github.com/spgennard/vscode_cobol/commit/eb67395abd3661a) *2020-12-04 06:45:20*


## 6.11.1
### GitHub [#249](https://github.com/spgennard/vscode_cobol/issues/249) File is too long?    *question*  

**File is too long? Fix #249**

* [2cf01a622e5a4b0](https://github.com/spgennard/vscode_cobol/commit/2cf01a622e5a4b0) *2020-12-01 21:52:43*



**add support Micro Focus ECM error categories: CICS, SQL, XDB, IMS and DB2**

* [cef61264dc35411](https://github.com/spgennard/vscode_cobol/commit/cef61264dc35411) *2020-12-03 22:56:54*

**let vscode decide where the problem matcher find files (when we can)**

* [ce067b340aeb1b1](https://github.com/spgennard/vscode_cobol/commit/ce067b340aeb1b1) *2020-12-02 18:21:18*

**use autoDetect on some of the problem matchers**

* [e78175b9e2a67b4](https://github.com/spgennard/vscode_cobol/commit/e78175b9e2a67b4) *2020-12-02 00:01:15*

**further tweaks**

* [d628ffdf353c74c](https://github.com/spgennard/vscode_cobol/commit/d628ffdf353c74c) *2020-12-01 23:15:28*

**update**

* [bf87255e6907633](https://github.com/spgennard/vscode_cobol/commit/bf87255e6907633) *2020-11-30 23:42:18*


## 6.10.24

**add extra brackets & if,unstring,string into completion list**

* [88a2368ad61beb9](https://github.com/spgennard/vscode_cobol/commit/88a2368ad61beb9) *2020-11-30 23:34:07*


## 6.10.23

**tweak the brackets support to handle if/end-if and a few more**

 * (more may come, I just want to see who this pans out)
* [0e2b616493994b4](https://github.com/spgennard/vscode_cobol/commit/0e2b616493994b4) *2020-11-28 23:02:45*

**tweak $schema location**

* [fd2f7609167fc91](https://github.com/spgennard/vscode_cobol/commit/fd2f7609167fc91) *2020-11-25 21:18:49*

**not required**

* [f6eff348a3da129](https://github.com/spgennard/vscode_cobol/commit/f6eff348a3da129) *2020-11-25 21:14:55*

**add schema, as the original one has gone.. so include so I can reference**

 * it
* [51e21bc9e450048](https://github.com/spgennard/vscode_cobol/commit/51e21bc9e450048) *2020-11-25 21:13:35*


## 6.10.22

**add some cbltypes as support.function**

 * add 78 specific colouriser
* [bcccd266a8bb77c](https://github.com/spgennard/vscode_cobol/commit/bcccd266a8bb77c) *2020-11-24 00:30:34*

**add bot**

* [58371aaaca97235](https://github.com/spgennard/vscode_cobol/commit/58371aaaca97235) *2020-11-20 23:31:19*


## 6.10.21

**tweak casing util and fix some storage items missed**

* [c241c42c0647d40](https://github.com/spgennard/vscode_cobol/commit/c241c42c0647d40) *2020-11-18 22:53:43*

**update**

* [5cec68a4615ab24](https://github.com/spgennard/vscode_cobol/commit/5cec68a4615ab24) *2020-11-17 19:42:22*


## 6.10.20

**gnucobol related tidy up**

* [8884af7e59e6903](https://github.com/spgennard/vscode_cobol/commit/8884af7e59e6903) *2020-11-17 08:26:26*

**tweaks for more field types for bms**

 * - fix unrequired , in package.json
* [d26b4b3d5b9bde8](https://github.com/spgennard/vscode_cobol/commit/d26b4b3d5b9bde8) *2020-11-15 22:47:25*

**start to think about web use of the extension**

 * - can&#x27;t do too much until I get codespace access
* [a232fd750bfd589](https://github.com/spgennard/vscode_cobol/commit/a232fd750bfd589) *2020-11-14 23:41:37*

**add support for "exec java" used in some java based COBOL dialects**

 * fix &quot;-&quot; in first column that affected some comment lines
* [abf4378422ef32f](https://github.com/spgennard/vscode_cobol/commit/abf4378422ef32f) *2020-11-14 22:24:57*

**ensure we don't change to a document type that does not exist**

* [ef510bc1cde0030](https://github.com/spgennard/vscode_cobol/commit/ef510bc1cde0030) *2020-11-14 16:53:29*

**update changelog**

* [f8fccf8925f67c0](https://github.com/spgennard/vscode_cobol/commit/f8fccf8925f67c0) *2020-11-14 16:43:32*

**fix links**

* [937678698a9c043](https://github.com/spgennard/vscode_cobol/commit/937678698a9c043) *2020-11-14 16:38:26*

**changes related to master -> main branch name change**

* [b39d122937121a9](https://github.com/spgennard/vscode_cobol/commit/b39d122937121a9) *2020-11-14 16:37:43*



