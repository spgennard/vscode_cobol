//
// Note: This example test is leveraging the Mocha test framework.
// Please refer to their documentation on https://mochajs.org/ for help.
//

// The module 'assert' provides assertion methods from node
import * as assert from 'assert';

import { performance } from 'perf_hooks';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
// import * as vscode from 'vscode';
// import * as myExtension from '../extension';
import ISourceHandler from '../isourcehandler';
import { FileSourceHandler } from '../FileSourceHandler';
import QuickCOBOLParse from '../cobolquickparse';
import { ICOBOLSettings, COBOLSettings } from '../iconfiguration';


// Defines a Mocha test suite to group tests of similar kind together
suite("Extension Tests", function () {

    test("Test FileSourceHandler", function() {
        let fr: ISourceHandler = new FileSourceHandler("src/test/Program1.cbl", false);
        assert.ok(fr.getLineCount() > 0, "Line Count == 0 for Program1.cbl");

    });

    test("Test QuickCOBOLParse", function() {
        let fileName = "src/test/Program1.cbl";
        let fr: ISourceHandler = new FileSourceHandler("src/test/Program1.cbl", false);
        assert.ok(fr.getLineCount() > 0, "Line Count == 0 for Program1.cbl");

        let cobolSettings: ICOBOLSettings = new COBOLSettings();
        // var startTime = performance.now();
        let qcpd = new QuickCOBOLParse(fr, fileName, cobolSettings, "");
    
        assert.ok(qcpd.constantsOrVariables.size > 0, "constantsOrVariables == 0 for Program1.cbl");
    });


});