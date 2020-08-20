import * as assert from 'assert';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
import * as testExtension from '../../extension';

import { FileSourceHandler } from "../../filesourcehandler";

suite('Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	const baseForSource = __dirname+"/../../../src/test/suite/";

	test('Read file (test.cbl)', () => {
		console.log("CWD="+__dirname);
		const f = new FileSourceHandler(baseForSource+"test.cbl",false,false);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
	});

});
