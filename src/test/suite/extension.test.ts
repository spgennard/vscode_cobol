import * as assert from 'assert';

import * as vscode from 'vscode';

import { FileSourceHandler } from "../../filesourcehandler";
import COBOLSourceScanner from "../../cobolsourcescanner";
import { COBOLSettings } from '../../iconfiguration';

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

	test('Parse file (test.cbl)', () => {
		console.log("CWD="+__dirname);
		const f = new FileSourceHandler(baseForSource+"test.cbl",false,false);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
		const settings = new COBOLSettings();
		const s = new COBOLSourceScanner(f, f.getFilename(), settings, "");

		assert.ok(s.constantsOrVariables.size > 0, "must contain should fields");

	});
});
