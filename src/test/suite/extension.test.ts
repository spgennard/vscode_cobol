import * as assert from 'assert';

import * as vscode from 'vscode';

import { FileSourceHandler } from "../../filesourcehandler";
import COBOLSourceScanner, { SharedSourceReferences } from "../../cobolsourcescanner";
import { COBOLSettings } from '../../iconfiguration';
import path from 'path';

suite('Core Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	const baseForSource = __dirname+"/../../../src/test/suite/";

	test('Read file [basic] (test.cbl)', () => {
		const f = new FileSourceHandler(path.join(baseForSource,"test.cbl"),false,false);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
	});

	test('Parse file for constants/paragraphs/sections (test.cbl)', () => {
		const f = new FileSourceHandler(path.join(baseForSource,"test.cbl"),false,false);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
		const settings = new COBOLSettings();
		const ssr = new SharedSourceReferences(true);
		const s = new COBOLSourceScanner(f, f.getFilename(), settings, "",ssr);

		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");
		assert.ok(s.paragraphs.size > 0, "should contain at least one paragraph");
		assert.ok(s.sections.size > 0, "should contain at least one section");

	});

	test('Parse file for functions (string.cbl)', () => {
		const f = new FileSourceHandler(path.join(baseForSource,"string.cbl"),false,false);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
		const settings = new COBOLSettings();
		const ssr = new SharedSourceReferences(true);
		const s = new COBOLSourceScanner(f, f.getFilename(), settings, "", ssr);

		assert.ok(s.functionTargets.size > 0, `should contain at least one function (got: ${s.functionTargets.size})`);
	});

});
