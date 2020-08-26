import * as assert from 'assert';

import * as vscode from 'vscode';

import { FileSourceHandler } from "../../filesourcehandler";
import COBOLSourceScanner, { CobolDocStyle } from "../../cobolsourcescanner";
import { COBOLSettings } from '../../iconfiguration';
import path from 'path';

suite('Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	const baseForSource = __dirname+"/../../../src/test/suite/";

	test('Read file (test.cbl)', () => {
		const f = new FileSourceHandler(path.join(baseForSource,"test.cbl"),false,false);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
	});

	test('Parse file (test.cbl)', () => {
		const f = new FileSourceHandler(path.join(baseForSource,"test.cbl"),false,false);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
		const settings = new COBOLSettings();
		const s = new COBOLSourceScanner(f, f.getFilename(), settings, "");

		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");
		assert.ok(s.paragraphs.size > 0, "should contain at least one paragraph");
		assert.ok(s.sections.size > 0, "should contain at least one section");

	});

	test('Issue: 189 [MSDN]', () => {
		const f = new FileSourceHandler(path.join(baseForSource,"issue189_msdn.cbl"),false,false);

		assert.ok(f.getFilename().length > 0, "filename is invalid");

		const settings = new COBOLSettings();
		const s = new COBOLSourceScanner(f, f.getFilename(), settings, "");
		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");

		assert.ok(f.commentCount > 0, "number of comments should be > 0");
		assert.ok(f.commentCallback !== undefined, "comment callback should be set");
		assert.ok(s.commentStyle === CobolDocStyle.MSDN, `Source comment style should be MSDN, got ${s.commentStyle}`);
	});


	test('Issue: 189 [COBOLDOC]', () => {
		const f = new FileSourceHandler(path.join(baseForSource,"issue189_coboldoc.cbl"),false,false);

		assert.ok(f.getFilename().length > 0, "filename is invalid");

		const settings = new COBOLSettings();
		const s = new COBOLSourceScanner(f, f.getFilename(), settings, "");
		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");

		assert.ok(f.commentCount > 0, "number of comments should be > 0");
		assert.ok(f.commentCallback !== undefined, "comment callback should be set");
		assert.ok(s.commentStyle === CobolDocStyle.COBOLDOC, `Source comment style should be COBOLDOC, got ${s.commentStyle}`);
	});


});
