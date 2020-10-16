import * as assert from 'assert';

import * as vscode from 'vscode';

import { FileSourceHandler } from "../../filesourcehandler";
import COBOLSourceScanner, { CobolDocStyle, EmptyCOBOLSourceScannerEventHandler } from "../../cobolsourcescanner";
import { COBOLSettings } from '../../iconfiguration';
import path from 'path';
import { ExternalFeatures } from '../../extension';


suite('Issues Raised Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	const baseForSource = __dirname + "/../../../src/test/suite/";
	const eventHandler = EmptyCOBOLSourceScannerEventHandler.Default;
	const features = ExternalFeatures;

	test('Issue: 189 [MSDN]', () => {
		const f = new FileSourceHandler(path.join(baseForSource, "issue189_msdn.cbl"), false);

		assert.ok(f.getFilename().length > 0, "filename is invalid");

		const settings = new COBOLSettings();
		const s = COBOLSourceScanner.ParseUncached(f, settings, false, eventHandler, features);
		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");

		assert.ok(f.commentCount > 0, "number of comments should be > 0");
		assert.ok(f.commentCallback !== undefined, "comment callback should be set");
		assert.ok(s.commentDocStyle === CobolDocStyle.MSDN, `Source comment style should be MSDN, got ${s.commentDocStyle}`);
	});


	test('Issue: 189 [COBOLDOC]', () => {
		const f = new FileSourceHandler(path.join(baseForSource, "issue189_coboldoc.cbl"), false);

		assert.ok(f.getFilename().length > 0, "filename is invalid");

		const settings = new COBOLSettings();
		const s =  COBOLSourceScanner.ParseUncached(f,  settings, false, eventHandler, features);
		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");

		assert.ok(f.commentCount > 0, "number of comments should be > 0");
		assert.ok(f.commentCallback !== undefined, "comment callback should be set");
		assert.ok(s.commentDocStyle === CobolDocStyle.COBOLDOC, `Source comment style should be COBOLDOC, got ${s.commentDocStyle}`);
	});


});
