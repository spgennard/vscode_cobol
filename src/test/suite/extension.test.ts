import * as assert from "assert";

import * as vscode from "vscode";

import { FileSourceHandler } from "../../filesourcehandler";
import { COBOLSourceScanner, EmptyCOBOLSourceScannerEventHandler } from "../../cobolsourcescanner";
import { COBOLSettings } from "../../iconfiguration";
import path from "path";
import { VSExternalFeatures } from "../../vsexternalfeatures";
import { CopybookExpansionBuilder } from "../../copybookexpansion";

suite("Core Extension Test Suite", () => {
	vscode.window.showInformationMessage("Start all tests.");

	const baseForSource = __dirname+"/../../../src/test/suite/";
	const eventHandler = EmptyCOBOLSourceScannerEventHandler.Default;
	const features = VSExternalFeatures;
	const settings = new COBOLSettings();

	test("Read file [basic] (test.cbl)", () => {
		const f = new FileSourceHandler(settings, undefined, path.join(baseForSource,"test.cbl"), features);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
	});

	test("Parse file for constants/paragraphs/sections (test.cbl)", () => {
		const f = new FileSourceHandler(settings, undefined, path.join(baseForSource,"test.cbl"), features);
		if (f.lines.length < 10) {
			assert.fail("test.cbl should have > 10 lines");
		}

		assert.ok(f.getFilename().length > 0, "filename is invalid");
		const s = COBOLSourceScanner.ScanUncached(f, settings, false, eventHandler, features);

		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");
		assert.ok(s.paragraphs.size > 0, "should contain at least one paragraph");
		assert.ok(s.sections.size > 0, "should contain at least one section");

	});

	test("Expand nested copybooks with inherited replacements", async () => {
		const fixtureDirectory = path.join(baseForSource, "fixtures/copybook-expansion");
		const expansionSettings = new COBOLSettings();
		expansionSettings.file_search_directory = [fixtureDirectory];
		expansionSettings.copybookexts = ["cpy"];
		expansionSettings.parse_copybooks_for_references = true;
		expansionSettings.enable_text_replacement = true;
		expansionSettings.copybook_scan_depth = 10;
		const sourceHandler = new FileSourceHandler(expansionSettings, undefined, path.join(fixtureDirectory, "program.cbl"), features);
		const scanner = COBOLSourceScanner.ScanUncached(sourceHandler, expansionSettings, true, eventHandler, features);
		const root = [...scanner.copyBooksUsed.values()]
			.flat()
			.find(entry => entry.token?.filename === sourceHandler.getFilename() && entry.token.tokenNameLower === "root");

		assert.ok(root, "root COPY occurrence should be resolved");
		assert.strictEqual(root.children.length, 1);
		const result = CopybookExpansionBuilder.build(root);
		assert.ok(result.content.includes("01 NEW-GROUP."));
		assert.ok(result.content.includes("05 NEW-FIELD PIC X."));
		assert.ok(!result.content.includes("COPY CHILD"));

		const occurrence = CopybookExpansionBuilder.occurrence(root);
		assert.ok(occurrence);
		const expandedUri = vscode.Uri.from({
			scheme: "cobol-expanded",
			path: "/ROOT.cpy.5.expanded.cbl",
			query: encodeURIComponent(JSON.stringify({ occurrence, view: "expanded" }))
		});
		await vscode.commands.executeCommand("cobolplugin.refreshExpandedCopybook", expandedUri);
	});
});
