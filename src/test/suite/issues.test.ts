import * as assert from 'assert';

import * as vscode from 'vscode';

import { FileSourceHandler } from "../../filesourcehandler";
import COBOLSourceScanner, { CobolDocStyle } from "../../cobolsourcescanner";
import { COBOLSettings } from '../../iconfiguration';
import path from 'path';
import { extensions } from 'vscode';
import * as fs from 'fs';


suite('Issues Raised Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	const baseForSource = __dirname + "/../../../src/test/suite/";

	test('Issue: 189 [MSDN]', () => {
		const f = new FileSourceHandler(path.join(baseForSource, "issue189_msdn.cbl"), false);

		assert.ok(f.getFilename().length > 0, "filename is invalid");

		const settings = new COBOLSettings();
		const s = new COBOLSourceScanner(f, settings, "");
		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");

		assert.ok(f.commentCount > 0, "number of comments should be > 0");
		assert.ok(f.commentCallback !== undefined, "comment callback should be set");
		assert.ok(s.commentStyle === CobolDocStyle.MSDN, `Source comment style should be MSDN, got ${s.commentStyle}`);
	});


	test('Issue: 189 [COBOLDOC]', () => {
		const f = new FileSourceHandler(path.join(baseForSource, "issue189_coboldoc.cbl"), false);

		assert.ok(f.getFilename().length > 0, "filename is invalid");

		const settings = new COBOLSettings();
		const s = new COBOLSourceScanner(f,  settings, "");
		assert.ok(s.constantsOrVariables.size > 0, "should contain at least one field");

		assert.ok(f.commentCount > 0, "number of comments should be > 0");
		assert.ok(f.commentCallback !== undefined, "comment callback should be set");
		assert.ok(s.commentStyle === CobolDocStyle.COBOLDOC, `Source comment style should be COBOLDOC, got ${s.commentStyle}`);
	});

	test('Issue: Package.json checks', () => {

		const ext = extensions.getExtension("bitlang.cobol");
		assert.ok(ext !== undefined, "bitlang.cobol not found");

		if (ext.packageJSON.id === undefined) {
			assert.fail("No .id found");
		}

		if (ext.packageJSON.contributes === undefined) {
			assert.fail("No .contributes found");
		}

		if (ext.packageJSON.contributes.problemPatterns === undefined) {
			assert.fail("No .problemPatterns found");
		}

		const patterns = ext.packageJSON.contributes.problemPatterns;
		const diagOutput = true;
		let patternCount = 0;
		for (const key in patterns) {
			const pattern = patterns[key];
			if (pattern.name !== undefined) {
				const patternName = `${pattern.name}`;

				assert.ok(patternName !== null, "pattern should not be null");
				// console.log(`INFO: ${patternName}`);
				patternCount++;


				try {
					const possibleData = path.join(baseForSource, `co_${patternName}.txt`);
					if (fs.existsSync(possibleData)) {
						const r = new RegExp(`${pattern.regexp}`, "gm");

						// console.log(`OK - Found test data for ${patternName}`);
						const compilerOutputs = fs.readFileSync(possibleData);
						let resultCount = 0;
						let missingCount = 0;
						for (const compilerOutput of compilerOutputs.toString().split("\n")) {
							if (compilerOutput.length === 0) {
								continue;
							}
							if (r.test(compilerOutput)) {
								if (diagOutput) {
									console.log(`${pattern.regexp}:`);
								}

								if (diagOutput) {
									const regex = new RegExp(`${pattern.regexp}`, "gm");
									let m;

									while ((m = regex.exec(compilerOutput)) !== null) {
										// This is necessary to avoid infinite loops with zero-width matches
										if (m.index === regex.lastIndex) {
											regex.lastIndex++;
										}

										// The result can be accessed through the `m`-variable.
										m.forEach((match, groupIndex) => {
											console.log(` ${groupIndex} => [${match}]`);
										});
									}
								}

								resultCount++;
							} else {
								// console.log(` MISSED: ${compilerOutput}`);
								missingCount++;
							}
						}
						const percenMatched = (resultCount / (resultCount+missingCount)) * 100;
						const percenMatchedFixed = Number(percenMatched).toFixed(2);
						// console.log(`${patternName} found ${resultCount} did not match ${missingCount}`);
						if (percenMatched < 50) {
							assert.fail(`${patternName} failed to match enough lines in co_${patternName}.txt ${percenMatched}% (${resultCount}/${missingCount})
							}})`);
						} else {
							if (diagOutput) {
								console.log(`    ${patternName} did match lines in co_${patternName}.txt ${percenMatchedFixed}% with count=${resultCount}, missed=${missingCount}`);
							}
						}
					}
				} catch (e) {
					assert.fail(`${key}/${pattern} : ${e}`);
				}
			}
		}

		assert.ok(patternCount !== 0, "We should have at least one! pattern matcher :-)");

		// assert.ok(pattern_mfcobol_errformat2_netx_sx !== undefined, "mfcobol-errformat2-netx-sx not found");

		// const pattern_mfcobol_errformat2_netx_sx_regexp = pattern_mfcobol_errformat2_netx_sx.regexp;
		// console.log("pattern_mfcobol_errformat2_netx_sx: " + pattern_mfcobol_errformat2_netx_sx);
		// console.log("RegEx: " + pattern_mfcobol_errformat2_netx_sx_regexp);

		// const r = new RegExp(pattern_mfcobol_errformat2_netx_sx_regexp);
		// r.compile();
	});

});
