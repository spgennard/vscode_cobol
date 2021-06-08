/* eslint-disable no-console */
import * as assert from 'assert';

import path from 'path';
import { extensions } from 'vscode';
import * as fs from 'fs';

suite('Issues Raised Test Suite', () => {
	test('Issue: Package.json checks', () => {
		const baseForSource = __dirname + "/../../../src/test/suite/";

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
			if (pattern !== undefined && pattern.name !== undefined) {
				const patternName = `${pattern.name}`;

				assert.ok(patternName !== null, "pattern should not be null");
				// console.log(`INFO: ${patternName}`);
				patternCount++;

				const regExPatterns = [];

				if (pattern.regexp !== undefined) {
					regExPatterns.push(`${pattern.regexp}`);
				}

				// note: this does not have the same semantics as vscode but is enough to see
				//       when/if the pattern works or not
				if (pattern.patterns !== undefined) {
					for (const key in pattern.patterns) {
						const patternR = pattern.patterns[key];
						if (patternR !== undefined) {
							regExPatterns.push(`${patternR.regexp}`);
						}
					}
				}
				try {
					const possibleData = path.join(baseForSource, `co_${patternName}.txt`);
					if (fs.existsSync(possibleData)) {

						assert.ok(regExPatterns.length !== 0, `pattern regex is empty for ${patternName}`);

						let passlevel = 50;

						// console.log(`OK - Found test data for ${patternName}`);
						const compilerOutputs = fs.readFileSync(possibleData);
						let resultCount = 0;
						let missingCount = 0;
						let lines = compilerOutputs.toString().split("\n");
						const firstLine = lines.shift();
						if (firstLine !== undefined) {
							const possiblePassLevel = Number.parseInt(firstLine,10);
							if (!isNaN(possiblePassLevel)) {
								passlevel = possiblePassLevel;
								console.log(` % pass level set @ ${passlevel}`);
							} else {
								lines = lines.splice(0,0, firstLine);
								console.log(` % pass level set @ ${passlevel} (default)`);
							}
						}
						for (const compilerOutput of lines) {
							if (compilerOutput.length === 0) {
								continue;
							}
							let okayCountPerPattern = 0;
							for (const regExPattern of regExPatterns) {
								const r = new RegExp(`${regExPattern}`, "gm");
								if (r.test(compilerOutput)) {
									okayCountPerPattern++;
									if (diagOutput) {
										console.log(`${patternName} is ${regExPattern}:`);
									}

									if (diagOutput) {
										const regex = new RegExp(`${regExPattern}`, "gm");
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
								}
							}
							if (okayCountPerPattern === 0) {
								if (diagOutput) {
									console.log(`${patternName} is ${pattern.regexp}:`);
								}
								console.log(` MISSED: ${patternName} => "${compilerOutput}"`);
								missingCount++;

							}
						}
						const percenMatched = (resultCount / (resultCount + missingCount)) * 100;
						const percenMatchedFixed = Number(percenMatched).toFixed(2);
						// console.log(`${patternName} found ${resultCount} did not match ${missingCount}`);
						if (percenMatched < passlevel) {
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
