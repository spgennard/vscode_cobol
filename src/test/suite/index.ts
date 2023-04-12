/* eslint-disable no-console */
import * as path from "path";
import Mocha from "mocha";
import { glob } from "glob";
import {  Path } from "path-scurry";

export async function run(): Promise<void> {
	// Create the mocha test
	const mocha = new Mocha({
		ui: "tdd",
		color: true
	});

	const testsRoot = path.resolve(__dirname, "..");

	const files = await glob("**/**.test.js", { withFileTypes:true, cwd:testsRoot});

	return new Promise((c, e) => {

			// Add files to the test suite
			files.forEach((f:Path) => mocha.addFile(path.resolve(testsRoot, f.fullpath())));

			try {
				// Run the mocha test
				mocha.run(failures => {
					if (failures > 0) {
						e(new Error(`${failures} tests failed.`));
					} else {
						c();
					}
				});
			} catch (err) {
				console.error(err);
				e(err);
			}
		});
	
}
