/* eslint-disable no-console */
import * as path from "path";
import Mocha from "mocha";
import glob from "glob";

export function run(): Promise<void> {
	// Create the mocha test
	const mocha = new Mocha({
		ui: "tdd",
		color: true
	});

	const testsRoot = path.resolve(__dirname, "..");

	return new Promise((c, e) => {
		const opt: glob.IOptions = { cwd: testsRoot};

		glob("**/**.test.js", opt, (err:Error|null, files:string[]) => {
			if (err) {
				return e(err);
			}

			// Add files to the test suite
			files.forEach(f => mocha.addFile(path.resolve(testsRoot, f)));

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
	});
}
