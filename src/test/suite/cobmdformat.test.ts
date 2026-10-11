import * as assert from "assert";
import { CobmdFormat } from "../../cobmdformat";

suite("COBOL Markdown format", () => {
    test("assembles repeated fragments into one target", () => {
        const document = CobmdFormat.parse([
            "# Sample",
            "",
            "```cobol file=myprogram.cbl",
            "working-storage section.",
            "01 d pic x(10) value 'Hi'.",
            "```",
            "",
            "```cobol file=myprogram.cbl",
            "procedure division.",
            "   display d \" developer\"",
            "```",
            ""
        ].join("\n"));

        assert.deepStrictEqual(document.diagnostics, []);
        assert.strictEqual(document.targets.length, 1);
        assert.strictEqual(document.targets[0].filename, "myprogram.cbl");
        assert.strictEqual(document.targets[0].content, [
            "working-storage section.",
            "01 d pic x(10) value 'Hi'.",
            "procedure division.",
            "   display d \" developer\"",
            ""
        ].join("\n"));
        assert.deepStrictEqual(document.targets[0].lines.map(line => line.sourceLine), [3, 4, 8, 9]);
    });

    test("supports multiple targets, quoted names, aliases, and tilde fences", () => {
        const document = CobmdFormat.parse([
            "~~~BITLANG-COBOL file=src/main.cbl",
            "procedure division.",
            "~~~",
            "```cobolit file=\"copy books/data.cpy\"",
            "01 DATA-ITEM PIC X.",
            "```"
        ].join("\r\n"));

        assert.deepStrictEqual(document.targets.map(target => target.filename), [
            "src/main.cbl",
            "copy books/data.cpy"
        ]);
    });

    test("reports missing, unsafe, unsupported, and unterminated targets", () => {
        const document = CobmdFormat.parse([
            "```cobol",
            "display 'missing'.",
            "```",
            "```cobol file=../outside.cbl",
            "display 'unsafe'.",
            "```",
            "```cobol file=notes.txt",
            "display 'wrong extension'.",
            "```",
            "```cobol file=broken.cbl"
        ].join("\n"));

        assert.deepStrictEqual(document.diagnostics.map(diagnostic => diagnostic.code), [
            "missing-target",
            "invalid-target",
            "invalid-target",
            "unterminated-fence"
        ]);
        assert.deepStrictEqual(document.targets, []);
    });

    test("ignores non-COBOL fenced blocks", () => {
        const document = CobmdFormat.parse([
            "```typescript file=ignored.cbl",
            "const value = 1;",
            "```"
        ].join("\n"));

        assert.deepStrictEqual(document.fragments, []);
        assert.deepStrictEqual(document.diagnostics, []);
    });
});