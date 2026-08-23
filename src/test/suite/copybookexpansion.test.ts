import * as assert from "assert";
import { CopybookExpansionBuilder } from "../../copybookexpansion";
import { COBOLCopybookToken, COBOLToken, copybookState } from "../../cobolsourcescanner";
import { ISourceHandler } from "../../isourcehandler";

function source(uri: string, lines: string[], updatedLines = new Map<number, string>()): ISourceHandler {
    return {
        getLineCount: () => lines.length,
        getUpdatedLine: (line: number) => updatedLines.get(line) ?? lines[line],
        getLine: (line: number) => lines[line],
        getUriAsString: () => uri
    } as unknown as ISourceHandler;
}

function copybook(
    sourceUri: string,
    sourceHandler: ISourceHandler | undefined,
    statementLine = 0,
    name = "TESTCPY"
): COBOLCopybookToken {
    const state = new copybookState(undefined);
    state.startLineNumber = statementLine;
    state.endLineNumber = statementLine;
    state.sourceHandler = sourceHandler;
    const token = {
        filenameAsURI: sourceUri,
        startLine: statementLine,
        startColumn: 7,
        tokenName: name
    } as COBOLToken;
    return new COBOLCopybookToken(undefined, token, true, state);
}

suite("Copybook expansion", () => {
    test("uses scanner-updated lines and records their origins", () => {
        const handler = source(
            "file:///copybooks/root.cpy",
            ["       01 OLD-NAME PIC X."],
            new Map([[0, "       01 NEW-NAME PIC X."]])
        );

        const root = copybook("file:///program.cbl", handler);
        const result = CopybookExpansionBuilder.build(root);

        assert.strictEqual(CopybookExpansionBuilder.originalContent(root), "       01 OLD-NAME PIC X.");
        assert.strictEqual(result.content, "       01 NEW-NAME PIC X.");
        assert.notStrictEqual(CopybookExpansionBuilder.originalContent(root), result.content);
        assert.deepStrictEqual(result.lines[0].origin, {
            sourceUri: "file:///copybooks/root.cpy",
            sourceLine: 0
        });
    });

    test("inserts nested copybooks in statement order", () => {
        const root = copybook(
            "file:///program.cbl",
            source("file:///copybooks/root.cpy", [
                "       01 ROOT-ITEM.",
                "       COPY CHILD.",
                "          05 ROOT-TAIL PIC X."
            ])
        );
        const child = copybook(
            "file:///copybooks/root.cpy",
            source("file:///copybooks/child.cpy", ["          05 CHILD-ITEM PIC X."]),
            1,
            "CHILD"
        );
        root.children.push(child);

        const result = CopybookExpansionBuilder.build(root);

        assert.deepStrictEqual(result.lines.map(line => line.text), [
            "       01 ROOT-ITEM.",
            "          05 CHILD-ITEM PIC X.",
            "          05 ROOT-TAIL PIC X."
        ]);
        assert.deepStrictEqual(result.lines.map(line => line.origin?.sourceUri), [
            "file:///copybooks/root.cpy",
            "file:///copybooks/child.cpy",
            "file:///copybooks/root.cpy"
        ]);
    });

    test("keeps a visible marker when a nested copybook cannot be expanded", () => {
        const root = copybook(
            "file:///program.cbl",
            source("file:///copybooks/root.cpy", ["       COPY MISSING."])
        );
        root.children.push(copybook("file:///copybooks/root.cpy", undefined, 0, "MISSING"));

        const result = CopybookExpansionBuilder.build(root);

        assert.strictEqual(result.content, "       COPY MISSING.\n*> Unable to expand copybook MISSING");
        assert.deepStrictEqual(result.lines[0].origin, {
            sourceUri: "file:///copybooks/root.cpy",
            sourceLine: 0
        });
        assert.strictEqual(result.lines[1].origin, undefined);
    });

    test("honors cancellation", () => {
        const root = copybook(
            "file:///program.cbl",
            source("file:///copybooks/root.cpy", ["       01 ROOT-ITEM PIC X."])
        );

        const result = CopybookExpansionBuilder.build(root, { isCancellationRequested: true });

        assert.strictEqual(result.content, "");
        assert.deepStrictEqual(result.lines, []);
    });

});