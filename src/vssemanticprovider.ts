import * as vscode from "vscode";
import { COBOLSourceScanner, COBOLTokenStyle } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSLogger } from "./vslogger";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";

const tokenTypes = ["label", "variable", "function"];
const tokenModifiers = ["declaration", "readonly"];

const legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);

export class VSSemanticProvider {
    public static getLegend(): vscode.SemanticTokensLegend {
        return legend;
    }

    public static provider(): vscode.DocumentSemanticTokensProvider {
        const provider: vscode.DocumentSemanticTokensProvider = {
            provideDocumentSemanticTokens(
                document: vscode.TextDocument
            ): vscode.ProviderResult<vscode.SemanticTokens> {
                return VSSemanticProvider.providerImpl(document);
            }
        };

        return provider;
    }

    private static providerImpl(document: vscode.TextDocument): vscode.ProviderResult<vscode.SemanticTokens> {
        const settings: ICOBOLSettings = VSCOBOLConfiguration.get();
        const tokensBuilder = new vscode.SemanticTokensBuilder(legend);
        if (settings.enable_semantic_token_provider === false) {
            return tokensBuilder.build();
        }

        const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (qcp === undefined) {
            return tokensBuilder.build();
        }

        for (const [, token] of qcp.sections) {
            try {
                if (token.ignoreInOutlineView === false && token.inProcedureDivision) {
                    tokensBuilder.push(
                        new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                        "function"
                    );
                }

            } catch (e) {
                VSLogger.logException("SemanticProvider: sections", e as Error);
            }
        }
        for (const [, token] of qcp.paragraphs) {
            try {
                if (token.ignoreInOutlineView === false && token.inProcedureDivision) {
                    tokensBuilder.push(
                        new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                        "function"
                    );
                }

            } catch (e) {
                VSLogger.logException("SemanticProvider: paragraphs", e as Error);
            }
        }

        for (const [, sourceRefs] of qcp.sourceReferences.targetReferences) {
            try {
                for (const sourceRef of sourceRefs) {
                    tokensBuilder.push(
                        new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                        "function"
                    );
                }
            } catch (e) {
                VSLogger.logException("SemanticProvider: targetReferences", e as Error);
            }

        }

        for (const [, tokens] of qcp.constantsOrVariables) {

            for (const token of tokens) {
                if (token.ignoreInOutlineView === false) {
                    try {
                        if (token.tokenType === COBOLTokenStyle.Constant) {
                            tokensBuilder.push(
                                new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                                "variable",
                                ["declaration", "readonly"]
                            );
                        } else {
                            tokensBuilder.push(
                                new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                                "variable",
                                ["declaration"]
                            );
                        }
                    } catch (e) {
                        VSLogger.logException("SemanticProvider: constantsOrVariables", e as Error);
                    }
                }
            }
        }

        for (const [, sourceRefs] of qcp.sourceReferences.constantsOrVariablesReferences) {
            //
            for (const sourceRef of sourceRefs) {
                try {
                    if (sourceRef.tokenStyle === COBOLTokenStyle.Constant) {
                        tokensBuilder.push(
                            new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                            "variable",
                            ["readonly"]
                        );

                    } else {
                        tokensBuilder.push(
                            new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                            "variable"
                        );
                    }
                } catch (e) {
                    VSLogger.logException("SemanticProvider: sourceRefs", e as Error);
                }
            }
        }

        return tokensBuilder.build();
    }
}
