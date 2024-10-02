import * as vscode from "vscode";
import { COBOLSourceScanner, COBOLTokenStyle } from "./cobolsourcescanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSLogger } from "./vslogger";
import { ICOBOLSettings } from "./iconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";

const tokenTypes = ["label", "variable", "function", "comment"];
const tokenModifiers = ["declaration", "readonly", "deprecated"];

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

        const fid = qcp.sourceFileId;
        for (const [, sourceRefs] of qcp.sourceReferences.targetReferences) {
            try {
                for (const sourceRef of sourceRefs) {
                    if (sourceRef.fileIdentifer === fid) {
                        tokensBuilder.push(
                            new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                            "function"
                        );

                    }
                }
            } catch (e) {
                VSLogger.logException("SemanticProvider: targetReferences", e as Error);
            }
        }

        for (const [, sourceRefs] of qcp.sourceReferences.constantsOrVariablesReferences) {
            //
            for (const sourceRef of sourceRefs) {
                try {
                    if (sourceRef.fileIdentifer === fid) {
                        if (sourceRef.tokenStyle === COBOLTokenStyle.Constant) {
                            tokensBuilder.push(
                                new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                                "variable",
                                ["readonly"]
                            );
                        }
                    }
                } catch (e) {
                    VSLogger.logException("SemanticProvider: sourceRefs", e as Error);
                }
            }
        }

        for (const sourceRef of qcp.sourceReferences.ignoreLSRanges) {
            //
            try {
                if (sourceRef.fileIdentifer === fid) {
                    tokensBuilder.push(
                        new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.endLine, sourceRef.endColumn)),
                        "comment",
                        ["deprecated"]
                    );

                }
            } catch (e) {
                VSLogger.logException("SemanticProvider: sourceRefs", e as Error);
            }
        }

        return tokensBuilder.build();
    }
}
