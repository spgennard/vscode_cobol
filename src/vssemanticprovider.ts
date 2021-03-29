import * as vscode from 'vscode';
import COBOLSourceScanner, { COBOLTokenStyle } from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './configuration';
import { logException } from './extension';
import { ICOBOLSettings } from './iconfiguration';
import VSCOBOLSourceScanner from './vscobolscanner';

const tokenTypes = ['label', 'variable', 'function'];
const tokenModifiers = ['declaration', 'readonly'];

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

        for (const [key, token] of qcp.sections) {
            try {
                if (token.ignoreInOutlineView === false && token.inProcedureDivision) {
                    tokensBuilder.push(
                        new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                        'function'
                    );
                }

            } catch (e) {
                logException("SemanticProvider: sections", e);
            }
        }
        for (const [key, token] of qcp.paragraphs) {
            try {
                if (token.ignoreInOutlineView === false && token.inProcedureDivision) {
                    tokensBuilder.push(
                        new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                        'function'
                    );
                }

            } catch (e) {
                logException("SemanticProvider: paragraphs", e);
            }
        }

        for (const [key, sourceRefs] of qcp.sourceReferences.targetReferences) {
            try {
                for (const sourceRef of sourceRefs) {
                    tokensBuilder.push(
                        new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                        'function'
                    );
                }
            } catch (e) {
                logException("SemanticProvider: targetReferences", e);
            }

        }

        for (const [key, tokens] of qcp.constantsOrVariables) {

            for (const token of tokens) {
                if (token.ignoreInOutlineView === false) {
                    try {
                        if (token.tokenType === COBOLTokenStyle.Constant) {
                            tokensBuilder.push(
                                new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                                'variable',
                                ['declaration', 'readonly']
                            );
                        } else {
                            tokensBuilder.push(
                                new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn + token.tokenName.length)),
                                'variable',
                                ['declaration']
                            );
                        }
                    } catch (e) {
                        logException("SemanticProvider: constantsOrVariables", e);
                    }
                }
            }
        }

        for (const [key, sourceRefs] of qcp.sourceReferences.constantsOrVariablesReferences) {
            //
            for (const sourceRef of sourceRefs) {
                try {
                    if (sourceRef.tokenStyle === COBOLTokenStyle.Constant) {
                        tokensBuilder.push(
                            new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                            'variable',
                            ['readonly']
                        );

                    } else {
                        tokensBuilder.push(
                            new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column + sourceRef.length)),
                            'variable'
                        );
                    }
                } catch (e) {
                    logException("SemanticProvider: sourceRefs", e);
                }
            }
        }

        return tokensBuilder.build();
    }
}
