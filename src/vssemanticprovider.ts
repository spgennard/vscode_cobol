import * as vscode from 'vscode';
import COBOLSourceScanner from './cobolsourcescanner';
import { VSCOBOLConfiguration } from './configuration';
import { ICOBOLSettings } from './iconfiguration';
import VSCOBOLSourceScanner from './vscobolscanner';

const tokenTypes = ['label', 'variable'];
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

        const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (qcp === undefined) {
            return tokensBuilder.build();
        }

        for (const [key, token] of qcp.sections) {
            if (token.inProcedureDivision) {
                tokensBuilder.push(
                    new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn+token.tokenName.length)),
                    'label',
                    ['declaration']
                );
            }
        }
        for (const [key, token] of qcp.paragraphs) {
            if (token.inProcedureDivision) {
                tokensBuilder.push(
                    new vscode.Range(new vscode.Position(token.startLine, token.startColumn), new vscode.Position(token.startLine, token.startColumn+token.tokenName.length)),
                    'label',
                    ['declaration']
                );
            }
        }

        for (const [key, sourceRefs] of qcp.sourceReferences.targetReferences) {
            for(const sourceRef of sourceRefs) {
                tokensBuilder.push(
                    new vscode.Range(new vscode.Position(sourceRef.line, sourceRef.column), new vscode.Position(sourceRef.line, sourceRef.column+sourceRef.length)),
                    'label'
                );
            }

        }
        return tokensBuilder.build();
    }
}
