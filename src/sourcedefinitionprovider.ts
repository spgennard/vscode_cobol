import * as vscode from 'vscode';
import { logMessage } from './extension';
import COBOLSourceScanner, { COBOLTokenStyle, COBOLToken } from './cobolsourcescanner';
import VSCOBOLSourceScanner from './vscobolscanner';
import { VSCOBOLConfiguration } from './configuration';
import { cobolKeywordDictionary } from './keywords/cobolKeywords';
import { ICOBOLSettings } from './iconfiguration';

export class COBOLSourceDefinition implements vscode.DefinitionProvider {

    readonly sectionRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
    readonly variableRegEx = new RegExp('[#0-9a-zA-Z][a-zA-Z0-9-_]*');
    readonly callRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
    readonly classRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');
    readonly methodRegEx = new RegExp('[0-9a-zA-Z][a-zA-Z0-9-_]*');

    public provideDefinition( document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken ): vscode.ProviderResult<vscode.Definition> {
        return this.resolveDefinitions(document, position, token);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async resolveDefinitions(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken): Promise<vscode.Definition> {
        const locations: vscode.Location[] = [];
        let loc;
        const settings: ICOBOLSettings = VSCOBOLConfiguration.get();

        const theline = document.lineAt(position.line).text;
        if (theline.match(/.*(perform|thru|go\s*to|until|varying).*$/i)) {
            const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
            if (qcp === undefined) {
                return locations;
            }

            loc = this.getSectionOrParaLocation(document, qcp, position);
            if (loc) {
                locations.push(loc);
                return locations;
            }
        }

        if (theline.match(/.*(new\s*|type).*$/i)) {
            const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
            if (qcp === undefined) {
                return locations;
            }

            loc = this.getClassTarget(document, qcp, position);
            if (loc !== undefined) {
                locations.push(loc);
                return locations;
            }
        }

        if (theline.match(/.*(invoke\s*|::)(.*$)/i)) {
            const qcp: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
            if (qcp === undefined) {
                return locations;
            }
            loc = this.getMethodTarget(document, qcp, position);
            if (loc !== undefined) {
                locations.push(loc);
                return locations;
            }
        }

        /* is it a known variable? */
        if (this.getVariableInCurrentDocument(locations, document, position,settings)) {
            return locations;
        }

        /* fuzzy search is not using the parser and it give false positive's, so lets
         * disable it by default but allow it to be re-enabled via config setting
         */
        if (settings.fuzzy_variable_search) {
            /* let's see if is a variable via our fuzzy matcher (catch all/brute force) */
            loc = this.getFuzzyVariable(document, position);
            if (loc) {
                locations.push(loc);
                return locations;
            }
        }

        return locations;
    }

    private getFuzzyVariable(document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, new RegExp("[a-zA-Z0-9_-]+"));
        const word = wordRange ? document.getText(wordRange) : '';
        if (word === "") {
            return undefined;
        }
        const workLower = word.toLocaleLowerCase();

        for (let i = 1; i < document.lineCount; i++) {
            const lineText = document.lineAt(i).text;

            // TODO - need to handle inline comments too
            if (lineText.length > 7 && lineText[6] === '*') {
                continue;
            }

            // time to leave
            if (lineText.match(/.*(procedure\s+division).*$/i)) {
                return undefined;
            }

            const wordIndex = lineText.toLowerCase().indexOf(workLower);
            if (wordIndex !== -1) {
                const leftOfWord = lineText.substr(0, wordIndex).trim();

                // fuzzy match for variable
                if (leftOfWord.match(/^[0-9 ]+$/i)) {
                    return new vscode.Location(
                        document.uri,
                        new vscode.Position(i, wordIndex)
                    );
                }

                // fuzzy match for a FD declaration
                if (leftOfWord.toLocaleLowerCase() === "fd") {
                    return new vscode.Location(
                        document.uri,
                        new vscode.Position(i, wordIndex)
                    );
                }
            }
        }
        return undefined;
    }


    private getSectionOrParaLocation(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, this.sectionRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        if (word === "") {
            return undefined;
        }

        const wordLower = word.toLowerCase();

        try {

            if (sf.sections.has(wordLower)) {
                const token = sf.sections.get(wordLower);
                if (token !== undefined) {
                    const srange = new vscode.Position(token.startLine, token.startColumn);
                    const uri = vscode.Uri.file(token.filename);
                    return new vscode.Location(uri, new vscode.Range(srange, srange));
                }
            }

        }
        catch (e) {
            logMessage(e);
        }

        try {
            if (sf.paragraphs.has(wordLower)) {
                const token = sf.paragraphs.get(wordLower);
                if (token !== undefined) {
                    const srange = new vscode.Position(token.startLine, token.startColumn);
                    const uri = vscode.Uri.file(token.filename);
                    return new vscode.Location(uri, new vscode.Range(srange, srange));
                }
            }
        }
        catch (e) {
            logMessage(e);
        }
        return undefined;
    }

    private isValidKeyword(keyword: string): boolean {
        return cobolKeywordDictionary.has(keyword);
    }

    private getVariableInCurrentDocument(locations: vscode.Location[], document: vscode.TextDocument, position: vscode.Position, settings: ICOBOLSettings): boolean {
        const wordRange = document.getWordRangeAtPosition(position, this.variableRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        if (word === "") {
            return false;
        }

        const tokenLower: string = word.toLowerCase();
        if (this.isValidKeyword(tokenLower)) {
            return false;
        }

        const sf: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (sf === undefined) {
            return false;
        }

        const tokens: COBOLToken[] | undefined = sf.constantsOrVariables.get(tokenLower);
        if (tokens === undefined || tokens.length === 0) {
            return false;
        }

        for (let i = 0; i < tokens.length; i++) {
            const token: COBOLToken = tokens[i];
            if (token.tokenNameLower === "filler") {
                continue;
            }

            switch (token.tokenType) {
                case COBOLTokenStyle.Union:
                    {
                        const srange = new vscode.Position(token.startLine, token.startColumn);
                        const uri = vscode.Uri.file(token.filename);
                        locations.push(new vscode.Location(uri, srange));
                        break;
                    }
                case COBOLTokenStyle.Constant:
                    {
                        const srange = new vscode.Position(token.startLine, token.startColumn);
                        const uri = vscode.Uri.file(token.filename);
                        locations.push(new vscode.Location(uri, srange));
                        break;
                    }
                case COBOLTokenStyle.ConditionName:
                    {
                        const srange = new vscode.Position(token.startLine, token.startColumn);
                        const uri = vscode.Uri.file(token.filename);
                        locations.push(new vscode.Location(uri, srange));
                        break;
                    }
                case COBOLTokenStyle.Variable:
                    {
                        const srange = new vscode.Position(token.startLine, token.startColumn);
                        const uri = vscode.Uri.file(token.filename);
                        locations.push(new vscode.Location(uri, srange));
                        break;
                    }
            }
        }

        if (locations.length === 0) {
            return false;
        }
        return true;
    }

    private getGenericTarget(queryRegEx: RegExp, tokenMap: Map<string, COBOLToken>, document: vscode.TextDocument, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, queryRegEx);
        const word = wordRange ? document.getText(wordRange) : '';
        if (word === "") {
            return undefined;
        }

        const workLower = word.toLowerCase();
        if (tokenMap.has(workLower)) {
            const token: COBOLToken | undefined = tokenMap.get(workLower);
            if (token !== undefined) {
                const srange = new vscode.Position(token.startLine, token.startColumn);
                const uri = vscode.Uri.file(token.filename);
                return new vscode.Location(uri, srange);
            }
        }
        return undefined;
    }

    private getClassTarget(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
        return this.getGenericTarget(this.classRegEx, sf.classes, document, position);
    }

    private getMethodTarget(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
        return this.getGenericTarget(this.methodRegEx, sf.methods, document, position);
    }

}
