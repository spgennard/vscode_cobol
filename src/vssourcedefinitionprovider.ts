import * as vscode from "vscode";
import { COBOLSourceScanner, COBOLTokenStyle, COBOLToken, COBOLVariable } from "./cobolsourcescanner";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { ICOBOLSettings } from "./iconfiguration";
import { VSLogger } from "./vslogger";
import { getCOBOLKeywordDictionary } from "./keywords/cobolKeywords";

export class COBOLSourceDefinition implements vscode.DefinitionProvider {

    readonly sectionRegEx = new RegExp("[0-9a-zA-Z][a-zA-Z0-9-_]*");
    readonly variableRegEx = new RegExp("[#0-9a-zA-Z_][a-zA-Z0-9-_]*");
    readonly classRegEx = new RegExp("[0-9a-zA-Z][a-zA-Z0-9-_]*");
    readonly methodRegEx = new RegExp("[0-9a-zA-Z][a-zA-Z0-9-_]*");

    public provideDefinition(document: vscode.TextDocument,
        position: vscode.Position,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        token: vscode.CancellationToken): vscode.ProviderResult<vscode.Definition> {
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
        if (this.getVariableInCurrentDocument(locations, document, position, settings)) {
            return locations;
        }

        return locations;
    }

    private getSectionOrParaLocation(document: vscode.TextDocument, sf: COBOLSourceScanner, position: vscode.Position): vscode.Location | undefined {
        const wordRange = document.getWordRangeAtPosition(position, this.sectionRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return undefined;
        }

        const wordLower = word.toLowerCase();

        try {

            if (sf.sections.has(wordLower)) {
                const token = sf.sections.get(wordLower);
                if (token !== undefined) {
                    const spos = new vscode.Position(token.startLine, token.startColumn);
                    const rpos = new vscode.Position(token.endLine, token.endColumn);
                    const trange = new vscode.Range(spos, rpos);
                    const uri = vscode.Uri.parse(token.filenameAsURI);
                    return new vscode.Location(uri, trange);
                }
            }

        }
        catch (e) {
            VSLogger.logMessage((e as Error).message);
        }

        try {
            if (sf.paragraphs.has(wordLower)) {
                const token = sf.paragraphs.get(wordLower);
                if (token !== undefined) {
                    const spos = new vscode.Position(token.startLine, token.startColumn);
                    const rpos = new vscode.Position(token.endLine, token.endColumn);
                    const trange = new vscode.Range(spos, rpos);
                    const uri = vscode.Uri.parse(token.filenameAsURI);
                    return new vscode.Location(uri, trange);
                }
            }
        }
        catch (e) {
            VSLogger.logMessage((e as Error).message);
        }
        return undefined;
    }

    private getVariableInCurrentDocument(locations: vscode.Location[], document: vscode.TextDocument, position: vscode.Position, settings: ICOBOLSettings): boolean {
        const wordRange = document.getWordRangeAtPosition(position, this.variableRegEx);
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return false;
        }

        const tokenLower: string = word.toLowerCase();
        if (getCOBOLKeywordDictionary(document.languageId).has(tokenLower)) {
            return false;
        }

        const sf: COBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(document, settings);
        if (sf === undefined) {
            return false;
        }

        const variables: COBOLVariable[] | undefined = sf.constantsOrVariables.get(tokenLower);
        if (variables === undefined || variables.length === 0) {
            return false;
        }

        for (let i = 0; i < variables.length; i++) {
            const token: COBOLToken = variables[i].token;
            if (token.tokenNameLower === "filler") {
                continue;
            }

            const spos = new vscode.Position(token.startLine, token.startColumn);
            const rpos = new vscode.Position(token.endLine, token.endColumn);
            const trange = new vscode.Range(spos, rpos);
            const uri = vscode.Uri.parse(token.filenameAsURI);
            switch (token.tokenType) {
                case COBOLTokenStyle.Union:
                    {
                        locations.push(new vscode.Location(uri, trange));
                        break;
                    }
                case COBOLTokenStyle.Constant:
                    {
                        locations.push(new vscode.Location(uri, trange));
                        break;
                    }
                case COBOLTokenStyle.ConditionName:
                    {
                        locations.push(new vscode.Location(uri, trange));
                        break;
                    }
                case COBOLTokenStyle.Variable:
                    {
                        locations.push(new vscode.Location(uri, trange));
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
        const word = wordRange ? document.getText(wordRange) : "";
        if (word === "") {
            return undefined;
        }

        const workLower = word.toLowerCase();
        if (tokenMap.has(workLower)) {
            const token: COBOLToken | undefined = tokenMap.get(workLower);
            if (token !== undefined) {
                const spos = new vscode.Position(token.startLine, token.startColumn);
                const rpos = new vscode.Position(token.endLine, token.endColumn);
                const trange = new vscode.Range(spos, rpos);
                const uri = vscode.Uri.parse(token.filenameAsURI);
                return new vscode.Location(uri, trange);
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
