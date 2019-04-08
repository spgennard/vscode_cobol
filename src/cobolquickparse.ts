import ISourceHandler from "./isourcehandler";
import { cobolKeywordDictionary } from "./keywords/cobolKeywords";

import { workspace } from 'vscode';

export enum COBOLTokenStyle {
    CopyBook = "Copybook",
    ProgramId = "Program-Id",
    FunctionId = "Function-Id",
    MethodId = "Method-Id",
    ClassId = "Class-Id",
    InterfaceId = "Interface-Id",
    ValueTypeId = "Valuetype-Id",
    EnumId = "Enum-id",
    Section = "Section",
    Paragraph = "Paragraph",
    Division = "Division",
    EntryPoint = "Entry",
    Variable = "Variable",

    Null = "Null"
}

enum COBOLTokenEnding {
    OneLIne,
    NextSection
}

class COBOLToken {
    public tokenType: COBOLTokenStyle;
    public startLine: number;
    public startColumn: number;
    public token: string;
    public description: string;
    public level: number;
    public parentToken: COBOLToken | undefined;
    public endLine: number;
    public endColumn: number;
    public tokenEnding: COBOLTokenEnding;

    public childTokens: COBOLToken[] = [];

    static Null: COBOLToken = new COBOLToken(COBOLTokenStyle.Null, -1, -1, "", "", undefined, COBOLTokenEnding.OneLIne);

    public constructor(tokenType: COBOLTokenStyle, startLine: number, startColumn: number, token:string, description: string, parentToken: COBOLToken | undefined, tokenEnding: COBOLTokenEnding) {
        this.tokenType = tokenType;
        this.startLine = startLine;
        this.startColumn = startColumn;
        this.description = description;
        this.endLine = this.endColumn = 0;
        this.level = (parentToken === undefined) ? 1 : 1 + parentToken.level;
        this.parentToken = parentToken;
        this.tokenEnding = tokenEnding;
        this.token = token;
    }

    public dump() {
        let prefix = "";
        for (let i = 1; i < this.level; i++) {
            prefix += " ";
        }
        console.log(prefix + this.tokenType + "=>" +
            this.startLine + ":" + this.startColumn + "<=>" +
            this.endLine + ":" + this.endColumn +
            " is [" + this.description + "]");
    }
}

export default class QuickCOBOLParse {
    public divisions: COBOLToken[] = [];

    public tokensInOrder: COBOLToken[] = [];
    private highestLine: number = 0;
    
    public isValidLiteral(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }
        let regex = /^[a-zA-Z][a-zA-Z0-9-_]*/g;

        if (id.match(regex)) {
            return true;
        }

        return false;
    }

    public isValidQuotedLiteral(id: string): boolean {

        if (id === null || id.length === 0) {
            return false;
        }
        id = id.replace(/\"/g, "");
        id = id.replace(/\'/g, "");

        return this.isValidLiteral(id);
    }

    inProcedureDivision: boolean;
    inWorkingStorage: boolean;
    currentDivision: COBOLToken;
    procedureDivsion: COBOLToken;
    parseColumnBOnwards: boolean = this.getColumBParsing();

    public constructor(sourceHandler: ISourceHandler) {
        this.inProcedureDivision = false;
        this.inWorkingStorage = false;
        this.currentDivision = COBOLToken.Null;
        this.procedureDivsion = COBOLToken.Null;

        for (let l = 0; l < sourceHandler.getLineCount(); l++) {
            this.parseLineByLine(sourceHandler, l);
        }
        this.updateEndings(sourceHandler);
    }

    private isInterestingToken(token: string): boolean {
        switch (token) {
            case "entry":
            case "copy":
            case "program-id":
            case "class-id":
            case "valuetype-id":
            case "method-id":
            case "entry-point":
            case "enum-id":
                return true;
        }

        return false;
    }

    private isValidKeyword(keyword: string): boolean {
        return cobolKeywordDictionary.containsKey(keyword);
    }

    private isNumber(value: string | number): boolean {
        if (value.toString().length === 0) {
            return false;
        }
        return !isNaN(Number(value.toString()));
    }

    private trimLiteral(literal: string) {
        let literalTrimmed = literal.trim();

        /* remove quotes */
        if (literalTrimmed.startsWith("\"") && literalTrimmed.endsWith("\"")) {
            return literal.substr(1, literalTrimmed.length - 2);
        }

        /* remove quotes */
        if (literalTrimmed.startsWith("\'") && literalTrimmed.endsWith("\'")) {
            return literal.substr(1, literalTrimmed.length - 2);
        }
        return literalTrimmed;
    }
    private getColumBParsing(): boolean {
        var editorConfig = workspace.getConfiguration('coboleditor');
        var parsingB = editorConfig.get<boolean>('ignorecolumn_b_onwards');
        if (parsingB === undefined || parsingB === null) {
            parsingB = false;
        }
        return parsingB;
    }

    private parseLineByLine(sourceHandler: ISourceHandler, lineNumber: number) {

        let line: string = sourceHandler.getLine(lineNumber);
        // console.log("Line : "+lineNumber + "[" + line + "]");
        let lineTokens = line.split(/[\s \,]/g);

        if (lineNumber > this.highestLine) {
            this.highestLine = lineNumber;
        }

        let prev = "";
        let prevColumn: number = 0;
        let rollingColumn = 0;
        let skip2Characer = null;

        for (let i = 0; i < lineTokens.length; i++) {
            let current: string = lineTokens[i].toLocaleLowerCase();
            let endWithDot = false;

            // continue now
            if (current.length === 0) {
                continue;
            }
            let quoteIndex = current.indexOf("\"");
            if (skip2Characer !== null && quoteIndex === -1) {
                continue;
            }

            // HACK for "set x to entry"
            if (prev === "to" && current === "entry") {
                prev = "";
                i++;
                continue;
            }

            // do we have a quote?
            if (quoteIndex !== -1 && (!this.isInterestingToken(prev))) {
                /* start? */
                if (skip2Characer === null) {
                    skip2Characer = '"';
                    /* drop the right hand side */
                    current = current.substr(0, quoteIndex);
                } else {
                    skip2Characer = null;
                    /* drop the left hand side */
                    current = current.substr(1 + quoteIndex);
                }
                continue;
            }

            let currentCol = line.indexOf(lineTokens[i], rollingColumn);

            if (current.endsWith(".")) {
                current = current.substr(0, current.length - 1);
                endWithDot = true;
            }

            rollingColumn = currentCol + lineTokens[i].length;
            let prevPlusCurrent = line.substr(prevColumn, (currentCol + current.length) - prevColumn);
            // let currentPlusNext = line.substr(currentCol, (nextColumn + next.length) - currentCol);

            // handle sections
            if (prev.length !== 0 && current === "section" && (prev !== 'exit')) {
                if (prev === "declare") {
                    continue;
                }
                let token = new COBOLToken(COBOLTokenStyle.Section, lineNumber, prevColumn, prevPlusCurrent, prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne);
                this.currentDivision.childTokens.push(token);
                this.tokensInOrder.push(token);

                if (prev === "working-storage") {
                    this.inWorkingStorage = true;
                    this.inProcedureDivision = false;
                    sourceHandler.setDumpAreaA(false);
                    sourceHandler.setDumpAreaBOnwards(!this.parseColumnBOnwards);
                }

                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle divisions
            if (prev.length !== 0 && current === "division") {
                let token = new COBOLToken(COBOLTokenStyle.Division, lineNumber, prevColumn, prevPlusCurrent, prevPlusCurrent, COBOLToken.Null, COBOLTokenEnding.NextSection);
                this.divisions.push(token);
                this.tokensInOrder.push(token);
                this.currentDivision = token;

                if (prev === "procedure") {
                    this.inProcedureDivision = true;
                    this.inWorkingStorage = false;
                    this.procedureDivsion = token;
                    sourceHandler.setDumpAreaA(true);
                    sourceHandler.setDumpAreaBOnwards(false);
                }

                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle entries
            if (prev === "entry" && current.length !== 0) {
                let token =  new COBOLToken(COBOLTokenStyle.EntryPoint, lineNumber, currentCol, this.trimLiteral(current), prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne);
                this.divisions.push(token);
                this.tokensInOrder.push(token);
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle program-id
            if (prev === "program-id" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.ProgramId, lineNumber, currentCol, this.trimLiteral(current), prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle class-id
            if (prev === "class-id" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.ClassId, lineNumber, currentCol, prevPlusCurrent, prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle enum-id
            if (prev === "enum-id" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.EnumId, lineNumber, currentCol, prevPlusCurrent, prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle interface-id
            if (prev === "interface-id" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.InterfaceId, lineNumber, currentCol, prevPlusCurrent, prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle valuetype-id
            if (prev === "valuetype-id" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.ValueTypeId, lineNumber, currentCol, prevPlusCurrent, prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle function-id
            if (prev === "function-id" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.FunctionId, lineNumber, currentCol, prevPlusCurrent, prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // handle method-id
            if (prev === "method-id" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.MethodId, lineNumber, currentCol, prevPlusCurrent, prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }
            // copybook handling
            if (prev === "copy" && current.length !== 0) {
                this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.CopyBook, lineNumber, currentCol, this.trimLiteral(current), prevPlusCurrent, this.currentDivision, COBOLTokenEnding.OneLIne));
                prev = current;
                prevColumn = currentCol;
                continue;
            }

            // we are in the procedure division
            if (this.currentDivision === this.procedureDivsion && endWithDot) {
                if (!this.isValidKeyword(prev) && !this.isValidKeyword(current)) {
                    let beforeCurrent = line.substr(0, currentCol - 1).trim();
                    if (beforeCurrent.length === 0) {
                        let c = lineTokens[i].substr(0, lineTokens[i].length - 1);
                        if (c.length !== 0) {
                            this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.Paragraph, lineNumber, currentCol-1, c, c, this.currentDivision, COBOLTokenEnding.OneLIne));
                            prev = current;
                            prevColumn = currentCol;
                            continue;
                        }
                    }
                }
            }

            // are we in the working-storage section?
            if (this.inWorkingStorage) {
                /* only interesteding in things that are after a number */
                if (this.isNumber(prev) && !this.isNumber(current)) {
                    if (!this.isValidKeyword(prev) && !this.isValidKeyword(current)) {
                        let c = lineTokens[i].substr(0, lineTokens[i].length);
                        if (c.length !== 0) {
                            if (c[c.length - 1] === '.') {
                                c= lineTokens[i].substr(0, lineTokens[i].length-1);
                            }
                            this.tokensInOrder.push(new COBOLToken(COBOLTokenStyle.Variable, lineNumber, currentCol, c, c, this.currentDivision, COBOLTokenEnding.OneLIne));
                            prev = current;
                            prevColumn = currentCol;
                            continue;
                        }
                    }
                }
            }
            prevColumn = currentCol;
            prev = current;

        }
    }

    private updateEndings(sourceHandler: ISourceHandler) {
        for (let i = 0; i < this.divisions.length; i++) {
            let token = this.divisions[i];
            if (1 + i < this.divisions.length) {
                let nextToken = this.divisions[i + 1];
                token.endLine = nextToken.startLine - 1;          /* use the end of the previous line */
                let theLastToken = sourceHandler.getLine(token.endLine);
                token.endColumn = theLastToken.length;
            } else {
                token.endLine = this.highestLine;
                token.endColumn = 0;
            }
        }

        for (let i = 0; i < this.divisions.length; i++) {
            let division = this.divisions[i];
            let sections = division.childTokens;
            for (let i = 0; i < sections.length; i++) {
                let token = sections[i];
                if (1 + i < sections.length) {
                    let nextToken = sections[i + 1];
                    token.endLine = nextToken.startLine - 1;          /* use the end of the previous line */
                    let theLastToken = sourceHandler.getLine(token.endLine);
                    token.endColumn = theLastToken.length;
                } else {
                    token.endLine = division.endLine; // -1
                    token.endColumn = sourceHandler.getLine(division.endLine - 1).length;
                }
            }
        }

        for (let i = 0; i < this.tokensInOrder.length; i++) {
            let token = this.tokensInOrder[i];
            if (token.endLine === 0 && token.tokenEnding === COBOLTokenEnding.OneLIne) {
                token.endLine = token.startLine;
                token.endColumn = token.startColumn + token.description.length;
            }
        }
    }

    public dump() {
        for (var i = 0; i < this.tokensInOrder.length; i++) {
            let token = this.tokensInOrder[i];
            token.dump();
        }
    }
}


