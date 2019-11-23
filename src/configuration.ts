'use strict';

import { commands, workspace, StatusBarItem, StatusBarAlignment, DecorationOptions, Range, ExtensionContext, languages, TextDocument, TextEditor, Position, CancellationToken, ProviderResult, Definition, window, Hover, OutputChannel, extensions } from 'vscode';

import { ICOBOLSettings, COBOLSettings, outlineFlag } from './iconfiguration';

import * as path from 'path';
import * as fs from 'fs';


export class COBOLConfiguration {
    private static config : ICOBOLSettings;

    public static init() {
        COBOLConfiguration.config = new COBOLSettings();
        COBOLConfiguration.config.ExperimentialFeatures = getExperimentialFeatures();
        COBOLConfiguration.config.TabstopEnabled = isTabstopEnabled();
        COBOLConfiguration.config.ColumBParsing = getColumBParsing();
        COBOLConfiguration.config.CopybookNestedInSection = getCopybookNestedInSection();
        COBOLConfiguration.config.FuzzyVariableSearch = getFuzzyVariableSearch();
        COBOLConfiguration.config.CachingSetting = getCachingSetting();
        COBOLConfiguration.config.OutlineEnabled = isOutlineEnabled();
        COBOLConfiguration.config.Copybookdirs_defaults = getCopybookdirs_defaults();
        COBOLConfiguration.config.CopybookExts = getCopybookExts();
        COBOLConfiguration.config.TabStops = getTabStops();
    }
    
    public static get(): ICOBOLSettings {
        return COBOLConfiguration.config;
    }

    public static getExperimentialFeatures(): boolean {
        return COBOLConfiguration.config.ExperimentialFeatures;
    }

    public static isTabstopEnabled(): boolean {
        return COBOLConfiguration.config.TabstopEnabled;
    }

    public static getPreParseLineLimit() : number {
        return COBOLConfiguration.config.PreParseLineLimit;
    }

    public static getColumBParsing() : boolean {
        return COBOLConfiguration.config.ColumBParsing;
    }

    public static getCopybookNestedInSection() : boolean {
        return COBOLConfiguration.config.CopybookNestedInSection;
    }

    public static getFuzzyVariableSearch() : boolean {
        return COBOLConfiguration.config.FuzzyVariableSearch;
    }
    
    public static getCachingSetting() : string {
        return COBOLConfiguration.config.CachingSetting;
    }

    public static isCachingEnabled(): boolean {
        var cacheEnum = getCachingSetting();
    
        switch (cacheEnum) {
            case "on": return true;
            case "partial": return true;
            case "off": return false;
        }
        return false;
    }
    
    public static isCachingSetToON(): boolean {
        var cacheEnum = getCachingSetting();
        switch (cacheEnum) {
            case "on": return true;
            case "partial": return false;
            case "off": return false;
        }
        return false;
    }
    
    public static isOutlineEnabled(): outlineFlag {
        return COBOLConfiguration.config.OutlineEnabled;
    }
    
    public static getCopybookdirs_defaults(): string[] {
        return COBOLConfiguration.config.Copybookdirs_defaults;
    }

    public static getExtentions() : string[] {
        return COBOLConfiguration.config.CopybookExts;
    }

    public static getTabStops(): number[] {
        return COBOLConfiguration.config.TabStops;
    }
}

function getExperimentialFeatures(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var expEnabled = editorConfig.get<boolean>('experimential.features');
    if (expEnabled === undefined || expEnabled === null) {
        expEnabled = false;
    }
    return expEnabled;
}

function isTabstopEnabled(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var expEnabled = editorConfig.get<boolean>('enable_tabstop');
    if (expEnabled === undefined || expEnabled === null) {
        expEnabled = false;
    }
    return expEnabled;
}

function getPreParseLineLimit(): number {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var lineLimit = editorConfig.get<number>('pre_parse_line_limit');
    if (lineLimit === undefined || lineLimit === null) {
        lineLimit = 25;
    }
    return lineLimit;
}


function getColumBParsing(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var parsingB = editorConfig.get<boolean>('ignorecolumn_b_onwards');
    if (parsingB === undefined || parsingB === null) {
        parsingB = false;
    }
    return parsingB;
}

function getCopybookNestedInSection(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var nestedFlag = editorConfig.get<boolean>('copybooks_nested');
    if (nestedFlag === undefined || nestedFlag === null) {
        nestedFlag = false;
    }
    return nestedFlag;
}


function getFuzzyVariableSearch(): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var fuzzyVarOn = editorConfig.get<boolean>('fuzzy_variable_search');
    if (fuzzyVarOn === undefined || fuzzyVarOn === null) {
        fuzzyVarOn = false;
    }
    return fuzzyVarOn;
}


function getCachingSetting(): string {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var cacheEnum = editorConfig.get<string>('cache_metadata');

    if (cacheEnum === undefined || cacheEnum === null) {
        return "";
    }

    return cacheEnum;
}


function isOutlineEnabled(): outlineFlag {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var outlineEnabled = editorConfig.get('outline');
    if (outlineEnabled === undefined || outlineEnabled === null) {
        return outlineFlag.On;
    }

    switch (outlineEnabled) {
        case "on": return outlineFlag.On;
        case "off": return outlineFlag.Off;
        case "partial": return outlineFlag.Partial;
        case "skeleton": return outlineFlag.Skeleton;
    }
    return outlineFlag.On;
}
const DEFAULT_COPYBOOK_DIR = ["."];


function getCopybookdirs_defaults(): string[] {
    let editorConfig = workspace.getConfiguration('coboleditor');
    let dirs = editorConfig.get<string[]>('copybookdirs');
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }

    let extraDirs: string[] = [];

    for (let dirpos = 0; dirpos < dirs.length; dirpos++) {
        let dir = dirs[dirpos];

        if (dir.startsWith("$")) {
            var e = process.env[dir.substr(1)];
            if (e !== undefined && e !== null) {
                e.split(path.delimiter).forEach(function (item) {
                    if (item !== undefined && item !== null && item.length > 0) {
                        if (fs.existsSync(item)) {
                            var itemStat = fs.statSync(item);
                            if (itemStat.isDirectory()) {
                                extraDirs.push(item);
                            }
                        }
                    }
                });
            }
        } else {
            extraDirs.push(dir);
        }
    }

    return extraDirs;
}

const DEFAULT_COPYBOOK_EXTS = ["cpy"];

function getCopybookExts(): string[] {
    var editorConfig = workspace.getConfiguration('coboleditor');
    editorConfig = editorConfig;
    var extensions = editorConfig.get<string[]>('copybookexts');
    if (!extensions || (extensions !== null && extensions.length === 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    extensions.push("");
    return extensions;
}

const DEFAULT_RULER = [0, 7,  11,  15,  19,  23,  27,  31, 35, 39,  43,  47, 51,  55, 59,  63,  67,  71,  75,  79];

function getTabStops(): number[] {
    let editorConfig =  workspace.getConfiguration('coboleditor');
    let tabStops = editorConfig.get<number[]>('tabstops');
    if (!tabStops || (tabStops !== null && tabStops.length === 0)) {
        tabStops = DEFAULT_RULER;
    }
    return tabStops;
}

