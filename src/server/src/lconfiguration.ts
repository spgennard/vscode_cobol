'use strict';

import { ICOBOLSettings, COBOLSettings, outlineFlag } from './iconfiguration';

import * as path from 'path';
import * as fs from 'fs';
import { connectionWorkspace } from './server';


export class VSLCOBOLConfiguration {
    private static config : ICOBOLSettings;

    public static init() {
        VSLCOBOLConfiguration.config = new COBOLSettings();
        VSLCOBOLConfiguration.config.experimential_features = getExperimentialFeatures();
        VSLCOBOLConfiguration.config.enable_tabstop = isTabstopEnabled();
        VSLCOBOLConfiguration.config.ignorecolumn_b_onwards = getColumBParsing();
        VSLCOBOLConfiguration.config.copybooks_nested = getCopybookNestedInSection();
        VSLCOBOLConfiguration.config.fuzzy_variable_search = getFuzzyVariableSearch();
        VSLCOBOLConfiguration.config.cache_metadata = getCachingSetting();
        VSLCOBOLConfiguration.config.outline = isOutlineEnabled();
        VSLCOBOLConfiguration.config.copybookdirs = getCopybookdirs_defaults();
        VSLCOBOLConfiguration.config.pre_parse_line_limit = getPreParseLineLimit();
        VSLCOBOLConfiguration.config.copybookexts = getCopybookExts();
        VSLCOBOLConfiguration.config.tabstops = getTabStops();
    }
    
    public static get(): ICOBOLSettings {
        return VSLCOBOLConfiguration.config;
    }

    public static getExperimentialFeatures(): boolean {
        return VSLCOBOLConfiguration.config.experimential_features;
    }

    public static isTabstopEnabled(): boolean {
        return VSLCOBOLConfiguration.config.enable_tabstop;
    }

    public static getPreParseLineLimit() : number {
        return VSLCOBOLConfiguration.config.pre_parse_line_limit;
    }

    public static getColumBParsing() : boolean {
        return VSLCOBOLConfiguration.config.ignorecolumn_b_onwards;
    }

    public static getCopybookNestedInSection() : boolean {
        return VSLCOBOLConfiguration.config.copybooks_nested;
    }

    public static getFuzzyVariableSearch() : boolean {
        return VSLCOBOLConfiguration.config.fuzzy_variable_search;
    }
    
    public static getCachingSetting() : string {
        return VSLCOBOLConfiguration.config.cache_metadata;
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
        return VSLCOBOLConfiguration.config.outline;
    }
    
    public static getCopybookdirs_defaults(): string[] {
        return VSLCOBOLConfiguration.config.copybookdirs;
    }

    public static getExtentions() : string[] {
        return VSLCOBOLConfiguration.config.copybookexts;
    }

    public static getTabStops(): number[] {
        return VSLCOBOLConfiguration.config.tabstops;
    }
}

function getBoolean(configSection: string, defaultValue: boolean) : boolean {
    // var editorConfig = connectionWorkspace.getConfiguration('coboleditor.'+configSection);

    // var expEnabled = editorConfig.get<boolean>(configSection);
    // if (expEnabled === undefined || expEnabled === null) {
    //     expEnabled = defaultValue;
    // }
    // return expEnabled; 

    return defaultValue;
}

function getExperimentialFeatures(): boolean {
    return getBoolean('experimential_features', false);
}

function isTabstopEnabled(): boolean {
    return getBoolean('enable_tabstop', false);
}

function getPreParseLineLimit(): number {
    // var editorConfig = workspace.getConfiguration('coboleditor');
    // var lineLimit = editorConfig.get<number>('pre_parse_line_limit');
    // if (lineLimit === undefined || lineLimit === null) {
    //     lineLimit = 25;
    // }
    // return lineLimit;

    return 25;
}


function getColumBParsing(): boolean {
    return getBoolean('ignorecolumn_b_onwards', false);
}

function getCopybookNestedInSection(): boolean {
    return getBoolean('copybooks_nested', false);
}


function getFuzzyVariableSearch(): boolean {
    return getBoolean('fuzzy_variable_search', false);
}


function getCachingSetting(): string {
    // var editorConfig = workspace.getConfiguration('coboleditor');
    // var cacheEnum = editorConfig.get<string>('cache_metadata');

    // if (cacheEnum === undefined || cacheEnum === null) {
    //     return "";
    // }

    // return cacheEnum;

    return "";
}


function isOutlineEnabled(): outlineFlag {
    // var editorConfig = workspace.getConfiguration('coboleditor');
    // var outlineEnabled = editorConfig.get('outline');
    // if (outlineEnabled === undefined || outlineEnabled === null) {
    //     return outlineFlag.On;
    // }

    // switch (outlineEnabled) {
    //     case "on": return outlineFlag.On;
    //     case "off": return outlineFlag.Off;
    //     case "partial": return outlineFlag.Partial;
    //     case "skeleton": return outlineFlag.Skeleton;
    // }
    return outlineFlag.On;
}
const DEFAULT_COPYBOOK_DIR = ["."];


function getCopybookdirs_defaults(): string[] {

    return DEFAULT_COPYBOOK_DIR;

    // let editorConfig = workspace.getConfiguration('coboleditor');
    // let dirs = editorConfig.get<string[]>('copybookdirs');
    // if (!dirs || (dirs !== null && dirs.length === 0)) {
    //     dirs = DEFAULT_COPYBOOK_DIR;
    // }

    // let extraDirs: string[] = [];

    // for (let dirpos = 0; dirpos < dirs.length; dirpos++) {
    //     let dir = dirs[dirpos];

    //     if (dir.startsWith("$")) {
    //         var e = process.env[dir.substr(1)];
    //         if (e !== undefined && e !== null) {
    //             e.split(path.delimiter).forEach(function (item) {
    //                 if (item !== undefined && item !== null && item.length > 0) {
    //                     if (fs.existsSync(item)) {
    //                         var itemStat = fs.statSync(item);
    //                         if (itemStat.isDirectory()) {
    //                             extraDirs.push(item);
    //                         }
    //                     }
    //                 }
    //             });
    //         }
    //     } else {
    //         extraDirs.push(dir);
    //     }
    // }

    // return extraDirs;
}

const DEFAULT_COPYBOOK_EXTS = ["cpy"];

function getCopybookExts(): string[] {
    // var editorConfig = workspace.getConfiguration('coboleditor');
    // editorConfig = editorConfig;
    // var extensions = editorConfig.get<string[]>('copybookexts');
    // if (!extensions || (extensions !== null && extensions.length === 0)) {
    //     extensions = DEFAULT_COPYBOOK_EXTS;
    // }
    // extensions.push("");
    // return extensions;

    return DEFAULT_COPYBOOK_EXTS;
}

const DEFAULT_RULER = [0, 7,  11,  15,  19,  23,  27,  31, 35, 39,  43,  47, 51,  55, 59,  63,  67,  71,  75,  79];

function getTabStops(): number[] {
    // let editorConfig =  workspace.getConfiguration('coboleditor');
    // let tabStops = editorConfig.get<number[]>('tabstops');
    // if (!tabStops || (tabStops !== null && tabStops.length === 0)) {
    //     tabStops = DEFAULT_RULER;
    // }
    // return tabStops;

    return DEFAULT_RULER;
}

