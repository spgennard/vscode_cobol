'use strict';

import { workspace } from 'vscode';

import { ICOBOLSettings, COBOLSettings, outlineFlag } from './iconfiguration';

import * as path from 'path';
import * as fs from 'fs';
import { isDirectory } from './extension';


export class VSCOBOLConfiguration {
    private static config: ICOBOLSettings = new COBOLSettings();

    public static init() {
        VSCOBOLConfiguration.config.experimential_features = getExperimentialFeatures();
        VSCOBOLConfiguration.config.enable_tabstop = isTabstopEnabled();
        VSCOBOLConfiguration.config.ignorecolumn_b_onwards = getColumBParsing();
        VSCOBOLConfiguration.config.copybooks_nested = getCopybookNestedInSection();
        VSCOBOLConfiguration.config.fuzzy_variable_search = getFuzzyVariableSearch();
        VSCOBOLConfiguration.config.cache_metadata = getCachingSetting();
        VSCOBOLConfiguration.config.outline = isOutlineEnabled();
        VSCOBOLConfiguration.config.copybookdirs = getCopybookdirs_defaults(VSCOBOLConfiguration.config.invalid_copybookdirs);
        VSCOBOLConfiguration.config.pre_parse_line_limit = getPreParseLineLimit();
        VSCOBOLConfiguration.config.copybookexts = getCopybookExts();
        VSCOBOLConfiguration.config.tabstops = getTabStops();
        VSCOBOLConfiguration.config.linter = geLinter();
        VSCOBOLConfiguration.config.line_comment = getline_comment();
        VSCOBOLConfiguration.config.fileformat_strategy = getFileformatStrategy();
        VSCOBOLConfiguration.config.parser_hint_directory = getparser_hint_directory();
        VSCOBOLConfiguration.config.enable_data_provider = getEnable_data_provider();
        VSCOBOLConfiguration.config.enable_auto_tasks = getEnable_auto_tasks();
        VSCOBOLConfiguration.config.disable_unc_copybooks_directories = getDisable_unc_copybooks_directories();
        VSCOBOLConfiguration.config.intellisense_include_unchanged = getIntellisense_include_unchanged();
        VSCOBOLConfiguration.config.intellisense_include_camalcase = getIntellisense_include_camalcase();
        VSCOBOLConfiguration.config.intellisense_include_uppercase = getIntellisense_include_uppercase();
        VSCOBOLConfiguration.config.intellisense_include_lowercase = getIntellisense_include_lowercase();
        VSCOBOLConfiguration.config.intellisense_item_limit = getIntellisense_item_limit();
        VSCOBOLConfiguration.config.process_metadata_cache_on_start = getProcess_metadata_cache_on_start();
        VSCOBOLConfiguration.config.cache_directory_strategy = getCache_directory_strategy();
        VSCOBOLConfiguration.config.parse_copybooks_for_references = getParse_copybooks_for_references();
    }

    public static get(): ICOBOLSettings {
        return VSCOBOLConfiguration.config;
    }

    public static getEnable_auto_tasks() : boolean
    {
        return VSCOBOLConfiguration.config.enable_auto_tasks;
    }

    public static getExperimentialFeatures(): boolean {
        return VSCOBOLConfiguration.config.experimential_features;
    }

    public static isTabstopEnabled(): boolean {
        return VSCOBOLConfiguration.config.enable_tabstop;
    }

    public static isLineCommentEnabled(): boolean {
        return VSCOBOLConfiguration.config.line_comment;
    }

    public static getPreParseLineLimit(): number {
        return VSCOBOLConfiguration.config.pre_parse_line_limit;
    }

    public static getColumBParsing(): boolean {
        return VSCOBOLConfiguration.config.ignorecolumn_b_onwards;
    }

    public static getCopybookNestedInSection(): boolean {
        return VSCOBOLConfiguration.config.copybooks_nested;
    }

    public static getFuzzyVariableSearch(): boolean {
        return VSCOBOLConfiguration.config.fuzzy_variable_search;
    }

    public static getDisable_unc_copybooks(): boolean {
        return VSCOBOLConfiguration.config.disable_unc_copybooks_directories;
    }

    public static getCachingSetting(): string {
        if (workspace.workspaceFolders) {
            return VSCOBOLConfiguration.config.cache_metadata;
        }
        return "off";
    }

    public static isCachingEnabled(): boolean {
        if (workspace.workspaceFolders) {
            var cacheEnum = getCachingSetting();

            switch (cacheEnum) {
                case "on": return true;
                case "partial": return true;
                case "off": return false;
            }
        }
        return false;
    }

    public static isOnDiskCachingEnabled(): boolean {
        var cacheEnum = getCachingSetting();
        switch (cacheEnum) {
            case "on": return true;
            case "partial": return false;
            case "off": return false;
        }
        return false;
    }

    public static isOutlineEnabled(): outlineFlag {
        return VSCOBOLConfiguration.config.outline;
    }

    public static getCopybookdirs_defaults(): string[] {
        return VSCOBOLConfiguration.config.copybookdirs;
    }

    public static getInvalid_copybookdirs(): string[] {
        return VSCOBOLConfiguration.config.invalid_copybookdirs;
    }

    public static getExtentions(): string[] {
        return VSCOBOLConfiguration.config.copybookexts;
    }

    public static getTabStops(): number[] {
        return VSCOBOLConfiguration.config.tabstops;
    }

    public static getCache_directory_strategy() : string {
        return VSCOBOLConfiguration.config.cache_directory_strategy;
    }
    public static getParse_copybooks_for_references(): boolean {
        return VSCOBOLConfiguration.config.parse_copybooks_for_references;
    }
}

function getBoolean(configSection: string, defaultValue: boolean): boolean {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var expEnabled = editorConfig.get<boolean>(configSection);
    if (expEnabled === undefined || expEnabled === null) {
        expEnabled = defaultValue;
    }
    return expEnabled;
}

function getExperimentialFeatures(): boolean {
    return getBoolean('experimential_features', false);
}

function isTabstopEnabled(): boolean {
    return getBoolean('enable_tabstop', false);
}

function getPreParseLineLimit(): number {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var lineLimit = editorConfig.get<number>('pre_parse_line_limit');
    if (lineLimit === undefined || lineLimit === null) {
        lineLimit = 25;
    }
    return lineLimit;
}

function getparser_hint_directory(): string {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var parserhints = editorConfig.get<string>('parser_hint_directory');

    if (parserhints === undefined || parserhints === null) {
        return "";
    }

    return parserhints;

}
function getline_comment(): boolean {
    return getBoolean("line_comment", false);
}

function getColumBParsing(): boolean {
    return getBoolean('ignorecolumn_b_onwards', false);
}

function getCopybookNestedInSection(): boolean {
    return getBoolean('copybooks_nested', false);
}

function geLinter(): boolean {
    return getBoolean('linter', false);
}

function getFuzzyVariableSearch(): boolean {
    return getBoolean('fuzzy_variable_search', false);
}

function getEnable_data_provider() : boolean {
    return getBoolean('enable_data_provider', true);
}

function getEnable_auto_tasks() : boolean {
    return getBoolean('enable_auto_tasks', false);
}

function getDisable_unc_copybooks_directories(): boolean {
    return getBoolean('disable_unc_copybooks_directories', false);

}

function getIntellisense_include_unchanged():boolean {
    return getBoolean("intellisense_include_unchanged", true);
}

function getIntellisense_include_camalcase(): boolean {
    return getBoolean("intellisense_include_camalcase",false);
}
function getIntellisense_include_uppercase(): boolean {
    return getBoolean("intellisense_include_uppercase", false);
}

function getProcess_metadata_cache_on_start(): boolean {
    return getBoolean("process_metadata_cache_on_start", false);
}

function getParse_copybooks_for_references(): boolean {
    return getBoolean("parse_copybooks_for_references", false);
}
function getIntellisense_include_lowercase(): boolean {
    return getBoolean("intellisense_include_lowercase",false);
}

function getCache_directory_strategy(): string {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var cacheDirStrategy = editorConfig.get<string>('cache_directory_strategy');

    if (cacheDirStrategy === undefined || cacheDirStrategy === null) {
        return "workspace";
    }

    return cacheDirStrategy;
}

function getIntellisense_item_limit(): number {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var itemLimit = editorConfig.get<number>('intellisense_item_limit');
    if (itemLimit === undefined || itemLimit === null) {
        itemLimit = 0;
    }
    return itemLimit;

}

function getCachingSetting(): string {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var cacheEnum = editorConfig.get<string>('cache_metadata');

    if (cacheEnum === undefined || cacheEnum === null) {
        return "";
    }

    return cacheEnum;
}

function getFileformatStrategy(): string {
    var editorConfig = workspace.getConfiguration('coboleditor');
    var fileStrat = editorConfig.get<string>('fileformat_strategy');

    if (fileStrat === undefined || fileStrat === null) {
        return "normal";
    }

    return fileStrat;
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

const DEFAULT_COPYBOOK_DIR:string[] = [];

function expandEnvVars(startEnv: string) : string {
    let complete: boolean = false;
    let env: string = startEnv;

    while(complete === false) {
        let indexOfEnv = env.indexOf("${env:");
        if (indexOfEnv === -1) {
            complete = true;
        } else {
            let lenOfValue = env.indexOf("}") - (indexOfEnv+6);
            let envValue = env.substr(6+indexOfEnv,lenOfValue);
            let left = env.substr(0,indexOfEnv);
            let right = env.substr(1+env.indexOf("}"));
            env = left + process.env[envValue]+right;
        }
    }

    return env;
}

function getCopybookdirs_defaults(invalidSearchDirectory: string[]): string[] {
    let editorConfig = workspace.getConfiguration('coboleditor');
    let dirs = editorConfig.get<string[]>('copybookdirs');
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }

    let extraDirs: string[] = [];

    for (let dirpos = 0; dirpos < dirs.length; dirpos++) {
        let dir = dirs[dirpos];

        /* remove ${workspaceFolder} */
        dir = expandEnvVars(dir);

        if (dir.startsWith("${workspaceFolder}")) {
            dir = dir.replace("${workspaceFolder}", "").trim();

            // remove / or \ forward
            if (dir.startsWith('/') || dir.startsWith('\\')) {
                dir = dir.substr(1).trim();
            }
        }

        // ignore empty elements
        if (dir.length !== 0) {
            if (dir.startsWith("$")) {
                var e = process.env[dir.substr(1)];
                if (e !== undefined && e !== null) {
                    e.split(path.delimiter).forEach(function (item) {
                        if (item !== undefined && item !== null && item.length > 0) {
                            if (isDirectory(item)) {
                                extraDirs.push(item);
                            } else {
                                invalidSearchDirectory.push(item);
                            }
                        }
                    });
                } else {
                    invalidSearchDirectory.push(dir);
                }
            } else {
                if (dir !== ".") {
                    extraDirs.push(dir);
                }
            }
        }
    }

    return extraDirs;
}

const DEFAULT_COPYBOOK_EXTS = ["cpy", "CPY"];

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

const DEFAULT_RULER = [0, 7, 11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79];

function getTabStops(): number[] {
    let editorConfig = workspace.getConfiguration('coboleditor');
    let tabStops = editorConfig.get<number[]>('tabstops');
    if (!tabStops || (tabStops !== null && tabStops.length === 0)) {
        tabStops = DEFAULT_RULER;
    }
    return tabStops;
}
