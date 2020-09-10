'use strict';

import { workspace } from 'vscode';
import { ICOBOLSettings, COBOLSettings, outlineFlag } from './iconfiguration';
import * as path from 'path';
import { isDirectory } from './extension';
import { getWorkspaceFolders } from './cobolfolders';


export class VSCOBOLConfiguration {
    private static config: ICOBOLSettings = new COBOLSettings();

    public static init(): ICOBOLSettings {
        const vsconfig = VSCOBOLConfiguration.config;

        vsconfig.experimental_features = getExperimentialFeatures();
        vsconfig.enable_tabstop = isTabstopEnabled();
        vsconfig.ignorecolumn_b_onwards = getColumBParsing();
        vsconfig.copybooks_nested = getCopybookNestedInSection();
        vsconfig.fuzzy_variable_search = getFuzzyVariableSearch();
        vsconfig.cache_metadata = getCachingSetting();
        vsconfig.outline = isOutlineEnabled();
        vsconfig.copybookdirs = getCopybookdirs_defaults(vsconfig.invalid_copybookdirs);
        vsconfig.pre_parse_line_limit = getPreParseLineLimit();
        vsconfig.copybookexts = getCopybookExts();
        vsconfig.tabstops = getTabStops();
        vsconfig.linter = getLinter();
        vsconfig.line_comment = getline_comment();
        vsconfig.fileformat_strategy = getFileformatStrategy();
        vsconfig.enable_data_provider = getEnable_data_provider();
        vsconfig.disable_unc_copybooks_directories = getDisable_unc_copybooks_directories();
        vsconfig.intellisense_include_unchanged = getIntellisense_include_unchanged();
        vsconfig.intellisense_include_camelcase = getintellisense_include_camelcase();
        vsconfig.intellisense_include_uppercase = getIntellisense_include_uppercase();
        vsconfig.intellisense_include_lowercase = getIntellisense_include_lowercase();
        vsconfig.intellisense_item_limit = getIntellisense_item_limit();
        vsconfig.process_metadata_cache_on_start = getProcess_metadata_cache_on_start();
        vsconfig.cache_directory_strategy = getCache_directory_strategy();
        vsconfig.parse_copybooks_for_references = getParse_copybooks_for_references();
        vsconfig.copybookdirs_order = getCopybookdirs_order();
        vsconfig.linter_unused_paragraphs_or_sections = getLinter_unused_paragraphs_or_sections();
        vsconfig.linter_house_standards = getLinter_house_standards();
        vsconfig.linter_house_standards_rules = getlinter_house_standards_rules();
        vsconfig.linter_mark_as_information = getLinter_mark_as_information();
        vsconfig.ignore_unsafe_extensions = getIgnore_unsafe_extensions();
        vsconfig.coboldoc_workspace_folder = getCoboldoc_workspace_folder();
        return vsconfig;
    }

    public static get(): ICOBOLSettings {
        return VSCOBOLConfiguration.config;
    }

    public static getExperimentialFeatures(): boolean {
        return VSCOBOLConfiguration.config.experimental_features;
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
        if (getWorkspaceFolders()) {
            return VSCOBOLConfiguration.config.cache_metadata;
        }
        return "off";
    }

    public static isCachingEnabled(): boolean {
        if (getWorkspaceFolders()) {
            const cacheEnum = getCachingSetting();

            switch (cacheEnum) {
                case "on": return true;
                case "off": return false;
            }
        }
        return false;
    }

    public static isOnDiskCachingEnabled(): boolean {
        const cacheEnum = getCachingSetting();
        switch (cacheEnum) {
            case "on": return true;
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
    const editorConfig = workspace.getConfiguration('coboleditor');
    let expEnabled = editorConfig.get<boolean>(configSection);
    if (expEnabled === undefined || expEnabled === null) {
        expEnabled = defaultValue;
    }
    return expEnabled;
}

function getExperimentialFeatures(): boolean {
    return getBoolean('experimental_features', false);
}

function isTabstopEnabled(): boolean {
    return getBoolean('enable_tabstop', false);
}

function getPreParseLineLimit(): number {
    const editorConfig = workspace.getConfiguration('coboleditor');
    let lineLimit = editorConfig.get<number>('pre_parse_line_limit');
    if (lineLimit === undefined || lineLimit === null) {
        lineLimit = 25;
    }
    return lineLimit;
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

function getLinter(): boolean {
    return getBoolean('linter', false);
}

function getLinter_unused_paragraphs_or_sections(): boolean {
    return getBoolean("linter_unused_paragraphs_or_sections", true);
}

function getFuzzyVariableSearch(): boolean {
    return getBoolean('fuzzy_variable_search', false);
}

function getEnable_data_provider() : boolean {
    return getBoolean('enable_data_provider', true);
}
function getDisable_unc_copybooks_directories(): boolean {
    return getBoolean('disable_unc_copybooks_directories', false);

}

function getIntellisense_include_unchanged():boolean {
    return getBoolean("intellisense_include_unchanged", true);
}

function getintellisense_include_camelcase(): boolean {
    return getBoolean("intellisense_include_camelcase",false);
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

function getLinter_house_standards(): boolean {
    return getBoolean("linter_house_standards", true);
}

function getLinter_mark_as_information(): boolean {
    return getBoolean("linter_mark_as_information", true);
}

function getIgnore_unsafe_extensions() : boolean {
    return getBoolean("ignore_unsafe_extensions", false);
}

function getCoboldoc_workspace_folder(): string {
    const editorConfig = workspace.getConfiguration('coboleditor');
    const coboldoc_folder = editorConfig.get<string>('coboldoc_workspace_folder');
    if (coboldoc_folder === undefined || coboldoc_folder === null) {
        return "coboldoc";
    }
    return coboldoc_folder;
}

function getCache_directory_strategy(): string {
    const editorConfig = workspace.getConfiguration('coboleditor');
    const cacheDirStrategy = editorConfig.get<string>('cache_directory_strategy');

    if (cacheDirStrategy === undefined || cacheDirStrategy === null) {
        return "workspace";
    }

    return cacheDirStrategy;
}

function getIntellisense_item_limit(): number {
    const editorConfig = workspace.getConfiguration('coboleditor');
    let itemLimit = editorConfig.get<number>('intellisense_item_limit');
    if (itemLimit === undefined || itemLimit === null) {
        itemLimit = 0;
    }
    return itemLimit;

}

function getCachingSetting(): string {
    const editorConfig = workspace.getConfiguration('coboleditor');
    const cacheEnum = editorConfig.get<string>('cache_metadata');

    if (cacheEnum === undefined || cacheEnum === null) {
        return "";
    }

    return cacheEnum;
}

function getFileformatStrategy(): string {
    const editorConfig = workspace.getConfiguration('coboleditor');
    const fileStrat = editorConfig.get<string>('fileformat_strategy');

    if (fileStrat === undefined || fileStrat === null) {
        return "normal";
    }

    return fileStrat;
}


function isOutlineEnabled(): outlineFlag {
    const editorConfig = workspace.getConfiguration('coboleditor');
    const outlineEnabled = editorConfig.get('outline');
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
    let complete = false;
    let env: string = startEnv;

    while(complete === false) {
        const indexOfEnv = env.indexOf("${env:");
        if (indexOfEnv === -1) {
            complete = true;
        } else {
            const lenOfValue = env.indexOf("}") - (indexOfEnv+6);
            const envValue = env.substr(6+indexOfEnv,lenOfValue);
            const left = env.substr(0,indexOfEnv);
            const right = env.substr(1+env.indexOf("}"));
            env = left + process.env[envValue]+right;
        }
    }

    return env;
}

function getCopybookdirs_defaults(invalidSearchDirectory: string[]): string[] {
    const editorConfig = workspace.getConfiguration('coboleditor');
    let dirs = editorConfig.get<string[]>('copybookdirs');
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }

    const extraDirs: string[] = [];

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
                const e = process.env[dir.substr(1)];
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
    const editorConfig = workspace.getConfiguration('coboleditor');
    let extensions = editorConfig.get<string[]>('copybookexts');
    if (!extensions || (extensions !== null && extensions.length === 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    extensions.push("");
    return extensions;
}

const DEFAULT_RULER = [0, 7, 11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79];

function getTabStops(): number[] {
    const editorConfig = workspace.getConfiguration('coboleditor');
    let tabStops = editorConfig.get<number[]>('tabstops');
    if (!tabStops || (tabStops !== null && tabStops.length === 0)) {
        tabStops = DEFAULT_RULER;
    }
    return tabStops;
}

function getCopybookdirs_order():string[] {
    const editorConfig = workspace.getConfiguration('coboleditor');
    let dirs = editorConfig.get<string[]>('copybookdirs_order');
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = [];
    }
    return dirs;
}


function getlinter_house_standards_rules(): string[] {
    const editorConfig = workspace.getConfiguration('coboleditor');
    let standards = editorConfig.get<string[]>('linter_house_standards_rules');
    if (!standards || (standards !== null && standards.length === 0)) {
        standards = [];
    }
    return standards;
}