import { workspace } from "vscode";

export enum outlineFlag {
    On = "on",
    Off = "off",
    Partial = "partial",
    Skeleton = "skeleton"
}


export interface ICOBOLSettings {
    experimential_features: boolean;
    enable_tabstop: boolean;
    pre_parse_line_limit: number;
    ignorecolumn_b_onwards: boolean;
    copybooks_nested:boolean;
    fuzzy_variable_search: boolean;
    cache_metadata: string;
    outline: outlineFlag;
    copybookdirs: string[];
    invalid_copybookdirs: string[];
    copybookexts: string[];
    tabstops: number[];
    linter: boolean;
    line_comment: boolean;
    fileformat_strategy: string;
    parser_hint_directory: string;
    enable_data_provider:boolean;
    enable_auto_tasks: boolean;
    disable_unc_copybooks_directories: boolean;
    intellisense_include_unchanged: boolean;
    intellisense_include_camalcase:boolean;
    intellisense_include_uppercase: boolean;
    intellisense_include_lowercase:boolean;
    intellisense_item_limit:number;
    process_metadata_cache_on_start:boolean;
    cache_directory_strategy: string;
}


export class COBOLSettings implements ICOBOLSettings {
    public tabstops: number[];
    public copybookexts: string[];
    public copybooks_nested: boolean;
    public experimential_features: boolean;
    public enable_tabstop: boolean;
    public pre_parse_line_limit: number;
    public ignorecolumn_b_onwards: boolean;
    public fuzzy_variable_search: boolean;
    public cache_metadata: string;
    public outline: outlineFlag;
    public copybookdirs: string[];
    public linter: boolean;
    public line_comment: boolean;
    public invalid_copybookdirs: string[];
    public fileformat_strategy: string;
    public parser_hint_directory: string;
    public enable_data_provider: boolean;
    public enable_auto_tasks: boolean;
    public disable_unc_copybooks_directories: boolean;
    public intellisense_include_unchanged:boolean;
    public intellisense_include_camalcase:boolean;
    public intellisense_include_uppercase:boolean;
    public intellisense_include_lowercase:boolean;
    public intellisense_item_limit:number;
    public process_metadata_cache_on_start:boolean;
    public cache_directory_strategy:string;

    constructor() {
        this.experimential_features = false;
        this.enable_tabstop = true;
        this.pre_parse_line_limit = 25;
        this.ignorecolumn_b_onwards = false;
        this.copybooks_nested = false;
        this.fuzzy_variable_search = false;
        this.cache_metadata = "off";
        this.fileformat_strategy = "normal";
        this.outline = outlineFlag.Off;
        this.copybookdirs = [];
        this.copybookexts = [];
        this.invalid_copybookdirs = [];
        this.tabstops = [];
        this.linter = false;
        this.line_comment = false;
        this.parser_hint_directory = "";
        this.enable_data_provider = true;
        this.enable_auto_tasks = false;
        this.disable_unc_copybooks_directories = false;
        this.intellisense_include_unchanged = true;
        this.intellisense_include_camalcase = false;
        this.intellisense_include_uppercase = false;
        this.intellisense_include_lowercase = false;
        this.intellisense_item_limit = 0;
        this.process_metadata_cache_on_start = false;
        this.cache_directory_strategy = "workspace";
    }
}

export class COBOLSettingsHelper {
    public static isCachingEnabled(setting: ICOBOLSettings): boolean {
        if (workspace.workspaceFolders)
        {
            var cacheEnum = setting.cache_metadata;

            switch (cacheEnum) {
                case "on": return true;
                case "partial": return true;
                case "off": return false;
            }
        }
        return false;
    }

    public static isOnDiskCachingEnabled(setting: ICOBOLSettings): boolean {
        if (workspace.workspaceFolders)
        {
            var cacheEnum = setting.cache_metadata;
            switch (cacheEnum) {
                case "on": return true;
                case "partial": return false;
                case "off": return false;
            }
        }
        return false;
    }

}