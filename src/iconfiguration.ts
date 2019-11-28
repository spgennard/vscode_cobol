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
    copybookexts: string[];
    tabstops: number[];
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

    constructor() {
        this.experimential_features = false;
        this.enable_tabstop = true;
        this.pre_parse_line_limit = 25;
        this.ignorecolumn_b_onwards = false;
        this.copybooks_nested = false;
        this.fuzzy_variable_search = false;
        this.cache_metadata = "off";
        this.outline = outlineFlag.Off;
        this.copybookdirs = [];
        this.copybookexts = [];
        this.tabstops = [];
    }
}

export class COBOLSettingsHelper {
    public static isCachingEnabled(setting: ICOBOLSettings): boolean {
        var cacheEnum = setting.cache_metadata;
    
        switch (cacheEnum) {
            case "on": return true;
            case "partial": return true;
            case "off": return false;
        }
        return false;
    }
    
    public static isCachingSetToON(setting: ICOBOLSettings): boolean {
        var cacheEnum = setting.cache_metadata;
        switch (cacheEnum) {
            case "on": return true;
            case "partial": return false;
            case "off": return false;
        }
        return false;
    }

}