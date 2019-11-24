export enum outlineFlag {
    On = "on",
    Off = "off",
    Partial = "partial",
    Skeleton = "skeleton"
}


export interface ICOBOLSettings {
    ExperimentialFeatures: boolean;
    TabstopEnabled: boolean;
    PreParseLineLimit: number;
    ColumBParsing: boolean;
    CopybookNestedInSection:boolean;
    FuzzyVariableSearch: boolean;
    CachingSetting: string;
    OutlineEnabled: outlineFlag;
    Copybookdirs_defaults: string[];
    CopybookExts: string[];
    TabStops: number[];
}


export class COBOLSettings implements ICOBOLSettings {
    public ExperimentialFeatures: boolean;
    public TabstopEnabled: boolean;
    public PreParseLineLimit: number;
    public ColumBParsing: boolean;
    public CopybookNestedInSection: boolean;
    public FuzzyVariableSearch: boolean;
    public CachingSetting: string;
    public OutlineEnabled: outlineFlag;
    public Copybookdirs_defaults: string[];
    public CopybookExts: string[];
    public TabStops: number[];

    constructor() {
        this.ExperimentialFeatures = false;
        this.TabstopEnabled = true;
        this.PreParseLineLimit = 25;
        this.ColumBParsing = false;
        this.CopybookNestedInSection = false;
        this.FuzzyVariableSearch = false;
        this.CachingSetting = "off";
        this.OutlineEnabled = outlineFlag.Off;
        this.Copybookdirs_defaults = [];
        this.CopybookExts = [];
        this.TabStops = [];
    }
}

export class COBOLSettingsHelper {
    public static isCachingEnabled(setting: ICOBOLSettings): boolean {
        var cacheEnum = setting.CachingSetting;
    
        switch (cacheEnum) {
            case "on": return true;
            case "partial": return true;
            case "off": return false;
        }
        return false;
    }
    
    public static isCachingSetToON(setting: ICOBOLSettings): boolean {
        var cacheEnum = setting.CachingSetting;
        switch (cacheEnum) {
            case "on": return true;
            case "partial": return false;
            case "off": return false;
        }
        return false;
    }

}