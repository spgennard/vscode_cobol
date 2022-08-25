import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";


export class VSCustomIntelliseRules {
    public static Default: VSCustomIntelliseRules;

    private customRule = new Map<string, intellisenseStyle>();

    constructor(settings: ICOBOLSettings) {
        VSCustomIntelliseRules.Default = this;
        this.reFreshConfiguration(settings);
    }

    public reFreshConfiguration(settings: ICOBOLSettings) {
        this.customRule.clear();
        for (const ruleString of settings.custom_intellisense_rules) {
            const colonPos = ruleString.indexOf(":");
            if (colonPos !== -1) {
                const key = ruleString.substring(0,colonPos);
                const kRule = ruleString.charAt(1+colonPos);
                let rule = intellisenseStyle.Unchanged;
                switch(kRule) {
                    case "u":  rule=intellisenseStyle.UpperCase; break;
                    case "l":  rule=intellisenseStyle.LowerCase; break;
                    case "c":  rule=intellisenseStyle.CamelCase; break;
                    case "U":  rule=intellisenseStyle.Unchanged; break;
                }
                this.customRule.set(key,rule);
            }
        }
    }

    public getCustomIStyle(settings: ICOBOLSettings, key: string): intellisenseStyle {
        const keyLower = key.toLowerCase();
        if (this.customRule.has(keyLower)) {
            const iStyle = this.customRule.get(keyLower);
            if (iStyle !== undefined) {
                return iStyle;
            }
        }
        return settings.intellisense_style;
    }

} 