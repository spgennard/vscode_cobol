import { ICOBOLSettings, intellisenseStyle } from "./iconfiguration";

export class VSCustomIntelliseRules {
    public static Default: VSCustomIntelliseRules = new VSCustomIntelliseRules();

    private customRule = new Map<string, intellisenseStyle>();
    private customStartsWithRule = new Map<string, intellisenseStyle>();

    constructor() {
        VSCustomIntelliseRules.Default = this;
    }

    public reFreshConfiguration(settings: ICOBOLSettings) {
        this.customRule.clear();
        for (const ruleString of settings.custom_intellisense_rules) {
            const colonPos = ruleString.indexOf(":");
            if (colonPos !== -1) {
                const key = ruleString.substring(0, colonPos);
                const kRule = ruleString.charAt(1 + colonPos);
                let rule = intellisenseStyle.Unchanged;
                switch (kRule) {
                    case "u": rule = intellisenseStyle.UpperCase; break;
                    case "l": rule = intellisenseStyle.LowerCase; break;
                    case "c": rule = intellisenseStyle.CamelCase; break;
                    case "=": rule = intellisenseStyle.Unchanged; break;
                }
                if (key.endsWith("*")) {
                    const sKey = key.substring(0, key.length - 1);
                    this.customStartsWithRule.set(sKey, rule);
                } else {
                    this.customRule.set(key, rule);
                }
            }
        }
    }

    public findCustomIStyle(settings: ICOBOLSettings, key: string, defaultStyle: intellisenseStyle): intellisenseStyle {
        this.reFreshConfiguration(settings);
        const keyLower = key.toLowerCase();
        if (this.customRule.has(keyLower)) {
            const iStyle = this.customRule.get(keyLower);
            if (iStyle !== undefined) {
                return iStyle as intellisenseStyle;
            }
        } else {
            for (const [partialKey, iStyle] of this.customStartsWithRule) {
                if (key.startsWith(partialKey)) {
                    return iStyle;
                }
            }
        }
        return defaultStyle;
    }

} 