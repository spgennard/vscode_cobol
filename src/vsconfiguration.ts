/* eslint-disable @typescript-eslint/naming-convention */
"use strict";

import { workspace } from "vscode";
import { ICOBOLSettings, COBOLSettings, outlineFlag, IEditorMarginFiles, hoverApi, intellisenseStyle, fileformatStrategy } from "./iconfiguration";
import { IExternalFeatures } from "./externalfeatures";
import { ExtensionDefaults } from "./extensionDefaults";

export class VSCOBOLConfiguration {
    private static settings: ICOBOLSettings = new COBOLSettings();

    public constructor(externalFeatures: IExternalFeatures) {
        VSCOBOLConfiguration.initSettings(VSCOBOLConfiguration.settings, externalFeatures);
    }

    private static initSettings(settings: ICOBOLSettings, externalFeatures: IExternalFeatures): ICOBOLSettings {
        settings.enable_tabstop = getBoolean("enable_tabstop", false);
        settings.copybooks_nested = getBoolean("copybooks_nested", false);
        settings.outline = isOutlineEnabled();
        settings.copybookdirs = getCopybookdirs_defaults(externalFeatures, settings.invalid_copybookdirs);
        settings.pre_scan_line_limit = getPreScanLineLimit();
        settings.copybookexts = getCopybookExts();
        settings.program_extensions = getProgram_extensions();
        settings.tabstops = getTabStops();
        settings.linter = getBoolean("linter", false);
        settings.line_comment = getBoolean("line_comment", false);
        settings.fileformat_strategy = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<fileformatStrategy>("fileformat_strategy",fileformatStrategy.Normal);
        settings.enable_data_provider = getBoolean("enable_data_provider", true);
        settings.disable_unc_copybooks_directories = getBoolean("disable_unc_copybooks_directories", false);
        settings.intellisense_item_limit = getIntellisense_item_limit();
        settings.process_metadata_cache_on_start = getBoolean("process_metadata_cache_on_start", false);
        // vsconfig.cache_metadata = getcache_metadata();
        settings.cache_metadata_inactivity_timeout = getNumber("cache_metadata_inactivity_timeout", 5000);
        settings.cache_metadata_verbose_messages = getBoolean("cache_metadata_verbose_messages", false);
        settings.parse_copybooks_for_references = getBoolean("parse_copybooks_for_references", false);
        settings.workspacefolders_order = getWorkspacefolders_order();
        settings.linter_unused_paragraphs_or_sections = getBoolean("linter_unused_paragraphs_or_sections", true);
        settings.linter_house_standards = getBoolean("linter_house_standards", true);
        settings.linter_house_standards_rules = getlinter_house_standards_rules();
        settings.linter_mark_as_information = getBoolean("linter_mark_as_information", true);
        settings.linter_ignore_section_before_entry = getBoolean("linter_ignore_section_before_entry", true);
        settings.linter_ignore_missing_copybook = getBoolean("linter_ignore_missing_copybook", false);

        // scan for comments can cause a file access.. so it cannot be trusted
        settings.scan_comments_for_hints = !workspace.isTrusted ? false : getBoolean("scan_comments_for_hints", false);

        settings.scan_comment_copybook_token = getscan_comment_copybook_token();
        settings.editor_maxTokenizationLineLength = workspace.getConfiguration("editor").get<number>("maxTokenizationLineLength", 20000);

        settings.sourceview = getBoolean("sourceview", false);
        settings.sourceview_include_jcl_files = getBoolean("sourceview_include_jcl_files", true);
        settings.sourceview_include_hlasm_files = getBoolean("sourceview_include_hlasm_files", true);
        settings.sourceview_include_pli_files = getBoolean("sourceview_include_pli_files", true);
        settings.sourceview_include_doc_files = getBoolean("sourceview_include_doc_files", true);
        settings.sourceview_include_script_files = getBoolean("sourceview_include_script_files", true);
        settings.sourceview_include_object_files = getBoolean("sourceview_include_object_files", true);
        settings.sourceview_include_test_files = getBoolean("sourceview_include_test_files", true);
        settings.format_on_return = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("format_on_return", false);
        settings.intellisense_style = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<intellisenseStyle>("intellisense_style", intellisenseStyle.Unchanged);
        settings.maintain_metadata_cache = getBoolean("maintain_metadata_cache", true);
        settings.maintain_metadata_recursive_search = getBoolean("maintain_metadata_recursive_search", false);
        settings.metadata_symbols = getmetadata_symbols(settings);
        settings.metadata_entrypoints = getmetadata_entrypoints(settings);
        settings.metadata_types = getmetadata_types(settings);
        settings.metadata_files = getmetadata_files(settings);
        settings.metadata_knowncopybooks = getmetadata_knowncopybooks(settings);
        settings.enable_semantic_token_provider = getBoolean("enable_semantic_token_provider", false);
        settings.enable_text_replacement = getBoolean("enable_text_replacement", false);
        settings.editor_margin_files = getFileFormatConfiguration();

        settings.enable_source_scanner = getBoolean("enable_source_scanner", true);

        const user_cobol_language_ids = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<string[]>("valid_cobol_language_ids", settings.valid_cobol_language_ids);
        let valid = true;

        for (const languageId of user_cobol_language_ids) {
            // same regex as package.json
            const validReg = new RegExp("(^cobol$|^COBOL$|^COBOLIT$|^ACUCOBOL$|^BITLANG-COBOL$|^COBOL_MF_LISTFILE$)", "gm");
            if (!validReg.test(languageId)) {
                valid = false;
            }
        }

        if (valid) {
            settings.valid_cobol_language_ids = user_cobol_language_ids;
        }

        settings.files_exclude = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<string[]>("files_exclude", settings.files_exclude);

        settings.scan_line_limit = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<number>("scan_line_limit", settings.scan_line_limit);

        settings.scan_time_limit = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<number>("scan_time_limit", settings.scan_time_limit);
        
        settings.in_memory_cache_size = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<number>("in_memory_cache_size", settings.in_memory_cache_size);

        settings.suggest_variables_when_context_is_unknown = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("suggest_variables_when_context_is_unknown", settings.suggest_variables_when_context_is_unknown);

        settings.hover_show_known_api = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<hoverApi>("hover_show_known_api", settings.hover_show_known_api);

        settings.enable_comment_tags = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("enable_comment_tags", settings.enable_comment_tags);

        settings.comment_tag_word = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("comment_tag_word", settings.comment_tag_word);

        settings.snippets = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("snippets", settings.snippets);

        settings.enable_columns_tags = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("enable_columns_tags", settings.enable_columns_tags);
        
        settings.hover_show_encoded_literals = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("hover_show_encoded_literals", settings.hover_show_encoded_literals);
        
        settings.check_file_format_before_file_scan = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<boolean>("check_file_format_before_file_scan", settings.check_file_format_before_file_scan);

        if (!workspace.isTrusted) {
            VSCOBOLConfiguration.adjustForUntructedEnv(settings);
        }

        settings.intellisense_no_space_keywords = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<string[]>("intellisense_no_space_keywords", settings.intellisense_no_space_keywords);

        settings.custom_intellisense_rules = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig).get<string[]>("custom_intellisense_rules", settings.custom_intellisense_rules);

        settings.format_constants_to_uppercase = getBoolean("format_constants_to_uppercase", settings.format_constants_to_uppercase);
        
        settings.margin = getBoolean("margin", settings.margin);
        
        // use the enable_source_scanner to turn off other features.. aka lets dum things down
        if (settings.enable_source_scanner === false) {
            settings.parse_copybooks_for_references = false;
            settings.process_metadata_cache_on_start = false;
            settings.enable_text_replacement = false;
        }
        return settings;
    }

    static logCacheMetadataDone = false;
    static logMarginDone = false;

    static adjustForUntructedEnv(settings: ICOBOLSettings): void {
        settings.enable_source_scanner = false;
        settings.disable_unc_copybooks_directories = true;
        settings.process_metadata_cache_on_start = false;
        settings.parse_copybooks_for_references = false;
        // vsconfig.cache_metadata = CacheDirectoryStrategy.Off;
        settings.cache_metadata_verbose_messages = false;
        settings.editor_maxTokenizationLineLength = 0;
        settings.sourceview = false;
        settings.format_on_return = false;

        settings.maintain_metadata_cache = false;
        settings.maintain_metadata_recursive_search = false;
        settings.metadata_symbols = [];
        settings.metadata_entrypoints = [];
        settings.metadata_types = [];
        settings.metadata_files = [];
        settings.enable_semantic_token_provider = false;
        settings.enable_text_replacement = false;
    }

    public static get(): ICOBOLSettings {
        return VSCOBOLConfiguration.settings;
    }

    public static reinit(externalFeatures: IExternalFeatures): ICOBOLSettings {
        VSCOBOLConfiguration.initSettings(VSCOBOLConfiguration.settings, externalFeatures);
        return VSCOBOLConfiguration.get();
    }
}


function getFileFormatConfiguration(): IEditorMarginFiles[] {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    const files: IEditorMarginFiles[] | undefined = editorConfig.get<IEditorMarginFiles[]>("fileformat");
    if (files === undefined || files === null) {
        return [];
    }

    return files;
}

function getBoolean(configSection: string, defaultValue: boolean): boolean {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let expEnabled = editorConfig.get<boolean>(configSection);
    if (expEnabled === undefined || expEnabled === null) {
        expEnabled = defaultValue;
    }
    return expEnabled;
}

function getNumber(configSection: string, defaultValue: number): number {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let lineLimit = editorConfig.get<number>(configSection);
    if (lineLimit === undefined || lineLimit === null) {
        lineLimit = defaultValue;
    }
    return lineLimit;
}

function getPreScanLineLimit(): number {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let lineLimit = editorConfig.get<number>("pre_scan_line_limit");
    if (lineLimit === undefined || lineLimit === null) {
        lineLimit = 25;
    }
    return lineLimit;
}

function getscan_comment_copybook_token(): string {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let hintToken = editorConfig.get<string>("scan_comment_copybook_token");
    if (hintToken === undefined || hintToken === null) {
        hintToken = "source-dependency";
    }
    return hintToken;
}

function getIntellisense_item_limit(): number {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let itemLimit = editorConfig.get<number>("intellisense_item_limit");
    if (itemLimit === undefined || itemLimit === null) {
        itemLimit = 0;
    }
    return itemLimit;
}

function isOutlineEnabled(): outlineFlag {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    const outlineEnabled = editorConfig.get("outline");
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

const DEFAULT_COPYBOOK_DIR: string[] = [];

function expandEnvVars(startEnv: string): string {
    let complete = false;
    let env: string = startEnv;

    while (complete === false) {
        const indexOfEnv = env.indexOf("${env:");
        if (indexOfEnv === -1) {
            complete = true;
        } else {
            const lenOfValue = env.indexOf("}") - (indexOfEnv + 6);
            const envValue = env.substr(6 + indexOfEnv, lenOfValue);
            const left = env.substring(0, indexOfEnv);
            const right = env.substring(1 + env.indexOf("}"));
            env = left + process.env[envValue] + right;
        }
    }

    return env;
}

function getPathDelimiter(): string {
    return process.platform === "win32" ? ";" : ":";
}

function getCopybookdirs_defaults(externalFeatures: IExternalFeatures, invalidSearchDirectory: string[]): string[] {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let dirs = editorConfig.get<string[]>("copybookdirs");
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = DEFAULT_COPYBOOK_DIR;
    }

    const extraDirs: string[] = [];

    for (let dirpos = 0; dirpos < dirs.length; dirpos++) {
        let dir = dirs[dirpos];

        /* remove ${workspaceFolder} */
        dir = expandEnvVars(dir);

        // eslint-disable-next-line no-template-curly-in-string
        if (dir.startsWith("${workspaceFolder}")) {
            // eslint-disable-next-line no-template-curly-in-string
            dir = dir.replace("${workspaceFolder}", "").trim();

            // remove / or \ forward
            if (dir.startsWith("/") || dir.startsWith("\\")) {
                dir = dir.substring(1).trim();
            }
        }

        // ignore empty elements
        if (dir.length !== 0) {
            if (dir.startsWith("$")) {
                const e = process.env[dir.substring(1)];
                if (e !== undefined && e !== null) {
                    e.split(getPathDelimiter()).forEach(function (item) {
                        if (item !== undefined && item !== null && item.length > 0) {
                            if (externalFeatures !== undefined) {
                                if (externalFeatures.isDirectory(item)) {
                                    extraDirs.push(item);
                                } else {
                                    invalidSearchDirectory.push(item);
                                }
                            } else {
                                // just assume it is okay
                                extraDirs.push(item);
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

const DEFAULT_COPYBOOK_EXTS = ["cpy", "scr", "CPY", "SCR", "cbl", "CBL", "ccp", "dds", "ss", "wks"];
const DEFAULT_PROGRAM_EXTS = ["cob", "COB", "cbl", "CBL", "cobol", "scbl", "pco"];

function getCopybookExts(): string[] {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let extensions = editorConfig.get<string[]>("copybookexts");
    if (!extensions || (extensions !== null && extensions.length === 0)) {
        extensions = DEFAULT_COPYBOOK_EXTS;
    }
    extensions.push("");
    return extensions;
}

function getProgram_extensions(): string[] {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let extensions = editorConfig.get<string[]>("program_extensions");
    if (!extensions || (extensions !== null && extensions.length === 0)) {
        extensions = DEFAULT_PROGRAM_EXTS;
    }
    return extensions;
}


const DEFAULT_RULER = [0, 7, 11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79];

function getTabStops(): number[] {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let tabStops = editorConfig.get<number[]>("tabstops");
    if (!tabStops || (tabStops !== null && tabStops.length === 0)) {
        tabStops = DEFAULT_RULER;
    }
    return tabStops;
}

function getWorkspacefolders_order(): string[] {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let dirs = editorConfig.get<string[]>("workspacefolders_order");
    if (!dirs || (dirs !== null && dirs.length === 0)) {
        dirs = [];
    }
    return dirs;
}


function getlinter_house_standards_rules(): string[] {
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let standards = editorConfig.get<string[]>("linter_house_standards_rules");
    if (!standards || (standards !== null && standards.length === 0)) {
        standards = [];
    }
    return standards;
}

function getmetadata_symbols(settings: ICOBOLSettings): string[] {
    if (settings.maintain_metadata_cache === false) {
        return [];
    }
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let symbols = editorConfig.get<string[]>("metadata_symbols");
    if (!symbols || (symbols !== null && symbols.length === 0)) {
        symbols = [];
    }
    return symbols;
}


function getmetadata_entrypoints(settings: ICOBOLSettings): string[] {
    if (settings.maintain_metadata_cache === false) {
        return [];
    }
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let entrypoints = editorConfig.get<string[]>("metadata_entrypoints");
    if (!entrypoints || (entrypoints !== null && entrypoints.length === 0)) {
        entrypoints = [];
    }
    return entrypoints;
}

function getmetadata_types(settings: ICOBOLSettings): string[] {
    if (settings.maintain_metadata_cache === false) {
        return [];
    }
    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let metadata_types = editorConfig.get<string[]>("metadata_types");
    if (!metadata_types || (metadata_types !== null && metadata_types.length === 0)) {
        metadata_types = [];
    }
    return metadata_types;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function getmetadata_files(config: ICOBOLSettings): string[] {
    if (config.maintain_metadata_cache === false) {
        return [];
    }

    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let metadata_files = editorConfig.get<string[]>("metadata_files");
    if (!metadata_files || (metadata_files !== null && metadata_files.length === 0)) {
        metadata_files = [];
    }

    return metadata_files;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function getmetadata_knowncopybooks(config: ICOBOLSettings): string[] {
    if (config.maintain_metadata_cache === false) {
        return [];
    }

    const editorConfig = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    let metadata_knowncopybooks = editorConfig.get<string[]>("metadata_knowncopybooks");
    if (!metadata_knowncopybooks || (metadata_knowncopybooks !== null && metadata_knowncopybooks.length === 0)) {
        metadata_knowncopybooks = [];
    }

    return metadata_knowncopybooks;
}
