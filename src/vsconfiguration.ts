/* eslint-disable @typescript-eslint/naming-convention */
"use strict";

import { TextDocument, Uri, workspace, WorkspaceConfiguration } from "vscode";
import { ICOBOLSettings, COBOLSettings, outlineFlag, IEditorMarginFiles, hoverApi, intellisenseStyle, fileformatStrategy, IAnchorTabInfo } from "./iconfiguration";
import { IExternalFeatures } from "./externalfeatures";
import { ExtensionDefaults } from "./extensionDefaults";
import { COBOLFileUtils } from "./fileutils";
import { VSCOBOLUtils } from "./vscobolutils";

export class WorkspaceSettings {
    public settings: IVSCOBOLSettings;
    public files = new Map<string, Uri>();
    public firstResource: Uri;

    constructor(settings: IVSCOBOLSettings, firstResource: Uri) {
        this.settings = settings;
        this.firstResource = firstResource;
    }
}
const InMemoryCache_Settings: Map<string, WorkspaceSettings> = new Map<string, WorkspaceSettings>();

export class VSCOBOLEditorConfiguration {

    public static getEditorConfig(): WorkspaceConfiguration {
        return workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    }


    public static getResourceEditorConfig(resource: Uri): WorkspaceConfiguration {
           if (resource.scheme === 'file') {
            return workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig, resource);
        }
        else {
            return VSCOBOLEditorConfiguration.getEditorConfig();
        }
    }
}

// export function cloneObject<T>(a: T): T {
//     return JSON.parse(JSON.stringify(a));
// }

export interface IVSCOBOLSettings extends ICOBOLSettings {
    id: number;
    create_from_document: boolean;
}

export class VSCOBOLSettings extends COBOLSettings implements IVSCOBOLSettings {

    id: number;
    create_from_document: boolean;

    public constructor(id: number, create_from_document:boolean) {
        super();

        this.id = id;
        this.create_from_document = create_from_document;
 
    }
}

export class VSCOBOLConfiguration {
    private static _settings: IVSCOBOLSettings = new VSCOBOLSettings(0, false);

    private static initSettings(editorConfig: WorkspaceConfiguration, settings: IVSCOBOLSettings, externalFeatures: IExternalFeatures): ICOBOLSettings {
        const editorHelper = new VSCOBOLConfigurationHelper(editorConfig);

        settings.enable_tabstop = editorHelper.getBoolean("enable_tabstop", false);
        settings.copybooks_nested = editorHelper.getBoolean("copybooks_nested", false);
        settings.outline = editorHelper.isOutlineEnabled();
        settings.config_copybookdirs = editorConfig.get<string[]>("copybookdirs", []);
        settings.copybookdirs = editorHelper.getCopybookdirs_defaults(externalFeatures, settings.invalid_copybookdirs, settings.perfile_copybookdirs);
        settings.pre_scan_line_limit = editorHelper.getPreScanLineLimit();
        settings.copybookexts = editorHelper.getCopybookExts();
        settings.program_extensions = editorHelper.getProgram_extensions();
        settings.tabstops = editorHelper.getTabStops();
        settings.linter = editorHelper.getBoolean("linter", false);
        settings.line_comment = editorHelper.getBoolean("line_comment", false);
        settings.fileformat_strategy = editorConfig.get<fileformatStrategy>("fileformat_strategy", fileformatStrategy.Normal);
        settings.enable_data_provider = editorHelper.getBoolean("enable_data_provider", true);
        settings.disable_unc_copybooks_directories = editorHelper.getBoolean("disable_unc_copybooks_directories", false);
        settings.intellisense_item_limit = editorHelper.getIntellisense_item_limit();
        settings.process_metadata_cache_on_start = editorHelper.getBoolean("process_metadata_cache_on_start", false);
        // vsconfig.cache_metadata = getcache_metadata();
        settings.cache_metadata_inactivity_timeout = editorHelper.getNumber("cache_metadata_inactivity_timeout", 5000);
        settings.cache_metadata_verbose_messages = editorHelper.getBoolean("cache_metadata_verbose_messages", false);
        settings.parse_copybooks_for_references = editorHelper.getBoolean("parse_copybooks_for_references", false);
        settings.workspacefolders_order = editorHelper.getWorkspacefolders_order();
        settings.linter_unused_sections = editorHelper.getBoolean("linter_unused_sections", true);
        settings.linter_unused_paragraphs = editorHelper.getBoolean("linter_unused_paragraphs", true);
        settings.linter_house_standards = editorHelper.getBoolean("linter_house_standards", true);
        settings.linter_house_standards_rules = editorHelper.getlinter_house_standards_rules();
        settings.linter_mark_as_information = editorHelper.getBoolean("linter_mark_as_information", true);
        settings.linter_ignore_section_before_entry = editorHelper.getBoolean("linter_ignore_section_before_entry", true);
        settings.linter_ignore_missing_copybook = editorHelper.getBoolean("linter_ignore_missing_copybook", false);
        settings.linter_ignore_malformed_using = editorHelper.getBoolean("linter_ignore_malformed_using", true);
        settings.linter_port_helper = editorHelper.getBoolean("linter_port_helper", true);

        // scan for comments can cause a file access.. so it cannot be trusted
        settings.scan_comments_for_hints = !workspace.isTrusted ? false : editorHelper.getBoolean("scan_comments_for_hints", false);

        settings.scan_comment_copybook_token = editorHelper.getscan_comment_copybook_token();
        settings.editor_maxTokenizationLineLength = workspace.getConfiguration("editor").get<number>("maxTokenizationLineLength", 20000);

        settings.sourceview = editorHelper.getBoolean("sourceview", false);
        settings.sourceview_include_jcl_files = editorHelper.getBoolean("sourceview_include_jcl_files", true);
        settings.sourceview_include_hlasm_files = editorHelper.getBoolean("sourceview_include_hlasm_files", true);
        settings.sourceview_include_pli_files = editorHelper.getBoolean("sourceview_include_pli_files", true);
        settings.sourceview_include_doc_files = editorHelper.getBoolean("sourceview_include_doc_files", true);
        settings.sourceview_include_script_files = editorHelper.getBoolean("sourceview_include_script_files", true);
        settings.sourceview_include_object_files = editorHelper.getBoolean("sourceview_include_object_files", true);
        settings.sourceview_include_test_files = editorHelper.getBoolean("sourceview_include_test_files", true);
        settings.format_on_return = editorConfig.get<boolean>("format_on_return", false);
        settings.intellisense_style = editorConfig.get<intellisenseStyle>("intellisense_style", intellisenseStyle.Unchanged);
        settings.maintain_metadata_cache = editorHelper.getBoolean("maintain_metadata_cache", true);
        settings.maintain_metadata_recursive_search = editorHelper.getBoolean("maintain_metadata_recursive_search", false);
        settings.metadata_symbols = editorHelper.getmetadata_symbols(settings);
        settings.metadata_entrypoints = editorHelper.getmetadata_entrypoints(settings);
        settings.metadata_types = editorHelper.getmetadata_types(settings);
        settings.metadata_files = editorHelper.getmetadata_files(settings);
        settings.metadata_knowncopybooks = editorHelper.getmetadata_knowncopybooks(settings);
        settings.enable_semantic_token_provider = editorHelper.getBoolean("enable_semantic_token_provider", false);
        settings.enable_text_replacement = editorHelper.getBoolean("enable_text_replacement", false);
        settings.editor_margin_files = editorHelper.getFileFormatConfiguration();

        settings.enable_source_scanner = editorHelper.getBoolean("enable_source_scanner", true);

        const user_cobol_language_ids = editorConfig.get<string[]>("valid_cobol_language_ids", settings.valid_cobol_language_ids);
        let valid = true;

        for (const languageId of user_cobol_language_ids) {
            // same regex as package.json
            const validReg = new RegExp("(^^COBOL$|^COBOLIT$|^ACUCOBOL$|^RMCOBOL|^BITLANG-COBOL$|^COBOL_MF_LISTFILE$)", "gm");
            if (!validReg.test(languageId)) {
                valid = false;
            }
        }

        if (valid) {
            settings.valid_cobol_language_ids = user_cobol_language_ids;
            settings.valid_cobol_language_ids_for_intellisense = [...user_cobol_language_ids];
        }

        settings.files_exclude = editorConfig.get<string[]>("files_exclude", settings.files_exclude);

        settings.scan_line_limit = editorConfig.get<number>("scan_line_limit", settings.scan_line_limit);

        settings.scan_time_limit = editorConfig.get<number>("scan_time_limit", settings.scan_time_limit);

        settings.in_memory_cache_size = editorConfig.get<number>("in_memory_cache_size", settings.in_memory_cache_size);

        settings.suggest_variables_when_context_is_unknown = editorConfig.get<boolean>("suggest_variables_when_context_is_unknown", settings.suggest_variables_when_context_is_unknown);

        settings.hover_show_known_api = editorConfig.get<hoverApi>("hover_show_known_api", settings.hover_show_known_api);

        settings.enable_comment_tags = editorConfig.get<boolean>("enable_comment_tags", settings.enable_comment_tags);

        settings.comment_tag_word = editorConfig.get<boolean>("comment_tag_word", settings.comment_tag_word);

        settings.snippets = editorConfig.get<boolean>("snippets", settings.snippets);

        settings.enable_columns_tags = editorConfig.get<boolean>("enable_columns_tags", settings.enable_columns_tags);

        settings.hover_show_encoded_literals = editorConfig.get<boolean>("hover_show_encoded_literals", settings.hover_show_encoded_literals);

        settings.check_file_format_before_file_scan = editorConfig.get<boolean>("check_file_format_before_file_scan", settings.check_file_format_before_file_scan);

        if (!workspace.isTrusted) {
            VSCOBOLConfiguration.adjustForUntructedEnv(settings);
        }

        settings.intellisense_add_space_keywords = editorConfig.get<string[]>("intellisense_add_space_keywords", settings.intellisense_add_space_keywords);

        settings.custom_intellisense_rules = editorConfig.get<string[]>("custom_intellisense_rules", settings.custom_intellisense_rules);

        settings.format_constants_to_uppercase = editorHelper.getBoolean("format_constants_to_uppercase", settings.format_constants_to_uppercase);

        settings.margin = editorHelper.getBoolean("margin", settings.margin);

        settings.margin_identification_area = editorHelper.getBoolean("margin_identification_area", settings.margin_identification_area);

        settings.enable_codelens_variable_references = editorHelper.getBoolean("enable_codelens_variable_references", settings.enable_codelens_variable_references);

        settings.enable_codelens_section_paragraph_references = editorHelper.getBoolean("enable_codelens_section_paragraph_references", settings.enable_codelens_section_paragraph_references);

        settings.enable_codelens_copy_replacing = editorHelper.getBoolean("enable_codelens_copy_replacing", settings.enable_codelens_copy_replacing);

        // use the enable_source_scanner to turn off other features.. aka lets dum things down
        if (settings.enable_source_scanner === false) {
            settings.parse_copybooks_for_references = false;
            settings.process_metadata_cache_on_start = false;
            settings.enable_text_replacement = false;
        }

        settings.hover_show_variable_definition = editorHelper.getBoolean("hover_show_variable_definition", settings.hover_show_variable_definition);

        settings.out_of_range_tabstop_size = editorHelper.getNumber("out_of_range_tabstop_size", settings.out_of_range_tabstop_size);

        settings.anchor_tabstops = editorHelper.getIAnchorTabInfo();

        settings.enable_tabstops_anchors = editorHelper.getBoolean("enable_tabstops_anchors", settings.enable_tabstops_anchors);

        settings.enable_rocket_cobol_lsp_when_active = editorHelper.getBoolean("enable_rocket_cobol_lsp_when_active", settings.enable_rocket_cobol_lsp_when_active);

        settings.enable_rocket_cobol_lsp_lang_server_control = editorHelper.getBoolean("enable_rocket_cobol_lsp_lang_server_control", settings.enable_rocket_cobol_lsp_lang_server_control);

        settings.enable_exec_sql_cursors = editorHelper.getBoolean("enable_exec_sql_cursors", settings.enable_exec_sql_cursors);

        settings.scan_comments_for_references = editorHelper.getBoolean("scan_comments_for_references", settings.scan_comments_for_references);

        settings.scan_comment_for_ls_control = editorHelper.getBoolean("scan_comment_for_ls_control", settings.scan_comments_for_references);

        settings.scan_comment_begin_ls_ignore = editorHelper.getString("scan_comment_begin_ls_ignore", settings.scan_comment_begin_ls_ignore);

        settings.scan_comment_end_ls_ignore = editorHelper.getString("scan_comment_end_ls_ignore", settings.scan_comment_end_ls_ignore);

        settings.enable_program_information = editorHelper.getBoolean("enable_program_information", settings.enable_program_information);
        return settings;
    }

    static adjustForUntructedEnv(settings: IVSCOBOLSettings): void {
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

    public static get_workspace_settings(): IVSCOBOLSettings {
        return VSCOBOLConfiguration._settings;
    }

    public static get_resource_settings(document: TextDocument, externalFeatures: IExternalFeatures): IVSCOBOLSettings {
        return VSCOBOLConfiguration.get_resource_settings_via_uri(document.uri, document.version, externalFeatures);
    }

    public static get_resource_settings_via_uri(documentUri: Uri, id: number, externalFeatures: IExternalFeatures): IVSCOBOLSettings {
        const workspaceDirectory = workspace.getWorkspaceFolder(documentUri);
        if (workspaceDirectory === undefined) {
            return VSCOBOLConfiguration._settings;
        }

        const workspaceDirectoryString = workspaceDirectory.uri.fsPath;
        const path2wd = documentUri.fsPath;
        let cachedSettings = InMemoryCache_Settings.get(workspaceDirectoryString);
        if (cachedSettings !== undefined) {

            if (cachedSettings.files.get(path2wd) === undefined) {
                cachedSettings.files.set(path2wd, documentUri);
            }
            return cachedSettings.settings;
        }

        const editorConfig = VSCOBOLEditorConfiguration.getResourceEditorConfig(documentUri);
        const config = new VSCOBOLSettings(id, true);
        VSCOBOLConfiguration.initSettings(editorConfig, config, externalFeatures);
        VSCOBOLUtils.setupFilePaths(config);
        VSCOBOLUtils.setupUrlPathsSync(config);
        const workspaceSettings = new WorkspaceSettings(config, documentUri);
        workspaceSettings.files.set(path2wd, documentUri);

        InMemoryCache_Settings.set(workspaceDirectoryString, workspaceSettings);
        return config;
    }

    public static clearResourceCache(document: TextDocument) {
        const workspaceDirectory = workspace.getWorkspaceFolder(document.uri);
        if (workspaceDirectory === undefined) {
            return;
        }

        const workspaceDirectoryString = workspaceDirectory.uri.fsPath;
        let cachedSettings = InMemoryCache_Settings.get(workspaceDirectoryString);
        if (cachedSettings !== undefined) {
            const path2wd = document.uri.fsPath;
            if (cachedSettings.files.get(path2wd) !== undefined) {
                cachedSettings.files.delete(path2wd);
            }

            if (cachedSettings.files.size === 0) {
                InMemoryCache_Settings.delete(workspaceDirectoryString);
            }
        }
    }

    public static reinitWorkspaceSettings(externalFeatures: IExternalFeatures): IVSCOBOLSettings {
        const editorConfig = VSCOBOLEditorConfiguration.getEditorConfig();
        VSCOBOLConfiguration.initSettings(editorConfig, VSCOBOLConfiguration._settings, externalFeatures);
        return VSCOBOLConfiguration._settings;
    }

    public static reinitWorkspaceSettingsScoped(externalFeatures: IExternalFeatures): void {
        for (const workspace of InMemoryCache_Settings.values()) {
            const editorConfig = VSCOBOLEditorConfiguration.getResourceEditorConfig(workspace.firstResource);
            VSCOBOLConfiguration.initSettings(editorConfig, workspace.settings, externalFeatures);
        }

        VSCOBOLConfiguration.reinitWorkspaceSettings(externalFeatures);
    }
}


class VSCOBOLConfigurationHelper {
    readonly editorConfig: WorkspaceConfiguration;
    readonly DEFAULT_COPYBOOK_DIR: string[] = [];
    readonly DEFAULT_COPYBOOK_EXTS = ["cpy", "scr", "CPY", "SCR", "cbl", "CBL", "ccp", "dds", "ss", "wks"];
    readonly DEFAULT_PROGRAM_EXTS = ["cob", "COB", "cbl", "CBL", "cobol", "scbl", "pco"];
    readonly DEFAULT_RULER = [0, 7, 11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79];

    constructor(editorConfig: WorkspaceConfiguration) {
        this.editorConfig = editorConfig;
    }

    public getIAnchorTabInfo(): IAnchorTabInfo[] {
        const files: IAnchorTabInfo[] | undefined = this.editorConfig.get<IAnchorTabInfo[]>("tabstops_anchors");
        if (files === undefined || files === null) {
            return [];
        }

        return files;
    }

    public getFileFormatConfiguration(): IEditorMarginFiles[] {
        const files: IEditorMarginFiles[] | undefined = this.editorConfig.get<IEditorMarginFiles[]>("fileformat");
        if (files === undefined || files === null) {
            return [];
        }

        return files;
    }

    public getBoolean(configSection: string, defaultValue: boolean): boolean {
        let expEnabled = this.editorConfig.get<boolean>(configSection);
        if (expEnabled === undefined || expEnabled === null) {
            expEnabled = defaultValue;
        }
        return expEnabled;
    }

    public getNumber(configSection: string, defaultValue: number): number {
        let lineLimit = this.editorConfig.get<number>(configSection);
        if (lineLimit === undefined || lineLimit === null) {
            lineLimit = defaultValue;
        }
        return lineLimit;
    }

    public getString(configSection: string, defaultValue: string): string {
        let expEnabled = this.editorConfig.get<string>(configSection);
        if (expEnabled === undefined || expEnabled === null) {
            expEnabled = defaultValue;
        }
        return expEnabled;
    }

    public getPreScanLineLimit(): number {
        let lineLimit = this.editorConfig.get<number>("pre_scan_line_limit");
        if (lineLimit === undefined || lineLimit === null) {
            lineLimit = 25;
        }
        return lineLimit;
    }

    public getscan_comment_copybook_token(): string {
        let hintToken = this.editorConfig.get<string>("scan_comment_copybook_token");
        if (hintToken === undefined || hintToken === null) {
            hintToken = "source-dependency";
        }
        return hintToken;
    }

    public getIntellisense_item_limit(): number {
        let itemLimit = this.editorConfig.get<number>("intellisense_item_limit");
        if (itemLimit === undefined || itemLimit === null) {
            itemLimit = 0;
        }
        return itemLimit;
    }

    public isOutlineEnabled(): outlineFlag {
        const outlineEnabled = this.editorConfig.get("outline");
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

    public expandEnvVars(startEnv: string): string {
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

    public getPathDelimiter(): string {
        return process.platform === "win32" ? ";" : ":";
    }

    public getCopybookdirs_defaults(externalFeatures: IExternalFeatures, invalidSearchDirectory: string[], perFileDirs: string[]): string[] {
        let dirs = this.editorConfig.get<string[]>("copybookdirs");
        if (!dirs || (dirs !== null && dirs.length === 0)) {
            dirs = this.DEFAULT_COPYBOOK_DIR;
        }

        const extraDirs: string[] = [];

        for (let dirpos = 0; dirpos < dirs.length; dirpos++) {
            let dir = dirs[dirpos];
            if (COBOLFileUtils.isDirectPath(dir)) {
                externalFeatures.logMessage(` non portable copybook directory ${dir} defined`);
            }

            /* expand ${env:}} */
            dir = this.expandEnvVars(dir);

            // eslint-disable-next-line no-template-curly-in-string
            if (dir.startsWith("${workspaceFolder}")) {
                // eslint-disable-next-line no-template-curly-in-string
                dir = dir.replace("${workspaceFolder}", "").trim();

                // remove / or \ forward
                if (dir.startsWith("/") || dir.startsWith("\\")) {
                    dir = dir.substring(1).trim();
                }
            }

            // does it container a per file element?
            if (dir.indexOf("${fileDirname}") !== -1) {
                perFileDirs.push(dir);
                continue;
            }

            // ignore empty elements
            if (dir.length !== 0) {
                if (dir.startsWith("$")) {
                    const e = process.env[dir.substring(1)];
                    if (e !== undefined && e !== null) {
                        e.split(this.getPathDelimiter()).forEach(function (item) {
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

    public getCopybookExts(): string[] {
        let extensions = this.editorConfig.get<string[]>("copybookexts");
        if (!extensions || (extensions !== null && extensions.length === 0)) {
            extensions = this.DEFAULT_COPYBOOK_EXTS;
        }
        extensions.push("");
        return extensions;
    }

    public getProgram_extensions(): string[] {
        let extensions = this.editorConfig.get<string[]>("program_extensions");
        if (!extensions || (extensions !== null && extensions.length === 0)) {
            extensions = this.DEFAULT_PROGRAM_EXTS;
        }
        return extensions;
    }

    public getTabStops(): number[] {
        let tabStops = this.editorConfig.get<number[]>("tabstops");
        if (!tabStops || (tabStops !== null && tabStops.length === 0)) {
            tabStops = this.DEFAULT_RULER;
        }
        return tabStops;
    }

    public getWorkspacefolders_order(): string[] {
        let dirs = this.editorConfig.get<string[]>("workspacefolders_order");
        if (!dirs || (dirs !== null && dirs.length === 0)) {
            dirs = [];
        }
        return dirs;
    }


    public getlinter_house_standards_rules(): string[] {
        let standards = this.editorConfig.get<string[]>("linter_house_standards_rules");
        if (!standards || (standards !== null && standards.length === 0)) {
            standards = [];
        }
        return standards;
    }

    public getmetadata_symbols(settings: ICOBOLSettings): string[] {
        if (settings.maintain_metadata_cache === false) {
            return [];
        }
        let symbols = this.editorConfig.get<string[]>("metadata_symbols");
        if (!symbols || (symbols !== null && symbols.length === 0)) {
            symbols = [];
        }
        return symbols;
    }


    public getmetadata_entrypoints(settings: ICOBOLSettings): string[] {
        if (settings.maintain_metadata_cache === false) {
            return [];
        }
        let entrypoints = this.editorConfig.get<string[]>("metadata_entrypoints");
        if (!entrypoints || (entrypoints !== null && entrypoints.length === 0)) {
            entrypoints = [];
        }
        return entrypoints;
    }

    public getmetadata_types(settings: ICOBOLSettings): string[] {
        if (settings.maintain_metadata_cache === false) {
            return [];
        }
        let metadata_types = this.editorConfig.get<string[]>("metadata_types");
        if (!metadata_types || (metadata_types !== null && metadata_types.length === 0)) {
            metadata_types = [];
        }
        return metadata_types;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getmetadata_files(config: ICOBOLSettings): string[] {
        if (config.maintain_metadata_cache === false) {
            return [];
        }

        let metadata_files = this.editorConfig.get<string[]>("metadata_files");
        if (!metadata_files || (metadata_files !== null && metadata_files.length === 0)) {
            metadata_files = [];
        }

        return metadata_files;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public getmetadata_knowncopybooks(config: ICOBOLSettings): string[] {
        if (config.maintain_metadata_cache === false) {
            return [];
        }

        let metadata_knowncopybooks = this.editorConfig.get<string[]>("metadata_knowncopybooks");
        if (!metadata_knowncopybooks || (metadata_knowncopybooks !== null && metadata_knowncopybooks.length === 0)) {
            metadata_knowncopybooks = [];
        }

        return metadata_knowncopybooks;
    }
}
