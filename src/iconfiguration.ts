import { CacheDirectoryStrategy } from "./externalfeatures";

export enum outlineFlag {
    On = "on",
    Off = "off",
    Partial = "partial",
    Skeleton = "skeleton"
}

export interface ICOBOLSettings {
    experimental_features: boolean;
    enable_tabstop: boolean;
    pre_parse_line_limit: number;
    ignorecolumn_b_onwards: boolean;
    copybooks_nested:boolean;
    fuzzy_variable_search: boolean;
    outline: outlineFlag;
    copybookdirs: string[];
    invalid_copybookdirs: string[];
    copybookexts: string[];
    program_extensions: string[];
    tabstops: number[];
    linter: boolean;
    line_comment: boolean;
    fileformat_strategy: string;
    enable_data_provider:boolean;
    disable_unc_copybooks_directories: boolean;
    intellisense_include_unchanged: boolean;
    intellisense_include_camelcase:boolean;
    intellisense_include_uppercase: boolean;
    intellisense_include_lowercase:boolean;
    intellisense_item_limit:number;
    process_metadata_cache_on_start:boolean;
    cache_metadata: CacheDirectoryStrategy;
    cache_metadata_time_limit: number;
    cache_metadata_max_directory_scan_depth: number;
    parse_copybooks_for_references: boolean;
    workspacefolders_order: string[];
    linter_mark_as_information: boolean;
    linter_unused_paragraphs_or_sections: boolean;
    linter_house_standards: boolean;
    linter_house_standards_rules: string[];
    ignore_unsafe_extensions: boolean;
    coboldoc_workspace_folder: string;
    process_scanner_hints_embedded_in_comments:boolean;
    cache_metadata_show_progress_messages: boolean;
    process_scanner_hint_token_for_source_dependancies: string;
    process_metadata_cache_on_file_save: boolean;
    storagearea_id: string;

    editor_maxTokenizationLineLength: number;
    init_required: boolean;
}

export class COBOLSettings implements ICOBOLSettings {
    public tabstops: number[];
    public copybookexts: string[];
    public copybooks_nested: boolean;
    public experimental_features: boolean;
    public enable_tabstop: boolean;
    public pre_parse_line_limit: number;
    public ignorecolumn_b_onwards: boolean;
    public fuzzy_variable_search: boolean;
    public outline: outlineFlag;
    public copybookdirs: string[];
    public linter: boolean;
    public line_comment: boolean;
    public invalid_copybookdirs: string[];
    public fileformat_strategy: string;
    public enable_data_provider: boolean;
    public disable_unc_copybooks_directories: boolean;
    public intellisense_include_unchanged:boolean;
    public intellisense_include_camelcase:boolean;
    public intellisense_include_uppercase:boolean;
    public intellisense_include_lowercase:boolean;
    public intellisense_item_limit:number;
    public process_metadata_cache_on_start:boolean;
    public cache_metadata:CacheDirectoryStrategy;
    public cache_metadata_time_limit: number;
    public cache_metadata_max_directory_scan_depth: number;
    public parse_copybooks_for_references: boolean;
    public workspacefolders_order: string[];
    public linter_mark_as_information: boolean;
    public linter_unused_paragraphs_or_sections: boolean;
    public linter_house_standards: boolean;
    public linter_house_standards_rules: string[];
    public ignore_unsafe_extensions: boolean;
    public coboldoc_workspace_folder: string;
    public program_extensions: string[];
    public process_scanner_hints_embedded_in_comments: boolean;
    public process_scanner_hint_token_for_source_dependancies: string;
    public cache_metadata_show_progress_messages:boolean;
    public process_metadata_cache_on_file_save: boolean;
    public storagearea_id: string;
    public editor_maxTokenizationLineLength: number;

    public init_required = true;

    constructor() {
        this.experimental_features = false;
        this.enable_tabstop = true;
        this.pre_parse_line_limit = 25;
        this.ignorecolumn_b_onwards = false;
        this.copybooks_nested = false;
        this.fuzzy_variable_search = false;
        this.fileformat_strategy = "normal";
        this.outline = outlineFlag.Off;
        this.copybookdirs = [];
        this.copybookexts = [];
        this.program_extensions = [];
        this.invalid_copybookdirs = [];
        this.tabstops = [];
        this.linter = false;
        this.line_comment = false;
        this.enable_data_provider = true;
        this.disable_unc_copybooks_directories = false;
        this.intellisense_include_unchanged = true;
        this.intellisense_include_camelcase = false;
        this.intellisense_include_uppercase = false;
        this.intellisense_include_lowercase = false;
        this.intellisense_item_limit = 0;
        this.process_metadata_cache_on_start = false;
        this.cache_metadata = CacheDirectoryStrategy.Off;
        this.cache_metadata_time_limit = 60000;
        this.cache_metadata_max_directory_scan_depth = 32;
        this.cache_metadata_show_progress_messages = false;
        this.parse_copybooks_for_references = false;
        this.workspacefolders_order = [];
        this.linter_mark_as_information = false;
        this.linter_unused_paragraphs_or_sections = true;
        this.linter_house_standards = true;
        this.linter_house_standards_rules = [];
        this.ignore_unsafe_extensions = false;
        this.process_scanner_hints_embedded_in_comments = false;
        this.process_scanner_hint_token_for_source_dependancies = "source-dependency";
        this.coboldoc_workspace_folder = "coboldoc";
        this.process_metadata_cache_on_file_save = false;
        this.storagearea_id = "";
        this.editor_maxTokenizationLineLength = 20000;
    }
}