import { ESourceFormat } from "./externalfeatures";

export enum outlineFlag {
    On = "on",
    Off = "off",
    Partial = "partial",
    Skeleton = "skeleton"
}

export enum formatOnReturn {
    Off = "off",
    CamelCase = "camelcase",
    UpperCase = "uppercase"
}


export interface IEditorMarginFiles {
    pattern: string;
    sourceformat: ESourceFormat;
}

export interface ICOBOLSettings {
    enable_tabstop: boolean;
    pre_scan_line_limit: number;
    copybooks_nested: boolean;
    outline: outlineFlag;
    copybookdirs: string[];
    invalid_copybookdirs: string[];
    copybookexts: string[];
    program_extensions: string[];
    tabstops: number[];
    linter: boolean;
    line_comment: boolean;
    fileformat_strategy: string;
    enable_data_provider: boolean;
    disable_unc_copybooks_directories: boolean;
    intellisense_include_unchanged: boolean;
    intellisense_include_camelcase: boolean;
    intellisense_include_uppercase: boolean;
    intellisense_include_lowercase: boolean;
    intellisense_item_limit: number;
    process_metadata_cache_on_start: boolean;
    // cache_metadata: CacheDirectoryStrategy;
    cache_metadata_inactivity_timeout: number;
    parse_copybooks_for_references: boolean;
    workspacefolders_order: string[];
    linter_mark_as_information: boolean;
    linter_unused_paragraphs_or_sections: boolean;
    linter_house_standards: boolean;
    linter_house_standards_rules: string[];
    linter_ignore_section_before_entry: boolean;
    linter_ignore_missing_copybook: boolean;

    ignore_unsafe_extensions: boolean;
    coboldoc_workspace_folder: string;
    scan_comments_for_hints: boolean;
    cache_metadata_verbose_messages: boolean;
    scan_comment_copybook_token: string;
    sourceview: boolean;
    sourceview_include_jcl_files: boolean;
    sourceview_include_hlasm_files: boolean;
    sourceview_include_pli_files: boolean;
    sourceview_include_doc_files: boolean;
    sourceview_include_script_files: boolean;
    sourceview_include_object_files: boolean;

    format_constants_to_uppercase: boolean;
    format_on_return: formatOnReturn;
    editor_maxTokenizationLineLength: number;
    init_required: boolean;

    metadata_symbols: string[];
    metadata_entrypoints: string[];
    metadata_types: string[];
    metadata_files: string[];
    metadata_knowncopybooks: string[];

    maintain_metadata_cache: boolean;
    maintain_metadata_recursive_search: boolean;

    enable_semantic_token_provider: boolean;

    enable_text_replacement: boolean;

    editor_margin_files: IEditorMarginFiles[];

    enable_source_scanner: boolean;

    valid_cobol_language_ids: string[];

    files_exclude: string[];

    scan_line_limit: number;

    scan_time_limit: number;

    in_memory_cache_size: number;
}

export class COBOLSettings implements ICOBOLSettings {
    enable_tabstop: boolean;
    pre_scan_line_limit: number;
    copybooks_nested: boolean;
    outline: outlineFlag;
    copybookdirs: string[];
    invalid_copybookdirs: string[];
    copybookexts: string[];
    program_extensions: string[];
    tabstops: number[];
    linter: boolean;
    line_comment: boolean;
    fileformat_strategy: string;
    enable_data_provider: boolean;
    disable_unc_copybooks_directories: boolean;
    intellisense_include_unchanged: boolean;
    intellisense_include_camelcase: boolean;
    intellisense_include_uppercase: boolean;
    intellisense_include_lowercase: boolean;
    intellisense_item_limit: number;
    process_metadata_cache_on_start: boolean;
    // cache_metadata: CacheDirectoryStrategy;
    cache_metadata_inactivity_timeout: number;
    parse_copybooks_for_references: boolean;
    workspacefolders_order: string[];
    linter_mark_as_information: boolean;
    linter_unused_paragraphs_or_sections: boolean;
    linter_house_standards: boolean;
    linter_house_standards_rules: string[];
    linter_ignore_section_before_entry: boolean;
    linter_ignore_missing_copybook: boolean;

    ignore_unsafe_extensions: boolean;
    coboldoc_workspace_folder: string;
    scan_comments_for_hints: boolean;
    cache_metadata_verbose_messages: boolean;
    scan_comment_copybook_token: string;
    sourceview: boolean;
    sourceview_include_jcl_files: boolean;
    sourceview_include_hlasm_files: boolean;
    sourceview_include_pli_files: boolean;
    sourceview_include_doc_files: boolean;
    sourceview_include_script_files: boolean;
    sourceview_include_object_files: boolean;

    format_constants_to_uppercase: boolean;
    format_on_return: formatOnReturn;
    editor_maxTokenizationLineLength: number;
    init_required: boolean;

    metadata_symbols: string[];
    metadata_entrypoints: string[];
    metadata_types: string[];
    metadata_files: string[];
    metadata_knowncopybooks: string[];

    maintain_metadata_cache: boolean;
    maintain_metadata_recursive_search: boolean;

    enable_semantic_token_provider: boolean;

    enable_text_replacement: boolean;

    editor_margin_files: IEditorMarginFiles[];

    enable_source_scanner: boolean;

    valid_cobol_language_ids: string[];

    files_exclude: string[];

    scan_line_limit: number;

    scan_time_limit: number;

    in_memory_cache_size: number;

    constructor() {
        this.init_required = true;
        this.enable_tabstop = true;
        this.pre_scan_line_limit = 25;
        this.copybooks_nested = false;
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
        this.intellisense_item_limit = 30;
        this.process_metadata_cache_on_start = false;
        this.cache_metadata_inactivity_timeout = 5000;
        this.cache_metadata_verbose_messages = false;
        this.parse_copybooks_for_references = false;
        this.workspacefolders_order = [];
        this.linter_mark_as_information = false;
        this.linter_unused_paragraphs_or_sections = true;
        this.linter_house_standards = true;
        this.linter_house_standards_rules = [];
        this.linter_ignore_missing_copybook = false;
        this.ignore_unsafe_extensions = false;
        this.scan_comments_for_hints = false;
        this.scan_comment_copybook_token = "source-dependency";
        this.coboldoc_workspace_folder = "coboldoc";
        this.editor_maxTokenizationLineLength = 20000;
        this.sourceview = false;
        this.sourceview_include_jcl_files = true;
        this.sourceview_include_hlasm_files = true;
        this.sourceview_include_pli_files = true;
        this.sourceview_include_doc_files = true;
        this.sourceview_include_script_files = true;
        this.sourceview_include_object_files = true;
        this.linter_ignore_section_before_entry = true;
        this.format_on_return = formatOnReturn.Off;
        this.format_constants_to_uppercase = true;
        this.metadata_symbols = [];
        this.metadata_entrypoints = [];
        this.metadata_types = [];
        this.metadata_files = [];
        this.metadata_knowncopybooks = [];
        this.maintain_metadata_cache = true;
        this.maintain_metadata_recursive_search = false;
        this.enable_semantic_token_provider = false;
        this.enable_text_replacement = false;
        this.editor_margin_files = [];
        this.enable_source_scanner = true;
        this.valid_cobol_language_ids = [
            "BITLANG-COBOL",
            "COBOL", 
            "COBOLIT",
            "ACUCOBOL",
            "COBOL_MF_LISTFILE"
        ];

        this.files_exclude = [];
        this.scan_line_limit = 15000;
        this.scan_time_limit = 4000;
        this.in_memory_cache_size = 6;
    }
}