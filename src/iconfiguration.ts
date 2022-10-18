import { ESourceFormat } from "./externalfeatures";

export enum outlineFlag {
    On = "on",
    Off = "off",
    Partial = "partial",
    Skeleton = "skeleton"
}

export enum intellisenseStyle {
    Unchanged = "unchanged",
    CamelCase = "camelcase",
    LowerCase = "lowercase",
    UpperCase = "uppercase"
}

export enum hoverApi {
    Off = "off",
    Short = "short",
    Long = "long"
}

export interface IEditorMarginFiles {
    pattern: string;
    sourceformat: ESourceFormat;
}

export enum fileformatStrategy {
    AlwaysFixed = "always_fixed",
    AlwaysVariable = "always_variable",
    Normal = "normal"
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
    fileformat_strategy: fileformatStrategy;
    enable_data_provider: boolean;
    disable_unc_copybooks_directories: boolean;
    intellisense_item_limit: number;
    process_metadata_cache_on_start: boolean;
    // cache_metadata: CacheDirectoryStrategy;
    cache_metadata_inactivity_timeout: number;
    parse_copybooks_for_references: boolean;
    workspacefolders_order: string[];
    linter_mark_as_information: boolean;
    linter_unused_sections: boolean;
    linter_unused_paragraphs: boolean;
    linter_house_standards: boolean;
    linter_house_standards_rules: string[];
    linter_ignore_section_before_entry: boolean;
    linter_ignore_missing_copybook: boolean;

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
    sourceview_include_test_files: boolean;

    format_constants_to_uppercase: boolean;
    format_on_return: boolean;
    intellisense_style: intellisenseStyle;
    editor_maxTokenizationLineLength: number;

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

    suggest_variables_when_context_is_unknown: boolean;

    hover_show_known_api: hoverApi;

    enable_comment_tags: boolean;

    comment_tag_word: boolean;

    snippets: boolean;

    enable_columns_tags: boolean;

    hover_show_encoded_literals: boolean;

    check_file_format_before_file_scan: boolean;

    intellisense_no_space_keywords:string[];

    custom_intellisense_rules: string[];

    margin: boolean;

    enable_codelens_variable_references:boolean;

    enable_codelens_section_paragraph_references: boolean;

    enable_codelens_copy_replacing: boolean;
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
    fileformat_strategy: fileformatStrategy;
    enable_data_provider: boolean;
    disable_unc_copybooks_directories: boolean;
    intellisense_item_limit: number;
    process_metadata_cache_on_start: boolean;
    // cache_metadata: CacheDirectoryStrategy;
    cache_metadata_inactivity_timeout: number;
    parse_copybooks_for_references: boolean;
    workspacefolders_order: string[];
    linter_mark_as_information: boolean;
    linter_unused_paragraphs: boolean;
    linter_unused_sections: boolean;
    linter_house_standards: boolean;
    linter_house_standards_rules: string[];
    linter_ignore_section_before_entry: boolean;
    linter_ignore_missing_copybook: boolean;

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
    sourceview_include_test_files: boolean;

    format_constants_to_uppercase: boolean;
    format_on_return: boolean;
    intellisense_style: intellisenseStyle;

    editor_maxTokenizationLineLength: number;
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

    suggest_variables_when_context_is_unknown: boolean;

    hover_show_known_api: hoverApi;

    enable_comment_tags: boolean;

    comment_tag_word: boolean;

    snippets: boolean;

    enable_columns_tags: boolean;

    hover_show_encoded_literals: boolean;

    check_file_format_before_file_scan: boolean;

    intellisense_no_space_keywords: string[];

    custom_intellisense_rules: string[];

    margin: boolean;

    enable_codelens_variable_references: boolean;

    enable_codelens_section_paragraph_references: boolean;

    enable_codelens_copy_replacing: boolean;

    constructor() {
        this.enable_tabstop = true;
        this.pre_scan_line_limit = 25;
        this.copybooks_nested = false;
        this.fileformat_strategy = fileformatStrategy.Normal;
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
        this.intellisense_item_limit = 30;
        this.process_metadata_cache_on_start = false;
        this.cache_metadata_inactivity_timeout = 5000;
        this.cache_metadata_verbose_messages = false;
        this.parse_copybooks_for_references = false;
        this.workspacefolders_order = [];
        this.linter_mark_as_information = false;
        this.linter_unused_sections = true;
        this.linter_unused_paragraphs = true;
        this.linter_house_standards = true;
        this.linter_house_standards_rules = [];
        this.linter_ignore_missing_copybook = false;
        this.scan_comments_for_hints = false;
        this.scan_comment_copybook_token = "source-dependency";
        this.editor_maxTokenizationLineLength = 20000;
        this.sourceview = false;
        this.sourceview_include_jcl_files = true;
        this.sourceview_include_hlasm_files = true;
        this.sourceview_include_pli_files = true;
        this.sourceview_include_doc_files = true;
        this.sourceview_include_script_files = true;
        this.sourceview_include_object_files = true;
        this.sourceview_include_test_files = true;
        this.linter_ignore_section_before_entry = true;
        this.format_on_return = false;
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
        this.suggest_variables_when_context_is_unknown = true;
        this.hover_show_known_api = hoverApi.Short;
        this.enable_comment_tags = false;
        this.comment_tag_word = false;
        this.snippets = false;
        this.intellisense_style = intellisenseStyle.Unchanged;
        this.enable_columns_tags = false;
        this.hover_show_encoded_literals = true;
        this.check_file_format_before_file_scan = true;
        this.intellisense_no_space_keywords = [];
        this.custom_intellisense_rules = [];
        this.margin = true;
        this.enable_codelens_variable_references = false;
        this.enable_codelens_section_paragraph_references = false;
        this.enable_codelens_copy_replacing = true;
    }
}