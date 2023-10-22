import { CancellationToken, FormattingOptions, OnTypeFormattingEditProvider, Position, ProviderResult, TextDocument, TextEdit, languages } from "vscode";
import { ICOBOLSettings } from "./iconfiguration";
import { VSExtensionUtils } from "./vsextutis";
import { COBOLCaseFormatter } from "./caseformatter";
import { COBOLDocumentationCommentHandler } from "./doccomment";

export class COBOLTypeFormatter implements OnTypeFormattingEditProvider {
    caseFormatter: COBOLCaseFormatter;
    docFormatter: COBOLDocumentationCommentHandler;

    constructor(settings: ICOBOLSettings) {
        this.caseFormatter = new COBOLCaseFormatter(settings);
        this.docFormatter = new COBOLDocumentationCommentHandler(settings);
    }

    provideOnTypeFormattingEdits(document: TextDocument, position: Position, ch: string, options: FormattingOptions, token: CancellationToken): ProviderResult<TextEdit[]> {

        const ed: TextEdit[] = [];

        try {
            const tmp_ed = this.caseFormatter.provideOnTypeFormattingEdits(document, position, ch, options, token);
            if (tmp_ed !== undefined && tmp_ed.length > 0) {
                ed.push(...tmp_ed);
            }
        }
        catch
        {
            // ignored
        }

        try {
            const tmp_ed = this.docFormatter.provideOnTypeFormattingEdits(document, position, ch, options, token);
            if (tmp_ed !== undefined && tmp_ed.length > 0) {
                ed.push(...tmp_ed);
            }
        }
        catch
        {
            // ignored
        }

        return ed;
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    static register(settings: ICOBOLSettings): any {
        const langPlusSchemas = VSExtensionUtils.getAllCobolSelectors(settings, false);

        return languages.registerOnTypeFormattingEditProvider(langPlusSchemas, new COBOLTypeFormatter(settings), "\n");
    }
}
