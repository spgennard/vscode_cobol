"use strict";

import { 
    DecorationOptions, 
    Range, 
    TextEditor, 
    Position, 
    window, 
    ThemeColor, 
    TextDocument, 
    TextEditorDecorationType,
    OverviewRulerLane,
    workspace 
} from "vscode";
import { VSCOBOLConfiguration } from "./vsconfiguration";
import { VSCOBOLSourceScanner } from "./vscobolscanner";
import { VSExtensionUtils, TextLanguage } from "./vsextutis";
import { ICOBOLSourceScanner } from "./icobolsourcescanner";
import { COBOLTokenStyle, COBOLToken } from "./cobolsourcescanner";
import { VSExternalFeatures } from "./vsexternalfeatures";
import { ExtensionDefaults } from "./extensionDefaults";

// Interface for color configuration
interface MinimapColors {
    division: { light: string; dark: string };
    section: { light: string; dark: string };
    paragraph: { light: string; dark: string };
}

// Default colors
const defaultColors: MinimapColors = {
    division: { light: "#FF6F00", dark: "#FFB74D" },
    section: { light: "#0066CC", dark: "#4FC3F7" },
    paragraph: { light: "#8E24AA", dark: "#CE93D8" }
};

// Function to get user-configured colors
function getMinimapColors(): MinimapColors {
    const config = workspace.getConfiguration(ExtensionDefaults.defaultEditorConfig);
    const userColors = config.get<MinimapColors>("minimap_section_colors");
    
    if (!userColors) {
        return defaultColors;
    }
    
    return {
        division: userColors.division || defaultColors.division,
        section: userColors.section || defaultColors.section,
        paragraph: userColors.paragraph || defaultColors.paragraph
    };
}

// Function to create decoration types with configurable colors
function createDecorationTypes() {
    const colors = getMinimapColors();
    
    const sectionDecorationType = window.createTextEditorDecorationType({
        overviewRulerColor: new ThemeColor("symbolIcon.classForeground"),
        overviewRulerLane: OverviewRulerLane.Right,
        rangeBehavior: 1, // ClosedClosed
        isWholeLine: true,
        light: {
            overviewRulerColor: colors.section.light,
            backgroundColor: hexToRgba(colors.section.light, 0.05)
        },
        dark: {
            overviewRulerColor: colors.section.dark,
            backgroundColor: hexToRgba(colors.section.dark, 0.05)
        }
    });
    
    const paragraphDecorationType = window.createTextEditorDecorationType({
        overviewRulerColor: new ThemeColor("symbolIcon.methodForeground"),
        overviewRulerLane: OverviewRulerLane.Right,
        rangeBehavior: 1, // ClosedClosed
        isWholeLine: true,
        light: {
            overviewRulerColor: colors.paragraph.light,
            backgroundColor: hexToRgba(colors.paragraph.light, 0.03)
        },
        dark: {
            overviewRulerColor: colors.paragraph.dark,
            backgroundColor: hexToRgba(colors.paragraph.dark, 0.03)
        }
    });
    
    const divisionDecorationType = window.createTextEditorDecorationType({
        overviewRulerColor: new ThemeColor("symbolIcon.moduleForeground"),
        overviewRulerLane: OverviewRulerLane.Left,
        rangeBehavior: 1, // ClosedClosed
        isWholeLine: true,
        light: {
            overviewRulerColor: colors.division.light,
            backgroundColor: hexToRgba(colors.division.light, 0.08)
        },
        dark: {
            overviewRulerColor: colors.division.dark,
            backgroundColor: hexToRgba(colors.division.dark, 0.08)
        }
    });
    
    return { sectionDecorationType, paragraphDecorationType, divisionDecorationType };
}

// Utility function to convert hex color to rgba
function hexToRgba(hex: string, alpha: number): string {
    // Remove # if present
    hex = hex.replace('#', '');
    
    // Parse hex values
    const r = parseInt(hex.substr(0, 2), 16);
    const g = parseInt(hex.substr(2, 2), 16);
    const b = parseInt(hex.substr(4, 2), 16);
    
    return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

/**
 * Handler for managing minimap decorations that show COBOL structure boundaries
 */
export class VSMinimapHandler {
    private decorationTypes: {
        sectionDecorationType: TextEditorDecorationType;
        paragraphDecorationType: TextEditorDecorationType;
        divisionDecorationType: TextEditorDecorationType;
    } | null = null;
    
    /**
     * Updates the minimap decorations for the given text editor
     */
    public async updateDecorations(activeTextEditor: TextEditor | undefined): Promise<void> {
        if (!activeTextEditor) {
            return;
        }

        const doc: TextDocument = activeTextEditor.document;
        const configHandler = VSCOBOLConfiguration.get_resource_settings(doc, VSExternalFeatures);
        const textLanguage: TextLanguage = VSExtensionUtils.isSupportedLanguage(doc);

        // Only process COBOL files
        if (textLanguage === TextLanguage.Unknown || textLanguage === TextLanguage.JCL) {
            this.clearDecorations(activeTextEditor);
            return;
        }

        // Check if minimap decorations are enabled
        if (!configHandler.enable_minimap_section_boundaries) {
            this.clearDecorations(activeTextEditor);
            return;
        }

        const sectionDecorations: DecorationOptions[] = [];
        const paragraphDecorations: DecorationOptions[] = [];
        const divisionDecorations: DecorationOptions[] = [];

        // Get the COBOL source scanner for the document
        const sourceScanner: ICOBOLSourceScanner | undefined = VSCOBOLSourceScanner.getCachedObject(doc, configHandler);

        if (!sourceScanner) {
            this.clearDecorations(activeTextEditor);
            return;
        }

        try {
            // Create or recreate decoration types (in case colors changed)
            this.ensureDecorationTypes();
            
            // Process sections
            this.processSections(sourceScanner, sectionDecorations);
            
            // Process paragraphs  
            this.processParagraphs(sourceScanner, paragraphDecorations);
            
            // Process divisions
            this.processDivisions(sourceScanner, divisionDecorations);

            // Apply the decorations
            if (this.decorationTypes) {
                activeTextEditor.setDecorations(this.decorationTypes.sectionDecorationType, sectionDecorations);
                activeTextEditor.setDecorations(this.decorationTypes.paragraphDecorationType, paragraphDecorations);
                activeTextEditor.setDecorations(this.decorationTypes.divisionDecorationType, divisionDecorations);
            }
        } catch (error) {
            // If anything goes wrong, clear decorations
            this.clearDecorations(activeTextEditor);
        }
    }

    /**
     * Process sections and add decorations for them
     */
    private processSections(sourceScanner: ICOBOLSourceScanner, decorations: DecorationOptions[]): void {
        // Iterate through all sections
        for (const [_, section] of sourceScanner.sections) {
            if (this.shouldShowToken(section)) {
                const decoration = this.createDecorationFromToken(section);
                if (decoration) {
                    decorations.push(decoration);
                }
            }
        }
    }

    /**
     * Process paragraphs and add decorations for them
     */
    private processParagraphs(sourceScanner: ICOBOLSourceScanner, decorations: DecorationOptions[]): void {
        // Iterate through all paragraphs
        for (const [_, paragraph] of sourceScanner.paragraphs) {
            if (this.shouldShowToken(paragraph)) {
                const decoration = this.createDecorationFromToken(paragraph);
                if (decoration) {
                    decorations.push(decoration);
                }
            }
        }
    }

    /**
     * Process divisions and add decorations for them  
     */
    private processDivisions(sourceScanner: ICOBOLSourceScanner, decorations: DecorationOptions[]): void {
        // Get all tokens in order and filter for divisions
        for (const token of sourceScanner.tokensInOrder) {
            if (token.tokenType === COBOLTokenStyle.Division && this.shouldShowToken(token)) {
                const decoration = this.createDecorationFromToken(token);
                if (decoration) {
                    decorations.push(decoration);
                }
            }
        }
    }

    /**
     * Create a decoration from a COBOL token
     */
    private createDecorationFromToken(token: COBOLToken): DecorationOptions | null {
        try {
            const startPos = new Position(token.rangeStartLine, 0);
            const endPos = new Position(token.rangeStartLine, 0);
            const range = new Range(startPos, endPos);
            
            return {
                range: range,
                hoverMessage: `${token.tokenType}: ${token.tokenName}`
            };
        } catch (error) {
            return null;
        }
    }

    /**
     * Determine if a token should be shown in the minimap
     */
    private shouldShowToken(token: COBOLToken): boolean {
        // Don't show tokens that are marked to ignore in outline view
        if (token.ignoreInOutlineView) {
            return false;
        }

        // Don't show implicit tokens
        if (token.isImplicitToken) {
            return false;
        }

        // For paragraphs and sections, only show those in procedure division
        if (token.tokenType === COBOLTokenStyle.Paragraph || token.tokenType === COBOLTokenStyle.Section) {
            return token.inProcedureDivision;
        }

        // Always show divisions (they help show high-level structure)
        if (token.tokenType === COBOLTokenStyle.Division) {
            return true;
        }

        return false;
    }

    /**
     * Ensure decoration types are created/updated
     */
    private ensureDecorationTypes(): void {
        // Dispose existing decoration types
        if (this.decorationTypes) {
            this.decorationTypes.sectionDecorationType.dispose();
            this.decorationTypes.paragraphDecorationType.dispose();
            this.decorationTypes.divisionDecorationType.dispose();
        }
        
        // Create new decoration types with current colors
        this.decorationTypes = createDecorationTypes();
    }
    
    /**
     * Clear all decorations from the text editor
     */
    private clearDecorations(activeTextEditor: TextEditor): void {
        if (this.decorationTypes) {
            activeTextEditor.setDecorations(this.decorationTypes.sectionDecorationType, []);
            activeTextEditor.setDecorations(this.decorationTypes.paragraphDecorationType, []);
            activeTextEditor.setDecorations(this.decorationTypes.divisionDecorationType, []);
        }
    }

    /**
     * Dispose of all decoration types
     */
    public dispose(): void {
        if (this.decorationTypes) {
            this.decorationTypes.sectionDecorationType.dispose();
            this.decorationTypes.paragraphDecorationType.dispose();
            this.decorationTypes.divisionDecorationType.dispose();
            this.decorationTypes = null;
        }
    }
}

export const vsMinimapHandler = new VSMinimapHandler();