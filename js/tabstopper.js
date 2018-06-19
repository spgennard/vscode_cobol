'use strict';
var vscode = require('vscode');
var Range = vscode.Range;
var Window = vscode.window;
var Position = vscode.Position;
var Workspace = vscode.workspace;

var DEFAULT_RULER = [0, 7,  11,  15,  19,  23,  27,  31, 35, 39,  43,  47,   51,  55, 59,  63,  67,  71,  75,  79];

function getTabs()
{
    var editorConfig =  Workspace.getConfiguration('coboleditor');   
    var rulers = editorConfig.get('tabstops');
    if (rulers == null || (rulers != null && rulers.length == 0))
    {
        rulers = DEFAULT_RULER;
    }
    return rulers;
}

function executeTab(editor, doc, sel, inserting)
{
    editor.edit(function (edit) 
    {
        for (var x = 0; x < sel.length; x++)
         {
            if(sel[x].start.line == sel[x].end.line)
            {
                var position = sel[x].start;

                if(inserting)
                    singleSelectionTab(edit, doc, position);
                else
                    singleSelectionUnTab(edit, doc, position);
            }
            else
            {
                if(inserting)
                    multipleSelectionTab(edit, doc, sel[x]);
                else
                    multipleSelectionUnTab(edit, doc, sel[x]);
            }
        }
    });
}

function singleSelectionTab(edit, d, pos)
{
    var size = tabSize(edit, pos.character);

    edit.insert(pos, ' '.repeat(size));
}

function singleSelectionUnTab(edit, d, pos) 
{
    var size = unTabSize(edit, pos.character);

    var range = new Range(pos.line, pos.character - size, pos.line, pos.character);
    var txt = d.getText(range);

    if(txt == ' '.repeat(size))
    {
        edit.delete(range);
    }
}

function multipleSelectionTab(edit, d, sel)
{
    for (var line = sel.start.line; line <= sel.end.line; line++) 
    {
        var pos = new Position(line, sel.start.character);
        singleSelectionTab(edit, d, pos);
    }
}

function multipleSelectionUnTab(edit, d, sel) 
{
    for (var line = sel.start.line; line <= sel.end.line; line++) 
    {
        var pos = new Position(line, sel.start.character);
        singleSelectionUnTab(edit, d, pos);
    }
}

function tabSize(editor, pos) 
{
    var tabs = getTabs(editor);
    var tab = 0;
    for (var index = 0; index < tabs.length; index++)
    {
        tab = tabs[index];

        if(tab > pos)
        {
            return tab - pos;
        }
    }
    // outside range?
    return 3 - ((pos - tabs[tabs.length - 1]) % 3);
}


function unTabSize(editor, pos) 
{
    var tabs = getTabs(editor);

    // outside range?
    if (pos > tabs[tabs.length - 1])
    {
        if ((pos - tabs[tabs.length - 1]) % 3 == 0)
            return 3;
        else
            return (pos - tabs[tabs.length - 1]) % 3;
    }

    for (var index = tabs.length - 1; index > -1; index--) 
    {
        var tab = tabs[index];

        if(tab < pos)
        {
            return pos - tab;
        }
    }
    return 0;
}

function processTabKey(inserting)
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var sel = editor.selections;
    executeTab(editor, doc, sel, inserting);
}
exports.processTabKey = processTabKey;