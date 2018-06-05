'use strict';
var vscode = require('vscode');
var fs = require('fs');

var Range = vscode.Range;
var Window = vscode.window;
var Workspace = vscode.workspace;

var DEFAULT_COPYBOOK_EXTS = ["cpy"];
var DEFAULT_COPYBOOK_DIR = ["."];

var previousFile;
var previousLineNumber;

function getExtensions()
{
    var editorConfig =  Workspace.getConfiguration('coboleditor');   
    var rulers = editorConfig.get('copybookexts');
    if (rulers == null || (rulers != null && rulers.length == 0))
    {
        rulers = DEFAULT_COPYBOOK_EXTS;
    }
    return rulers;
}

function getcopybookdirs()
{
    var editorConfig =  Workspace.getConfiguration('coboleditor');   
    var rulers = editorConfig.get('copybookdirs');
    if (rulers == null || (rulers != null && rulers.length == 0))
    {
        rulers = DEFAULT_COPYBOOK_DIR;
    }
    return rulers;
}


function extractText( str )
{
    var ret = "";
  
    if ( /"/.test( str ) )
    {
      return str.match( /"(.*?)"/ )[1];
    } 

    if ( /'/.test( str ) )
    {
      return str.match( /'(.*?)'/ )[1];
    } 
    
     return str;
}

function openFile(editor, filename, line = 0) 
{
    if(filename == "")
        return;

    var fileExtension = filename.split('.').pop();
    var fullPath = vscode.workspace.rootPath + '/' + filename ;
    
    var extsdir = getcopybookdirs();
    var extsdirpos = 0;
    for(extsdirpos=0; extsdirpos < extsdir.length; extsdirpos++)
    {
        var extdir = extsdir[extsdirpos];

        var basefullPath = vscode.workspace.rootPath + "/" + extdir + '/' + filename ;

        //No extension?
        if (filename == fileExtension)
        {
            // search through the possible extensions
            var exts = getExtensions();
            var extpos = 0;
            for(extpos=0; extpos < exts.length; extpos++)
            {
                var ext = exts[extpos];
                var possibleFile = basefullPath + "." + ext;

                if (!fs.existsSync(possibleFile) === false)
                {
                    fullPath = possibleFile;
                    break;
                }
            }
        }
        else
        {
            if (!fs.existsSync(basefullPath) === false)
            {
                fullPath = basefullPath;
            }  
        }
    }

    if (fs.existsSync(fullPath) === false)
    {
        return;
    }
    
    previousFile = editor.document.fileName;
    previousLineNumber = editor.selection.active.line;

    (vscode.workspace.openTextDocument(fullPath).then(
        function (txtDocument) {
           return Window.showTextDocument(txtDocument).then(function()
           {
                  goToLine(line);
           })
        }, function() 
        {
            Window.showWarningMessage("Cannot open : "+fullPath);
        }));
}

function openSavedFile(editor, filename, line = 0)
{
    if (previousFile === null)
    {
        return;
    }

    (vscode.workspace.openTextDocument(previousFile).then(
        function (txtDocument) {
           return Window.showTextDocument(txtDocument).then(function()
           {
                  goToLine(line);
           })
        }, function() 
        {
            Window.showWarningMessage("Cannot open previous file : "+fullPath);
        }));   
}

function openPreviousFile() 
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var sel = editor.selections;

    openSavedFile(editor, previousFile, previousLineNumber);
}
exports.openPreviousFile = openPreviousFile;

function goToLine(line)
{
    let reviewType = vscode.TextEditorRevealType.InCenter;
    if (line === vscode.window.activeTextEditor.selection.active.line) 
    {
        reviewType = vscode.TextEditorRevealType.InCenterIfOutsideViewport;
    }
    let newSelection = new vscode.Selection(line, 0, line, 0);
    vscode.window.activeTextEditor.selection = newSelection;
    vscode.window.activeTextEditor.revealRange(newSelection, reviewType);
}


function parseLine(doc, sel)
{
    var range = new Range(sel[0].start.line, 0, sel[0].end.line, 999);
    var txt = doc.getText(range);

    var match = extractText(txt);

    if(match != txt)
        return match;
    else
        return "";
}


function openCopyBookFile() 
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var sel = editor.selections;

    if(sel.length > 1 || sel[0].start.line != sel[0].end.line)
        return;

    var filename = parseLine(doc, sel);

    openFile(editor, filename);
}

exports.openCopyBookFile = openCopyBookFile;
