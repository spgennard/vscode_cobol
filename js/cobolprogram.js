'use strict';
var vscode = require('vscode');
var Position = vscode.Position;
var Range = vscode.Range;
var Window = vscode.window;

function move2pd()
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var line = findProcedureLine();

     if (line > 0)
     {
         goToLine(line)
     }
     else
     {
        vscode.window.setStatusBarMessage('ERROR: \'PROCEDURE DIVISION\' not found.', 4000);
     }

}

exports.move2pd = move2pd;

function findMatch(mat)
{
    var doc = Window.activeTextEditor.document;
    var line;
    for (line = 0; line <= doc.lineCount; line++)
    {
       var range = new Range(line, 1, line, 132);
       var txt = doc.getText(range);

       if (txt.match(mat))
       {
           break;
       }
    }
    return line;
 }

function findProcedureLine()
{
    return findMatch(/procedure\s*division/i);
}

 function goToLine(line)
 {
    let reviewType = vscode.TextEditorRevealType.InCenter;
    if (line === vscode.window.activeTextEditor.selection.active.line)
    {
        reviewType = vscode.TextEditorRevealType.InCenterIfOutsideViewport;
    }
    let newSe = new vscode.Selection(line, 0, line, 0);
    vscode.window.activeTextEditor.selection = newSe;
    vscode.window.activeTextEditor.revealRange(newSe, reviewType);
  }