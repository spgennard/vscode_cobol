'use strict';
var vscode = require('vscode');
var Position = vscode.Position;
var Range = vscode.Range;
var Window = vscode.window;

function move2pd()
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var line = findProcedureDivision();

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

function move2dd()
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var line = findDataDivision();

     if (line > 0)
     {
         goToLine(line)
         return;
     }

     line = findWorkingStorageSection();

     if (line > 0)
     {
        goToLine(line);
        return;
     }

    vscode.window.setStatusBarMessage('ERROR: \'DATA DIVISION\' or \'WORKING-STORAGE SECTION\' not found.', 4000);
}
exports.move2dd = move2dd;

function move2ws()
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var line = findWorkingStorageSection();

     if (line > 0)
     {
        goToLine(line);
        return;
     }

    vscode.window.setStatusBarMessage('ERROR: \'WORKING-STORAGE SECTION\' not found.', 4000);
}
exports.move2ws = move2ws

function move2anyforward()
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var line = findAnyNext(1);

     if (line > 0)
     {
        goToLine(line);
        return;
     }
}
exports.move2anyforward = move2anyforward

function move2anybackwards()
{
    var editor = Window.activeTextEditor;
    var doc = editor.document;
    var line = findAnyNext(-1);

     if (line > 0)
     {
        goToLine(line);
        return;
     }
}
exports.move2anybackwards = move2anybackwards


function findMatchForward(mat)
{
    var doc = Window.activeTextEditor.document;
    var line;

    for (line =  Window.activeTextEditor.line; line <= doc.lineCount; line++)
    {
       var range = new Range(line, 1, line, 132);
       var txt = doc.getText(range);

       for(matpos = 0; matpos < mat.length; matpos++)
       {
            if (txt.match(mat[matpos]))
            {
                return line;
            }
        }
    }

    return 0;
}

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
           return line;
       }
    }

    return 0;
}

function findAnyMatch(mats, counter)
{
    var doc = Window.activeTextEditor.document;
    const editor = vscode.window.activeTextEditor;
    var line = editor.selection.active.line;    

    line += counter;

    var endValue = counter == 1 ? doc.lineCount : 0;
    for (; line != endValue; line += counter)
    {
       var range = new Range(line, 1, line, 132);
       var txt = doc.getText(range);

       var matsLen = mats.length;
       var matpos;
       for(matpos = 0; matpos < matsLen; matpos++)
       {
            var mat = mats[matpos];

            if (txt.match(mat))
            {
                return line;
            }
        }
    }

    return 0;
}

function findAnyNext(counter)
{
    var mats = [/.*\s*division/i,
                /entry\s*"/i,
                /.*\s*section\./i,
                /eject/i,
                /program-id\./i,
                /class-id[\.|\s]/i,
                /method-id[\.|\s]/i
                ];

    return findAnyMatch(mats, counter);
}

function findProcedureDivision()
{
    return findMatch(/procedure\s*division/i);
}

function findDataDivision()
{
    return findMatch(/data\s*division/i);
}

function findWorkingStorageSection()
{
    return findMatch(/working-storage\s*section/i);
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