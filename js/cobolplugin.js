var vscode = require('vscode');

var cobolProgram = require('./cobolprogram');

function activate(context)
{
    var move2pdCommand = vscode.commands.registerCommand('cobolplugin.move2pd', function () {
        cobolProgram.move2pd();
    });

    context.subscriptions.push(move2pdCommand);

}
exports.activate = activate;

function deactivate()
{
}
exports.deactivate = deactivate;