var vscode = require('vscode');

var cobolProgram = require('./cobolprogram');
var tabstopper = require('./tabstopper');

function activate(context)
{
    var move2pdCommand = vscode.commands.registerCommand('cobolplugin.move2pd', function () {
        cobolProgram.move2pd();
    });

    var move2ddCommand = vscode.commands.registerCommand('cobolplugin.move2dd', function () {
        cobolProgram.move2dd();
    })

    var move2wsCommand = vscode.commands.registerCommand('cobolplugin.move2ws', function () {
        cobolProgram.move2ws();
    })
    
    var move2anyforwardCommand = vscode.commands.registerCommand('cobolplugin.move2anyforward', function () {
        cobolProgram.move2anyforward();
    })    
    
    var move2anybackwardsCommand = vscode.commands.registerCommand('cobolplugin.move2anybackwards', function () {
        cobolProgram.move2anybackwards();
    })

    var tabCommand = vscode.commands.registerCommand('cobolplugin.tab', function () {
        tabstopper.processTabKey(true);
    });
    var unTabCommand = vscode.commands.registerCommand('cobolplugin.revtabab', function () {
        tabstopper.processTabKey(false);
    });
    context.subscriptions.push(move2pdCommand);
    context.subscriptions.push(move2ddCommand);
    context.subscriptions.push(move2wsCommand);
    context.subscriptions.push(move2anyforwardCommand);
    context.subscriptions.push(move2anybackwardsCommand);
    context.subscriptions.push(tabCommand);
    context.subscriptions.push(unTabCommand);
}
exports.activate = activate;

function deactivate()
{
}
exports.deactivate = deactivate;