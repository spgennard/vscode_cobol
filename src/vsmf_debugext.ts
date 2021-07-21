import * as vscode from 'vscode';
import { DebugProtocol } from 'vscode-debugprotocol';
import { Variable, Scope } from 'vscode-debugadapter';
import { VSCOBOLConfiguration } from './vsconfiguration';

export class DebugAdapterInterceptor implements vscode.DebugAdapterTracker {
	
    static SP_REGISTER_SCOPE_ID = 0x49424c54;	

    private pendingVariableResponses = new Map<number, Variable[]>();	

    someCounter=0;

    private lastSPRegisters: Variable[] = [];

    async onDidSendMessage(message: DebugProtocol.ProtocolMessage):Promise<void> {
        const config = VSCOBOLConfiguration.get();
        if (config.extend_micro_focus_cobol_extension_debugger === false) {
            return;
        }

        if (message.type === 'response') {
			const m = message as DebugProtocol.Response;
			if (m.command === 'scopes' && m.body && Array.isArray(m.body.scopes)) {
				m.body.scopes.push(new Scope('Special Registers', DebugAdapterInterceptor.SP_REGISTER_SCOPE_ID));
			}

			if (m.command === 'variables' && this.pendingVariableResponses.has(m.request_seq)) {
				m.body = m.body || {};	// make sure that the response has a body
				m.body.variables = this.pendingVariableResponses.get(m.request_seq);
				m.success = true;	    // override any error that might be returned from the DA
				delete m.message;	    // clear the error message
				this.pendingVariableResponses.delete(m.request_seq);
			}
		}

        if (message.type === 'event') {
			const event = message as DebugProtocol.Event;
			if (event.event === 'stopped') {	// listen on stopped events so that we can have some "state" that changes
				this.lastSPRegisters = await this.getSpecialRegisters();
			}
        }
    }


	async onWillReceiveMessage?(message: DebugProtocol.ProtocolMessage):Promise<void> {
		if (message.type === 'request') {
			const m = message as DebugProtocol.Request;
			if (m.command === 'variables' && m.arguments && typeof m.arguments.variablesReference === 'number') {
				if (m.arguments.variablesReference === DebugAdapterInterceptor.SP_REGISTER_SCOPE_ID) {
                    this.pendingVariableResponses.set(m.seq, this.lastSPRegisters);
				}
			}
		}
    }

    public static registerDebugAdapterInterceptor(): vscode.Disposable {
        const reg = vscode.debug.registerDebugAdapterTrackerFactory('cobol', {
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            createDebugAdapterTracker(session: vscode.DebugSession) {
                return new DebugAdapterInterceptor();
            }
        });

        return reg;
    }


    readonly specialRegisters: string[] = [
        "RETURN-CODE",
        "SORT-RETURN",
        "JSON-CODE",
        "JSON-STATUS",
        "XML-CODE",
        "XML-EVENT",
        "XML-NTEXT",
        "XML-TEXT",
        "SQLCODE"
    ];

    async getSpecialRegisters() : Promise<Variable[]> {
        const vars: Variable[] = [];

        if (vscode.debug.activeDebugSession === undefined) {
            return vars;
        }

        const ds = vscode.debug.activeDebugSession;
        let response = await ds.customRequest('stackTrace', { threadId: 1 })
        const frameId = response.stackFrames[0].id;

        for (const specialRegister of this.specialRegisters) {
            try {
                response = await ds.customRequest('evaluate', { expression: specialRegister, frameId: frameId });
                if (response !== undefined && response.result !== undefined) {
                    let failedExpression = false;
                    if (response.presentationHint !== undefined && response.presentationHint.attributes !== undefined) {
                        for (let alc = 0; alc < response.presentationHint.attributes.length; alc++) {
                            const attr = response.presentationHint.attributes[alc];
                            if (attr === 'failedEvaluation') {
                                failedExpression = true;
                            }
                        }
                    }

                    if (failedExpression) {
                        continue; // skip
                    }

                    const res = response.result;
                    vars.push(new Variable(specialRegister, `${res}`));
                }
            }
            catch {
                //
            }
        }
    
        return vars;
    }
}
