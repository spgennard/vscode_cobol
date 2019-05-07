import * as Collections from 'typescript-collections';
import { Hover } from 'vscode';

import ile_datatime from './ile_datetime.json';
import { Dictionary } from 'typescript-collections';

class CallTarget
{
	api: string;
	url: string;
	description: string;

	constructor(_api: string, _url: string, _description: string) {
		this.api = _api;
		this.url = _url;
		this.description = _description;
	}
}

interface IDictionary {
	[index: string]: CallTarget;
}

interface IAPIDictionary {
    [index:string]: string;
}
interface CallTargetInterfaces {
	url: string;
	name: string;
	apis: IAPIDictionary;
}

let callTargets: IDictionary = {};

function addApis(a: CallTargetInterfaces)
{
	let values = Object.keys(a.apis);
	for (let c = 0; c < values.length; c++) {
		let value = values[c];
		callTargets[value] = new CallTarget(value, a.url, a.apis[value]);
	}
}

addApis(ile_datatime);





/* inline decl */
export function getCallTarget(api: string) {
	return callTargets[api];
}
