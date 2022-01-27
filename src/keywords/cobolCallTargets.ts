import ile_datatime from "./ile_datetime";
import cbl_apis from "./mf_cbl_apis";

export class CallTarget {
	public api: string;
	public url: string;
	public name: string;

	public description: string;

	constructor(_name: string, _url: string, _api: string, _description: string) {
		this.api = _api;
		this.name = _name;
		this.url = _url;
		this.description = _description;
	}
}

interface IDictionary {
	[index: string]: CallTarget;
}

interface IAPIDictionary {
	[index: string]: string;
}

interface CallTargetInterfaces {
	url: string;
	name: string;
	apis: IAPIDictionary;
}

const callTargets: IDictionary = {};

function addApis(a: CallTargetInterfaces) {
	const values = Object.keys(a.apis);
	for (let c = 0; c < values.length; c++) {
		const value = values[c];
		callTargets[value] = new CallTarget(a.name, a.url, value, a.apis[value]);
	}
}

addApis(ile_datatime);
addApis(cbl_apis);


// /* inline decl */
export function getCallTarget(api: string): CallTarget | undefined {

	if (typeof callTargets[api] === "undefined") {
		return undefined;
	}

	return callTargets[api];
}
