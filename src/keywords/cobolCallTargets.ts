import { ILE_APIs } from "./ile_datetime";
import { CBL_APIs } from "./mf_cbl_apis";
import { MFUNIT_APIs } from "./mf_mfunit";

export interface IKnownApis
{
    url: string;
    name: string;
    apis: Map<string, string>;
}

export class CallTarget {
	public api: string;
	public url: string;
	public apiGroup: string;
	public description: string;

	constructor(_name: string, _url: string, _api: string, _description: string) {
		this.api = _api;
		this.apiGroup = _name;
		this.url = _url;
		this.description = _description;
	}
}

const callTargets = new Map<string, CallTarget>();

function addApis(a: IKnownApis) {
	for(const [key, description] of a.apis) {
		callTargets.set(key, new CallTarget(a.name, a.url, key, description));
	}
}

addApis(new CBL_APIs());
addApis(new MFUNIT_APIs());
addApis(new ILE_APIs());

export class KnownAPIs {
	// /* inline decl */
	public static getCallTarget(api: string): CallTarget | undefined {
		return callTargets.get(api);
	}
}
