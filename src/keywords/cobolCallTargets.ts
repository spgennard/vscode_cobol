// import { VSExternalFeatures } from "../vsexternalfeatures";
import { ILE_APIs } from "./ile_datetime";
import { CBL_APIs } from "./mf_cbl_apis";
import { MFUNIT_APIs } from "./mf_mfunit";
import { ACU_COMMON_APIs } from "./acu_common_apis";

export interface IKnownApis {
	url: string;
	name: string;
	apis: Map<string, string[]>;
	examples: Map<string, string[]>;
	snippets: Map<string, string[]>;
}

export class CallTarget {
	public api: string;
	public url: string;
	public apiGroup: string;
	public description: string[];
	public example: string[];
	public snippet: string[];

	constructor(_name: string, _url: string, _api: string, _description: string[], _example: string[], _snippet: string[]) {
		this.api = _api;
		this.apiGroup = _name;
		this.url = _url;
		this.description = _description;
		// if (this.description.length === 0) {
		// 	VSExternalFeatures.logMessage(`INFO: Missing description for ${_api}`);
		// }
		this.example = _example;
		this.snippet = _snippet;
	}
}

const emptyMap = new Map<string, CallTarget>();
const callTargets_cobol = new Map<string, CallTarget>();

const callTargets_ilecobol = new Map<string, CallTarget>();

function addApis(calltarget:Map<string, CallTarget>, a: IKnownApis) {
	for (const [key, description] of a.apis) {
		const possibleExample = a.examples.get(key);
		const possibleSnippet = a.snippets.get(key);
		calltarget.set(key, new CallTarget(a.name, a.url, key, description,
			(possibleExample === undefined ? [] : possibleExample),
			(possibleSnippet === undefined ? [] : possibleSnippet)
		));
	}

	// for (const [api, ] of a.snippets) {
	// 	if (a.apis.get(api) === undefined) {
	// 		// eslint-disable-next-line no-console
	// 		console.log(`${api} has no description`);
	// 	}
	// }

}

addApis(callTargets_cobol, new CBL_APIs());
addApis(callTargets_cobol, new MFUNIT_APIs());
addApis(callTargets_cobol, new ILE_APIs());

addApis(callTargets_cobol, new ACU_COMMON_APIs());

addApis(callTargets_ilecobol, new ILE_APIs());

export class KnownAPIs {
	// /* inline decl */
	public static getCallTarget(language:string, api: string): CallTarget | undefined {
		switch(language) {
			case "ILECOBOL": return callTargets_ilecobol.get(api);
			case "COBOL": return callTargets_cobol.get(api);
		}
		return undefined;
	}

	public static getCallTargetMap(language:string): Map<string, CallTarget> {
		switch(language) {
			case "ILECOBOL": return callTargets_ilecobol;
			case "COBOL": return callTargets_cobol;
		}		
		return emptyMap;
	}
}
