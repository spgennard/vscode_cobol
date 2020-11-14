declare module 'trie-search';
declare class TrieSearch {
	constructor(keyFields?: string|string[], options?: any);

	add(obj: any, customKeys?: any): void;

	addAll(arr: any, customKeys?: any): void;

	addFromObject(obj: any, valueField?: any): void;

	cleanCache(): void;

	clearCache(): void;

	expandString(value: any): any;

	findNode(key: any): any;

	get(phrases: any, reducer?: any): any;

	getId(item: any): any;

	keyToArr(key: any): any;

	map(key: any, value: any): void;

	reset(): void;
}
