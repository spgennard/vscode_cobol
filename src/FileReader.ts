import fs = require("fs");
import path = require("path");

import { getCurrentContext } from './extension';

export class FileReader {
     
    public static ReadJSONFromExtensionDir(file: string, encoding?: string):any {
        try {
            let ext = getCurrentContext().extensionPath;
            
            let content = fs.readFileSync(path.join(ext, file), encoding);
            if (content !== null || content !== undefined) {
                return JSON.parse(content.toString());
            }
        }
        catch (error)
        {
            return "";
        }
    }
}