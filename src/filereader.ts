import fs = require("fs");
import path = require("path");

import { getCurrentContext } from './extension';

export class FileReader {

    public static ReadJSONFromExtensionDir(file: string, encoding?: string):any {
        try {
            let ext = getCurrentContext().extensionPath;
            let fileEncoding: string = "";

            if (encoding !== undefined) {
                fileEncoding = encoding;
            }

            let content = fs.readFileSync(path.join(ext, file));
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