import fs = require("fs");
import path = require("path");

import { getCurrentContext } from './extension';

export class FileReader {

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    public static ReadJSONFromExtensionDir(file: string):any {
        try {
            const ext = getCurrentContext().extensionPath;

            const content = fs.readFileSync(path.join(ext, file));
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