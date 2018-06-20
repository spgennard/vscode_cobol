'use strict';

import { Range, TextDocument, workspace, Definition, Position, CancellationToken, ProviderResult, Uri, Location } from 'vscode';


const regexes = {
    copy: {
        regx: / COPY {1,}(\'([^ ]+)\')?([^ ]+)? ?\./i,
        stmtType: 'COPY',
        capIndex: { 
            hardCodeCopySource: 2,
            variableCopySouurce: 3,
        },
        provideDefinition: (lineText: string) => {
            return new Promise((resolve, reject) => {
                regexes.copy.regx.test(lineText);
    
                var  m;
                if((m = regexes.copy.regx.exec(lineText)) === null) {
                    return reject({});
                }
                if(!m[regexes.copy.capIndex.hardCodeCopySource]) {
                    return reject({});
                }
                
                const filename = m[regexes.copy.capIndex.hardCodeCopySource];
                    
                workspace
                .findFiles(`**/${filename}.{[Cc][Bb][Ll],[Cc][Oo][Bb],[Cc][Pp][Yy]}`)
                .then((files: any[]) => {
                    resolve(new Location(
                        Uri.file(files[0].path),
                        new Range(new Position(0, 0), new Position(0, 0))
                    ));
                });
            });

        }
    },
    fieldDefinition: {
        regx: / {0,}([0-9]+) +([a-zA-Z-0-9-#]+)/i,
        stmtType: 'FIELD_DEFINITION',
        capIndex: { 
            level: 1,
            name: 2,
        },
        provideDefinition: (lineText: string) => {
            return new Promise((resolve, reject) => {
                regexes.fieldDefinition.regx.test(lineText);
                return reject({});
            });
        }
    }
};

function matchStatement(statement: string){
    const regs = [regexes.copy, regexes.fieldDefinition];
    for (let m = 0; m < regs.length; m++) {
        const regex = regs[m].regx;
        if( regex.test(statement) ) {
            return regs[m];
        }
    }
}




export function provideDefinition(document: TextDocument, position: Position, token: CancellationToken): ProviderResult<Definition> {
    const line = document.lineAt(position);
    const stmtType = matchStatement(line.text);
    if (!stmtType) { 
        return new Promise((resolve, reject)=> {
            return resolve(null);
        });
    }
    
    return <ProviderResult<Definition>>stmtType.provideDefinition(line.text);

}
