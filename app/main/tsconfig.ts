import * as ts from "typescript";
import * as jsonOpts from "../../tsconfig-base.json";

const opts = ts.parseJsonConfigFileContent(
    jsonOpts,
    {
        useCaseSensitiveFileNames: true,
        readDirectory(
            rootDir: string,
            extensions: string[],
            excludes: string[],
            includes: string[]
        ) : string[] {
            return [];
        },
        fileExists(path : string) : boolean {
            throw new Error("should not be used");
        },
        readFile(path : string) : string {
            throw new Error("should not be used");
        }
    },
    ".");

const config : ts.CompilerOptions = {
    ...opts.options,
    module: ts.ModuleKind.AMD
};

export default config;