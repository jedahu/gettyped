import {tsconfig} from "./config";
import ts from "./ts-services";

export const getTsOpts = () => ts.parseJsonConfigFileContent(
    tsconfig,
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
    ".").options;
