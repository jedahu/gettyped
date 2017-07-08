type TS = typeof ts;

const jsonOpts : any = window.__gt.tsconfig;

export const getTsOpts = (ts : TS) => ts.parseJsonConfigFileContent(
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
    ".").options;
