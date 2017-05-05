import * as ts from "typescript";

type Script = {version : number, text : string};
type Scripts = {[name : string] : Script};

export class StringHost implements ts.LanguageServiceHost {
    readonly options : ts.CompilerOptions;
    readonly scripts : Scripts;

    constructor(options : ts.CompilerOptions, scripts? : Scripts) {
        this.options = options;
        this.scripts = scripts || {};
    }

    setScript(name : string, text : string) : void {
        const old = this.scripts[name];
        const oldVersion = old ? old.version : 0;
        const modName = name.endsWith(".ts")
            ? name.substring(0, name.length - 3)
            : name;
        this.scripts[name] = {
            version: oldVersion + 1,
            text: `///<amd-module name='${modName}'/>\n${text}`
        };
    }

    getCompilationSettings() : ts.CompilerOptions {
        return this.options;
    }

    getCurrentDirectory() : string {
        return ".";
    }

    getDefaultLibFileName(options : ts.CompilerOptions) : string {
        return "lib.es6.d.ts";
    }

    getScriptFileNames() : string[] {
        return Object.keys(this.scripts);
    }

    getScriptSnapshot(name : string) : ts.IScriptSnapshot {
        const script = this.scripts[name];
        if (!script) return undefined;
        const text = script.text;
        return {
            getChangeRange: (_old : ts.IScriptSnapshot) => undefined,
            getLength: () => text.length,
            getText: (beg : number, end : number) => text.substring(beg, end),
            toString: () => text
        };
    }

    getScriptVersion(name : string) : string {
        const script = this.scripts[name];
        if (!script) return undefined;
        return script.version.toString();
    }

    resolveModuleNames(names : string[], file : string) : ts.ResolvedModule[] {
        return names.map(name => {
            return {
                isExternalLibraryImport: false,
                resolvedFileName: name + ".ts"
            };
        });
    }
}
