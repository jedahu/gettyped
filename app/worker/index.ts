import * as ts from "typescript";
import * as api from "./api";
import {StringHost} from "./languageServiceHost";
import * as libEs2015 from "../lib/dts-es2015.txt";
import * as almond from "raw-loader!almond/almond";
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

const compilerOpts = {...opts.options, module: ts.ModuleKind.AMD};

// const nullOpts = {...compilerOpts, strictNullChecks: false};

const host = new StringHost(compilerOpts, {});

const lang = ts.createLanguageService(host);

const errorJson = (e : any) : string =>
    e instanceof Error && e.message
    ? "" + e.message
    : JSON.stringify(e);

const isImportDeclaration = (node : ts.Node) : node is ts.ImportDeclaration =>
    node.kind === ts.SyntaxKind.ImportDeclaration;

const isStringLiteral = (node : ts.Node) : node is ts.StringLiteral =>
    node.kind === ts.SyntaxKind.StringLiteral;

const uniq = (xs : Array<string>) => {
    const seen : {[key : string] : boolean} = {};
    return xs.filter(x => seen.hasOwnProperty(x) ? false : (seen[x] = true));
};

const arrayFlatMap = <A, B>(f : (a:A) => Array<B>, xs : Array<A>) : Array<B> =>
      <Array<B>>[].concat.apply([], xs.map(f));

const moduleDeps = (module : string) : Array<string> => {
    const deps : Array<string> = [];
    const node : ts.SourceFile = lang.getProgram().getSourceFile(module + ".ts");
    const go = (node : ts.Node) => {
        if (isImportDeclaration(node)
            && isStringLiteral(node.moduleSpecifier)
           ) {
            deps.push(node.moduleSpecifier.text);
        }
        ts.forEachChild(node, go);
    };
    go(node);
    return deps;
};

const moduleTransitiveDeps = (module : string) : Array<string> => {
    const go = (m : string) => {
        const deps = moduleDeps(m);
        const recur = arrayFlatMap(moduleTransitiveDeps, deps);
        return deps.concat(recur);
    };
    return uniq([module].concat(go(module))).reverse();
};

const actions : {[key : string] : Function} = {
    initialize: ({modules} : api.Initialize) : api.InitializeResponse => {
        try {
            host.setScript("lib.es2015.d.ts", libEs2015);
            modules.forEach(([name, src]) => host.setScript(name + ".ts", src));
            return {
                action: "initialize",
                status: "ok"
            };
        }
        catch (e) {
            return {
                action: "initialize",
                status: "error",
                error: errorJson(e)
            };
        }
    },

    source: ({module, code} : api.Source) : api.Success<"source"> => {
        host.setScript(module + ".ts", code);
        return {
            action: "source",
            status: "ok"
        };
    },

    evaluate: ({module} : api.Evaluate) : api.EvaluateResponse => {
        let js : string;
        try {
            try {
                const deps = moduleTransitiveDeps(module);
                const outs = deps.map(m => ({
                    module: m,
                    errSyntax: lang.getSyntacticDiagnostics(m + ".ts"),
                    errSemantic: lang.getSemanticDiagnostics(m + ".ts"),
                    js: lang.getEmitOutput(m + ".ts").
                        outputFiles.
                        map(f => f.text).join("\n")
                }));
                const hasErrors =
                    outs.find(
                        o => o.errSyntax.length + o.errSemantic.length > 0);
                if (hasErrors) {
                    return {
                        action: "evaluate",
                        status: "error",
                        kind: "compile",
                        module,
                        error: "tsc errors",
                        errors: outs.map(
                            o => ({
                                module: o.module,
                                syntax: o.errSyntax,
                                semantics: o.errSemantic
                            }))
                    };
                }
                js = deps.map(
                    m => lang.getEmitOutput(m + ".ts").
                        outputFiles.map(f => f.text).join("\n")
                ).join("\n");
            }
            catch (error) {
                return {
                    action: "evaluate",
                    status: "error",
                    kind: "compile",
                    module,
                    error: errorJson(error),
                    errors: []
                };
            }
            return {
                action: "evaluate",
                status: "ok",
                module,
                value: new Function(`
                    ${almond};
                    ${js};
                    return require('${module}').__eval();
                `)()
            };
        }
        catch (error) {
            return {
                action: "evaluate",
                status: "error",
                kind: "runtime",
                module,
                error: errorJson(error)
            };
        }
    }
};

onmessage = (e : any) => {
    const data : api.Action = e.data;
    const action = actions[data.action];
    if (action) {
        (<any>postMessage)(action(data));
    }
    else {
        (<any>postMessage)({
            status: "missing",
            error: "no such action " + data.action
        });
    }
};
