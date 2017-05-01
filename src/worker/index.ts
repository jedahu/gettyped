import * as fs from "fs";
import * as ts from "typescript";
import {TypeScriptSimple as TSS} from "lib/typescript-simple";
import * as api from "./api";

TSS.prototype["formatDiagnostics"] = function (diagnostics) {
    return diagnostics.map(function (d : any) {
        if (d.file) {
            return 'L' +
                d.file.getLineAndCharacterOfPosition(d.start).line +
                ': ' +
                (typeof d.messageText === "string"
                 ? d.messageText
                 : d.messageText.messageText ||
                 "unable to parse error diagnostic");
        }
        else {
            return d.messageText;
        }
    }).join("\n");
};

const compilerOpts : ts.CompilerOptions =
    JSON.parse(fs.readFileSync("tsconfig.json").toString()).compilerOptions;

delete compilerOpts.rootDir;
delete compilerOpts.baseUrl;
delete compilerOpts.outDir;

const tss = new TSS(compilerOpts);

const nullOpts = {...compilerOpts, strictNullChecks: false};

const tssNull = new TSS(nullOpts);

const compiler = (module : string) =>
    module.includes("re-null") ? tssNull : tss;

let moduleOrder : Array<string> = [];

const moduleSource : {[key : string] : string} = {};

const moduleJs : {[key : string] : string} = {};

const errorJson = (e : any) : string =>
    e instanceof Error && e.message
    ? "" + e.message
    : JSON.stringify(e);

const errorLines = (error : any) : Array<[number, string]> =>
    error instanceof Error
    ? error.message.split("\n").
        filter(l => /^L[0-9]/.test(l)).
        map(l => <[number, string]>[parseInt(l.substring(1)), l])
    : [];

const uniq = (xs : Array<string>) => {
    const seen : {[key : string] : boolean} = {};
    return xs.filter(x => seen.hasOwnProperty(x) ? false : (seen[x] = true));
};

const arrayFlatMap = <A, B>(f : (a:A) => Array<B>, xs : Array<A>) : Array<B> =>
      <Array<B>>[].concat.apply([], xs.map(f));

const pathToNs = (path : string) : string =>
    path.replace(/[-\/]/g, '_');

const isImportDeclaration = (node : ts.Node) : node is ts.ImportDeclaration =>
    node.kind === ts.SyntaxKind.ImportDeclaration;

const isStringLiteral = (node : ts.Node) : node is ts.StringLiteral =>
    node.kind === ts.SyntaxKind.StringLiteral;

const moduleDeps = (module : string) : Array<string> => {
    const deps : Array<string> = [];
    const node : ts.SourceFile =
        ts.createSourceFile(
            "tmp.ts",
            moduleSource[module],
            ts.ScriptTarget.ES2015,
            true);
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

const rewriteModule = (module : string) : string => {
    const src = moduleSource[module];
    const ns = pathToNs(module);
    const newSrc = src.split("\n").map(line => {
        const importAs = line.match(
                /^\s*import\s\*\sas\s([^\s]+)\sfrom\s[\'\"]([^\'\"]+)[\'\"];/);
        if (importAs) {
            const name = importAs[1];
            const path = importAs[2];
            const ns = pathToNs(path);
            return `import ${name} = ${ns};`
        }
        const importDefault = line.match(
                /^\s*import\s([^\s]+)\sfrom\s[\'\"]([^\'\"]+)[\'\"];/);
        if (importDefault) {
            throw new Error("Default imports not supported");
        }
        const importSome = line.match(
                /^\s*import\s(\{[^\}]+\})\sfrom\s[\'\"]([^\'\"]+)[\'\"];/);
        if (importSome) {
            throw new Error("Named imports not supported");
        }
        return line;
    }).join("\n");
    return `namespace ${ns} {\n${newSrc}\n}\n`;
};

const moduleTransitiveDeps = (module : string) : Array<string> => {
    const go = (m : string) => {
        const deps = moduleDeps(m);
        const recur = arrayFlatMap(moduleTransitiveDeps, deps);
        return deps.concat(recur);
    };
    return uniq([module].concat(go(module))).reverse();
};

const comment = (module : string) : string => {
    const src = moduleSource[module];
    const newSrc = src.replace(/\n/g, "\n//");
    return `// namespace ${module} {\n// ${newSrc}\n//}\n`;
};

const moduleCode = (module : string) : string => {
    const deps = new Set(moduleTransitiveDeps(module));
    return moduleOrder.map(m => {
        return deps.has(m) ? rewriteModule(m) : comment(m);
    }).join("\n");
};

const actions : {[key : string] : Function} = {
    initialize: ({modules} : api.Initialize) : api.InitializeResponse => {
        try {
            moduleOrder = modules.map(([name, _]) => name);
            modules.forEach(([name, src]) => moduleSource[name] = src);
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
        moduleSource[module] = code;
        return {
            action: "source",
            status: "ok"
        };
    },

    evaluate: ({module} : api.Evaluate) : api.EvaluateResponse => {
        const ns = pathToNs(module);
        try {
            try {
                moduleJs[module] = compiler(module).compile(moduleCode(module));
            }
            catch (error) {
                return {
                    action: "evaluate",
                    status: "error",
                    kind: "compile",
                    module,
                    error: errorJson(error),
                    errorLines: errorLines(error)
                };
            }
            const js = moduleJs[module];
            return {
                action: "evaluate",
                status: "ok",
                module,
                value: new Function(`
                    ${js};
                    return ${ns}.__eval();
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

onmessage = e => {
    const data : api.Action = e.data;
    const action = actions[data.action];
    if (action) {
        postMessage(action(data));
    }
    else {
        postMessage({
            status: "missing",
            error: "no such action " + data.action
        });
    }
};
