import * as ts from "typescript";
import {StringHost} from "./languageServiceHost";
import * as libEs2015 from "../lib/dts-es2015.txt";
import * as almond from "raw-loader!almond/almond";
import * as jsonOpts from "../../tsconfig-base.json";
import {makeApi} from "ts-rpc/worker";
import * as api from "./api";
import {either} from "fp-ts";

const {left, right} = either;
type Either<A, B> = either.Either<A, B>;

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

const diagnosticMessages =
    (d: ts.DiagnosticMessageChain)
        : Array<api.DiagnosticMessage> => {
        const acc = [];
        acc.push({ text: d.messageText, code: d.code, category: d.category });
        while (d.next) {
            d = d.next;
            acc.push({ text: d.messageText, code: d.code, category: d.category });
        }
        return acc;
    };

const diagnostic = (d: ts.Diagnostic): api.Diagnostic => ({
    start: d.start,
    length: d.length,
    source: d.source,
    moduleName: d.file.moduleName,
    fileName: d.file.fileName,
    messages:
        typeof d.messageText === "string"
        ? [{text: d.messageText, code: d.code, category: d.category}]
        : diagnosticMessages(d.messageText)
})

const moduleDiagnostics = (name : string) : api.Diagnostics => {
    const syntax = lang.getSyntacticDiagnostics(name + ".ts");
    const semantics = lang.getSemanticDiagnostics(name + ".ts");
    return syntax.concat(semantics).map(diagnostic);
}
makeApi<api.Arg, api.Ret>({
    initialize: ({modules}, cont) => {
        try {
            host.setScript("lib.es2015.d.ts", libEs2015);
            modules.forEach(
                ({name, code}) => host.setScript(name + ".ts", code));
            const diags =
                arrayFlatMap(
                    ({name}) => moduleDiagnostics(name),
                    modules);
            cont(right<string, api.Diagnostics>(diags));
        }
        catch (e) {
            cont(left<string, api.Diagnostics>(errorJson(e)));
        }
    },
    setModule: ({name, code}, cont) => {
        host.setScript(name + ".ts", code);
        cont(right<string, api.Diagnostics>(moduleDiagnostics(name)));
    },
    evalModule: ({moduleName}, cont) => {
        let js = "";
        try {
            try {
                const deps = moduleTransitiveDeps(moduleName);
                const outs = deps.map(m => ({
                    module: m,
                    diags: moduleDiagnostics(m),
                    js: lang.getEmitOutput(m + ".ts").
                        outputFiles.
                        map(f => f.text).join("\n")
                }));
                const diags = arrayFlatMap(x => x.diags, outs);
                if (diags.length > 0) {
                    cont(
                        right<string, Either<api.Diagnostics, any>>(
                            left<api.Diagnostics, any>(diags)));
                }
                else {
                    js = deps.map(
                        m => lang.getEmitOutput(m + ".ts").
                            outputFiles.map(f => f.text).join("\n")
                    ).join("\n");
                }
            }
            catch (error) {
                cont(left<string, Either<api.Diagnostics, any>>(errorJson(error)));
            }
            cont(
                right<string, Either<api.Diagnostics, any>>(
                    right<api.Diagnostics, any>(
                        new Function(`
                            ${almond};
                            ${js};
                            return require('${moduleName}').__eval();
                        `)()
                    )));
        }
        catch (error) {
            cont(left<string, Either<api.Diagnostics, any>>(errorJson(error)));
        }
    }
});
