import {getTsOpts} from "./tsconfig";
import global from "./global";
import {assertNever, objMap, objValues, objEntries, arrayFlatMap} from "./util";

declare function requestIdleCallback(f : () => void) : void;

type TS = typeof ts;
type Ed = monaco.editor.IStandaloneCodeEditor;
type Mod = monaco.editor.IModel;

type Module = {
    editor : Ed;
    model : Mod;
    path : string;
    uri : monaco.Uri;
    imports : Array<string>;
    originalText : string;
    container : HTMLElement;
    runButton : HTMLElement;
};

type Modules = {[path : string] : Module};

const resetRequireError = () => {
    global.require.onError = (e : any) => { throw e; };
};

const prequire = async (paths : Array<string>) : Promise<any> =>
    new Promise((res, rej) => {
        global.require.onError = rej;
        global.require(
            paths,
            function() {
                res([].slice.call(arguments));
            },
            rej);
    }).
    then(
        x => {
            resetRequireError();
            return x;
        },
        e => {
            resetRequireError();
            throw e;
        });

const scrollbarSize = global.__gt.scrollbarSize;
const siteRoot = global.__gt.siteRoot;

const lib_es6_d_ts =
    global.fetch(`${siteRoot}/lib.es6.d.ts`).
    then(r => r.text());

export const init =
    () => global.require([
        "vs/editor/editor.main",
        "vs/language/typescript/lib/typescriptServices"
    ] , (_ : any, ts : TS) => {
        const opts = getTsOpts(ts);
        const m = monaco;
        const mts = m.languages.typescript;
        const getWorker = () =>
            monaco.languages.typescript.getTypeScriptWorker();
        const getClient = async (uri : monaco.Uri) : Promise<ts.LanguageService> => {
            const worker = await getWorker();
            return await worker(uri);
        };
        mts.typescriptDefaults.setCompilerOptions({
            ...opts,
            noEmit: false,
            baseUrl: "/",
            typeRoots: [],
            allowNonTsExtensions: false,
            lib: ["es6", "dom"],
            noImplicitAny: true,
            module: mts.ModuleKind.AMD,
            jsx: undefined as any,
            paths: undefined as any
        });
        mts.typescriptDefaults.setDiagnosticsOptions({
            noSemanticValidation: false,
            noSyntaxValidation: false
        });
        lib_es6_d_ts.then(
            dts => mts.typescriptDefaults.addExtraLib(dts, "lib.es6.d.ts"));

        const snippetElems = document.getElementsByClassName("rundoc-block");
        const modules : Modules =
            objMap<Module>(
                [].slice.call(snippetElems).
                    filter(
                        (sel : HTMLElement) =>
                            sel.getAttribute("rundoc-language") === "ts" &&
                            sel.getAttribute("rundoc-module")).
                    map((sel : HTMLElement) => {
                        const text = sel.innerText;
                        sel.textContent = "";
                        const path = sel.getAttribute("rundoc-module");
                        const uri = monaco.Uri.parse(`/${path}.ts`);
                        const model = monaco.editor.createModel("", "typescript", uri);
                        const editor : Ed =
                            monaco.editor.create(sel, {
                                model,
                                lineNumbers: "on",
                                scrollBeyondLastLine: false,
                                minimap: {
                                    enabled: false
                                },
                                scrollbar: {
                                    vertical: "hidden",
                                    horizontalScrollbarSize: scrollbarSize
                                }
                            });
                        const toolbar = document.createElement('div');
                        toolbar.innerHTML = `<button class='gt-run'>Run</button>`;
                        sel.appendChild(toolbar);
                        return [path, {
                            editor,
                            originalText: text,
                            container: sel,
                            model,
                            path,
                            uri,
                            runButton: toolbar.getElementsByClassName('gt-run')[0],
                            imports: []
                        }];
                    }));

        const resizeEditors = (mods : Modules) : void => {
            for (const {editor, container} of objValues(mods)) {
                editor.layout();
                window.requestAnimationFrame(() => {
                    container.style.height = `${editor.getScrollHeight()}px`;
                    editor.layout();
                });
            };
        };

        const updateImports = (mod : Module) =>
            requestIdleCallback(() => {
                mod.imports =
                    ts.preProcessFile(mod.model.getValue(), true, false).
                    importedFiles.
                    map(f => f.fileName);
            });

        const importedModules =
            (mod : Module, modules : Modules) : Array<Module> =>
            ts.preProcessFile(mod.model.getValue(), true, false).
            importedFiles.
            map(f => modules[f.fileName]);

        const transitivelyImportedModules =
            (mod : Module, modules : Modules) : Array<Module> => {
                const deps : Array<Module> = [];
                const depset = new Set<string>();
                const go = (mod : Module) => {
                    const ms = importedModules(mod, modules);
                    for (const m of ms) {
                        go(m);
                    }
                    for (const m of ms) {
                        if (!depset.has(m.path)) {
                            depset.add(m.path);
                            deps.push(m);
                        }
                    }
                }
                go(mod);
                return deps;
            };


        const dependentModules =
            (path : string, mods : Modules) : Array<Module> =>
            objValues(mods).filter(m => m.imports.includes(path));

        const transitivelyDependentModules =
            (path : string, mods : Modules) : Array<Module> => {
                const deps : Array<Module> = [];
                const depset = new Set<string>();
                const go = (path : string) => {
                    const ms = dependentModules(path, mods);
                    for (const m of ms) {
                        if (!depset.has(m.path)) {
                            depset.add(m.path);
                            deps.push(m);
                        }
                    }
                    for (const m of ms) {
                        go(m.path);
                    }
                }
                go(path);
                return deps;
            };

        const refreshDependentModules = (path : string, mods : Modules) =>
            requestIdleCallback(() => {
                const deps = transitivelyDependentModules(path, mods);
                for (const d of deps) {
                    requestIdleCallback(() => d.model.setValue(d.model.getValue()))
                }
            });

        const getDiagnostics = async (uri : monaco.Uri) : Promise<Array<ts.Diagnostic>> => {
            const client = await getClient(uri);
            const syntactic = await client.getSyntacticDiagnostics(uri.toString());
            const semantic = await client.getSemanticDiagnostics(uri.toString());
            return syntactic.concat(semantic);
        };

        type DiagMap = {[key : string] : Array<ts.Diagnostic>};

        const getDiagnosticsMap =
            async (modules : Array<Module>) : Promise<DiagMap> => {
                const xs = await Promise.all(
                    modules.map(async ({uri, path}) => {
                        const diags = await getDiagnostics(uri);
                        return {diags, path};
                    }));
                const dmap : DiagMap = {};
                for (const x of xs) {
                    if (x.diags.length > 0) {
                        dmap[x.path] = x.diags;
                    }
                }
                return dmap;
            };

        const getJs =
            (modules : Array<Module>) : Promise<Array<string>> =>
                Promise.all(
                    modules.map(async ({uri, path}) => {
                        const client = await getClient(uri);
                        const emitted = await client.getEmitOutput(uri.toString());
                        return emitted.
                            outputFiles[0].
                            text.
                            replace(/^define\(/, `define('${path}',`);
                    }));

        const diagnosticMessage = (diag : ts.Diagnostic) : string => {
            const msg = diag.messageText;
            if (typeof msg === "string") {
                return msg;
            }
            let chain = msg;
            let s = chain.messageText;
            while (chain.next) {
                chain = chain.next;
                s = s + "\n" + chain.messageText;
            };
            return s;
        };

        type RunRet =
            {tag : "diagnostics"; val : Array<{module : string; message : string}>}
            | {tag : "value"; val : any}
            | {tag : "runtime"; val : any}
            | {tag : "require"; val : any};

        const runModule =
            async (mod : Module, modules : Modules, log : (_:any) => void) : Promise<RunRet> => {
                const imports = await transitivelyImportedModules(mod, modules);
                const relevant = imports.concat(mod);
                const diagMap = await getDiagnosticsMap(relevant);
                if (Object.keys(diagMap).length > 0) {
                    return {
                        tag: "diagnostics",
                        val: arrayFlatMap(
                            objEntries(diagMap),
                            ([path, diags]) =>
                                diags.map(d => ({
                                    module: path,
                                    message: diagnosticMessage(d)
                                })))
                    }
                }
                else {
                    for (const m of relevant) {
                        global.require.undef(m.path);
                    }
                    try {
                        const sources = await getJs(relevant);
                        for (const src of sources) {
                            new Function(src)();
                        }
                        const [m] = await prequire([mod.path]);
                        const val =
                            typeof m.run === "function"
                            ? m.run()
                            : undefined;
                        return {
                            tag: "value",
                            val
                        };
                    }
                    catch (e) {
                        return {
                            tag: "require",
                            val: e
                        };
                    }
                    finally {
                        resetRequireError();
                    }
                }
            };

        const runModuleDisplay =
            async (mod : Module, modules : Modules) : Promise<void> => {
                const x = await runModule(mod, modules, _ => {});
                if (x.tag === "diagnostics") {
                    console.log(x);
                }
                else if (x.tag === "value") {
                    console.log(x);
                }
                else if (x.tag === "runtime" || x.tag === "require") {
                    console.log(
                        x.val instanceof Error
                        ? {tag: x.tag, val: x.val.message}
                        : x);
                }
                else {
                    assertNever(x);
                }
            };

        const contentChangeHandler = (m : Module, ms : Modules) => {
            updateImports(m);
            refreshDependentModules(m.path, ms);
        };

        for (const module of objValues(modules)) {
            const {model, originalText, runButton} = module;
            model.setValue(originalText);
            model.onDidChangeContent(() => contentChangeHandler(module, modules));
            runButton.addEventListener("click", () => runModuleDisplay(module, modules));
        }

        window.addEventListener("resize", () => resizeEditors(modules));

        resizeEditors(modules);
    });
