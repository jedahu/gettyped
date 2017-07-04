import {getTsOpts} from "./tsconfig";
import global from "./global";
import {objMap, objValues, objEntries, arrayFlatMap} from "./util";

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

const siteRoot = global.__gt.siteRoot;
const scrollbarSize = global.__gt.scrollbarSize;

global.require.config({paths: {vs: `${siteRoot}/vs`}});

window.addEventListener(
    "load",
    () => global.require([
        "vs/editor/editor.main",
        "vs/language/typescript/lib/typescriptServices"
    ], (_ : any, ts : TS) => {
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
            inlineSourceMap: true,
            inlineSources: true,
            noImplicitAny: true,
            module: mts.ModuleKind.AMD,
            jsx: undefined as any,
            paths: undefined as any
        });
        mts.typescriptDefaults.setDiagnosticsOptions({
            noSemanticValidation: false,
            noSyntaxValidation: false
        });

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
                                lineNumbers: "off",
                                scrollBeyondLastLine: false,
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
            async (modules : Array<Module>) : Promise<string> => {
                const emits = await Promise.all(
                    modules.map(async ({uri, path}) => {
                        const client = await getClient(uri);
                        const emitted = await client.getEmitOutput(uri.toString());
                        return emitted.outputFiles[0].text.replace(/^define\(/, `define('${path}',`);
                    }));
                return emits.join("\n;");
            };

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
            | {tag : "runtime"; val : Error};

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
                    const js = await getJs(relevant);
                    try {
                        return {
                            tag: "value",
                            val: await new Function(`
return new Promise(function(resolve) {
    ${js}
    ;require(['${mod.path}'], function(m) { resolve(m.run()); });
});`)()
                        };
                    }
                    catch (e) {
                        return {
                            tag: "runtime",
                            val: e
                        };
                    }
                }
            };

        const runModuleDisplay =
            async (mod : Module, modules : Modules) : Promise<void> => {
                const x = await runModule(mod, modules, _ => {});
                if (x.tag === "diagnostics") {
                    console.log(x.val);
                }
                else if (x.tag === "value") {
                    console.log(x.val);
                }
                else if (x.tag === "runtime") {
                    console.log(x.val);
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
    }));
