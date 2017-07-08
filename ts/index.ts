import {getTsOpts} from "./tsconfig";
import {assertNever, objMap, objValues, objEntries, arrayFlatMap} from "./util";
import {manageFocusOutlines} from "./focus-outline";

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
    revertButton : HTMLElement;
};

type Modules = {[path : string] : Module};

const resetRequireError = () => {
    window.require.onError = (e : any) => { throw e; };
};

const prequire = async (paths : Array<string>) : Promise<any> =>
    new Promise((res, rej) => {
        window.require.onError = rej;
        window.require(
            paths,
            function() {
                res([].slice.call(arguments));
            });
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

const scrollbarSize = window.__gt.scrollbarSize;
const siteRoot = window.__gt.siteRoot;

const lib_es6_d_ts =
    window.fetch(`${siteRoot}/lib.es6.d.ts`).
    then(r => r.text());

export const init =
    () => window.require([
        "vs/editor/editor.main",
        "vs/language/typescript/lib/typescriptServices"
    ] , (_ : any, ts : TS) => {
        manageFocusOutlines(document, "visible-focus-outline");
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

        const sections = document.getElementsByClassName("gt-module-section");
        const modules : Modules =
            objMap<Module>(
                [].slice.call(sections).
                    filter(
                        (sec : HTMLElement) => {
                            const sel = sec.getElementsByClassName("rundoc-block")[0];
                            return sel.getAttribute("rundoc-language") === "ts" &&
                                sel.getAttribute("rundoc-module");
                        }).
                    map((sec : HTMLElement) => {
                        const summary = sec.getElementsByTagName("summary")[0];
                        const lessMore = document.createElement("i");
                        lessMore.className = "gt-less-more material-icons md-24 md-dark";
                        summary.appendChild(lessMore);
                        const sel = sec.getElementsByClassName("rundoc-block")[0] as HTMLElement;
                        const text = sel.innerText.trim();
                        sel.textContent = "";
                        const path = sel.getAttribute("rundoc-module");
                        const uri = monaco.Uri.parse(`/${path}.ts`);
                        const model = monaco.editor.createModel("", "typescript", uri);
                        const editor : Ed =
                            monaco.editor.create(sel, {
                                model,
                                lineNumbers: "off",
                                scrollBeyondLastLine: false,
                                minimap: {
                                    enabled: false
                                },
                                scrollbar: {
                                    vertical: "hidden",
                                    horizontalScrollbarSize: scrollbarSize
                                }
                            });
                        const toolbar = sec.getElementsByClassName('gt-module-tools')[0];
                        const runButton = document.createElement("button");
                        runButton.className = "gt-action-run material-icons md-24 md-dark";
                        runButton.innerText = "play_circle_filled";
                        runButton.setAttribute("title", "Run this module");
                        toolbar.appendChild(runButton);
                        const revertButton = document.createElement("button");
                        revertButton.className = "gt-action-revert material-icons md-24 md-dark";
                        revertButton.innerText = "restore";
                        revertButton.setAttribute("title", "Revert this module");
                        toolbar.appendChild(revertButton);
                        return [path, {
                            editor,
                            originalText: text,
                            container: sel,
                            model,
                            path,
                            uri,
                            runButton,
                            revertButton,
                            imports: []
                        }];
                    }));

        const resizeEditor = ({editor, model, container} : Module) : void => {
            window.requestAnimationFrame(() => {
                const lh = editor.getConfiguration().lineHeight;
                const lc = model.getLineCount();
                const height = (lh * lc) + scrollbarSize;
                container.style.height = `${height}px`;
                editor.layout();
            });
        };

        const resizeEditors = (mods : Modules) : void => {
            for (const m of objValues(mods)) {
                m.editor.layout();
                resizeEditor(m);
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

        // const transitivelyDependentModules =
        //     (path : string, mods : Modules) : Array<Module> => {
        //         const deps : Array<Module> = [];
        //         const depset = new Set<string>();
        //         const go = (path : string) => {
        //             const ms = dependentModules(path, mods);
        //             for (const m of ms) {
        //                 if (!depset.has(m.path)) {
        //                     depset.add(m.path);
        //                     deps.push(m);
        //                 }
        //             }
        //             for (const m of ms) {
        //                 go(m.path);
        //             }
        //         }
        //         go(path);
        //         return deps;
        //     };

        const refreshDependentModules = (path : string, mods : Modules) =>
            requestIdleCallback(() => {
                const deps = dependentModules(path, mods);
                for (const {editor, model} of deps) {
                    requestIdleCallback(() => {
                        const pos = editor.getPosition();
                        editor.executeEdits("force-recheck", [{
                            identifier: {major: 1, minor: 1},
                            range: new monaco.Range(1, 1, 10000, 1),
                            text: model.getValue(),
                            forceMoveMarkers: true
                        }]);
                        editor.setSelection(new monaco.Range(0, 0, 0, 0));
                        editor.setPosition(pos);
                    });
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
                        window.require.undef(m.path);
                    }
                    try {
                        const sources = await getJs(relevant);
                        for (const src of sources) {
                            new Function(src)();
                        }
                        const [m] = await prequire([mod.path]);
                        const ret =
                            typeof m.run === "function"
                            ? m.run()
                            : undefined;
                        const val = ret instanceof Promise ? await ret : ret;
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
                mod.runButton.classList.add("gt-run-spinner");
                try {
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
                                ? {
                                    tag: x.tag,
                                    val: x.val.message,
                                    module: (x.val as any).requireMap.name
                                }
                            : x);
                    }
                    else {
                        assertNever(x);
                    }
                }
                finally {
                    mod.runButton.classList.remove("gt-run-spinner");
                }
            };

        const revertModule = (m : Module, ms : Modules) => {
            m.model.setValue(m.originalText);
        };

        const contentChangeHandler = (m : Module, ms : Modules) => {
            resizeEditor(m);
            updateImports(m);
            refreshDependentModules(m.path, ms);
        };

        for (const module of objValues(modules)) {
            const {model, originalText, runButton, revertButton} = module;
            model.setValue(originalText);
            model.onDidChangeContent(() => contentChangeHandler(module, modules));
            runButton.addEventListener("click", () => runModuleDisplay(module, modules));
            revertButton.addEventListener("click", () => revertModule(module, modules));
        }

        window.addEventListener("resize", () => resizeEditors(modules));

        resizeEditors(modules);
    });
