import {getTsOpts} from "./tsconfig";
import {objMap, objValues, objEntries, arrayFlatMap} from "./util";
import {manageFocusOutlines} from "./focus-outline";
import {withGtLib} from "./gt-lib";
import * as monaco from "./monaco";
import * as tss from "./ts-services";
import {Diag, RunRet, Editor, Module, Modules} from "./types";
import {clearOutput, writeResult} from "./output";
import {data, html as h} from "./dom";
import {amdRequire} from "./amd";
import {siteRoot, scrollbarSize} from "./config";
import {prequire} from "./prequire";

declare function requestIdleCallback(f : () => void) : void;

const fetchText = (url : string) : Promise<string> =>
    window.fetch(url).then(r => r.text());

const libs_d_ts = () =>
    [
        "libs.d.ts"
    ].map(async name => {
        const content = await fetchText(siteRoot + "/" + name);
        return {name, content};
    });

const getWorker = () =>
    monaco.languages.typescript.getTypeScriptWorker();

const getClient = async (uri : monaco.Uri) : Promise<ts.LanguageService> => {
    const worker = await getWorker();
    return await worker(uri);
};

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

const importedPaths =
    (mod : Module) : Array<string> =>
    tss.preProcessFile(mod.model.getValue(), true, false).
    importedFiles.
    map(f => f.fileName).
    filter(p => p !== "gt-lib");

const updateImports = (mod : Module) : void =>
    requestIdleCallback(() => {
        mod.imports = importedPaths(mod);
    });

const importedModules =
    (mod : Module, modules : Modules) : Array<Module> =>
    importedPaths(mod).map(p => modules[p]);

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
                    range: model.getFullModelRange(),
                    text: model.getValue(),
                    forceMoveMarkers: true
                }]);
                editor.setSelection(new monaco.Range(0, 0, 0, 0));
                editor.setPosition(pos);
            });
        }
    });

const getDiagnostics = async (uri : monaco.Uri) : Promise<Array<Diag>> => {
    const client = await getClient(uri);
    const [syntactic, semantic] = await Promise.all([
        client.getSyntacticDiagnostics(uri.toString()),
        client.getSemanticDiagnostics(uri.toString())
    ]);
    return syntactic.map((d) : Diag => ({...d, diagType : "syntax"})).
        concat(semantic.map((d) : Diag => ({...d, diagType: "types"})));
};

type DiagMap = {[key : string] : Array<Diag>};

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

const updateJs =
    (modules : Array<Module>) : Promise<void> =>
    Promise.all(
        modules.map(async m => {
            const client = await getClient(m.uri);
            const emitted = await client.getEmitOutput(m.uri.toString());
            m.js = emitted.
                outputFiles[0].
                text.
                replace(/^define\(/, `define('${m.path}',`) +
                `\n//# sourceURL=${m.path}.ts`;
        })).then(_ => {});

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

const runModule =
    async (mod : Module, modules : Modules) : Promise<RunRet> => {
        const imports = await transitivelyImportedModules(mod, modules);
        const relevant = imports.concat(mod);
        const diagMap = await getDiagnosticsMap(relevant);
        if (Object.keys(diagMap).length > 0) {
            return {
                tag: "diagnostics",
                val: arrayFlatMap(
                    objEntries(diagMap),
                    ([path, diags]) =>
                        diags.map(d => {
                            const pos =
                                d.start != undefined
                                ? modules[path].model.getPositionAt(d.start)
                                : undefined;
                            const position =
                                pos
                                ? {line: pos.lineNumber, column: pos.column}
                                : undefined;
                            return ({
                                module: path,
                                message: diagnosticMessage(d),
                                diagType: d.diagType,
                                position
                            });
                        }))
            }
        }
        else {
            return {
                tag: "run",
                val: withGtLib(mod, async () => {
                    for (const m of relevant) {
                        amdRequire.undef(m.path);
                    }
                    try {
                        await updateJs(relevant);
                        for (const m of relevant) {
                            // new Function(m.js)();
                            eval(m.js);
                        }
                        const [m] = await prequire([mod.path]);
                        const ret =
                            typeof m.run === "function"
                            ? m.run()
                            : undefined;
                        const val = ret instanceof Promise ? await ret : ret;
                        return ["value", val] as ["value", any];
                    }
                    catch (e) {
                        return ["runtime", e] as ["runtime", any];
                    }
                })
            };
        }
    };

const runModuleDisplay =
    async (mod : Module, modules : Modules) : Promise<void> => {
        mod.runButton.classList.add("gt-run-spinner");
        clearOutput(mod);
        try {
            await writeResult(
                mod,
                modules,
                await runModule(mod, modules));
        }
        finally {
            mod.runButton.classList.remove("gt-run-spinner");
        }
    };

const revertModule = (m : Module, ms : Modules) => {
    m.model.setValue(m.originalText);
    clearOutput(m);
};

const contentChangeHandler = (m : Module, ms : Modules) => {
    resizeEditor(m);
    updateImports(m);
    refreshDependentModules(m.path, ms);
};

const handleOutputClick = (ms : Modules) => (e : MouseEvent) => {
    const elem = e.target;
    if (elem instanceof Element) {
        const entry = elem.closest(".gt-log-goto");
        if (entry instanceof HTMLElement) {
            const {path, line, column} = data(entry);
            if (isFinite(line) && isFinite(column)) {
                const m = ms[path];
                m.editor.setPosition({lineNumber: line, column});
                m.editor.focus();
            }
        }
    }
};

export const init =
    () => {
        manageFocusOutlines(document, "visible-focus-outline");
        const sections =
            [].slice.call(document.getElementsByClassName("gt-module-section"));
        const opts = getTsOpts();
        const m = monaco;
        const mts = m.languages.typescript;
        mts.typescriptDefaults.setCompilerOptions({
            ...opts,
            noEmit: false,
            baseUrl: "/",
            typeRoots: [],
            allowNonTsExtensions: false,
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

        for (const p of libs_d_ts()) {
            p.then(({name, content}) => {
                mts.typescriptDefaults.addExtraLib(content, name);
            });
        }

        const modules : Modules =
            objMap<Module>(
                sections.
                    filter(
                        (sec : HTMLElement) => {
                            const sel = sec.getElementsByClassName("rundoc-block")[0];
                            return sel.getAttribute("rundoc-language") === "ts" &&
                                sel.getAttribute("rundoc-module");
                        }).
                    map((sec : HTMLElement) => {
                        const sel = sec.getElementsByClassName("rundoc-block")[0] as HTMLElement;
                        const text = sel.innerText.trim();
                        sel.textContent = "";
                        const path = sel.getAttribute("rundoc-module");
                        const uri = monaco.Uri.parse(`/${path}.ts`);
                        const model = monaco.editor.createModel("", "typescript", uri);
                        const editor : Editor =
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
                        const runButton =
                            h("button",
                              { class: "gt-action-run material-icons md-24 md-dark",
                                title: "Run"
                              },
                              ["play_circle_filled"]);
                        toolbar.appendChild(runButton);
                        const revertButton =
                            h("button",
                              { class: "gt-action-revert material-icons md-24 md-dark",
                                title: "Revert code"
                              },
                              ["restore"]);
                        toolbar.appendChild(revertButton);
                        const clearButton =
                            h("button",
                              { class: "gt-action-clear material-icons md-24 md-dark",
                                title: "Clear output"
                              },
                              ["clear"]);
                        toolbar.appendChild(clearButton);
                        const output = sec.getElementsByClassName("gt-module-output")[0];
                        return [path, {
                            editor,
                            originalText: text,
                            section: sec,
                            container: sel,
                            model,
                            path,
                            uri,
                            runButton,
                            revertButton,
                            clearButton,
                            output,
                            imports: [],
                            js: ""
                        }];
                    }));

        for (const m of objValues(modules)) {
            m.model.setValue(m.originalText);
            m.model.onDidChangeContent(() => contentChangeHandler(m, modules));
            m.runButton.addEventListener("click", () => runModuleDisplay(m, modules));
            m.revertButton.addEventListener("click", () => revertModule(m, modules));
            m.output.addEventListener("click", handleOutputClick(modules));
            m.clearButton.addEventListener("click", () => clearOutput(m));
            const spinner =
                m.section.getElementsByClassName("gt-editor-load-spinner")[0];
            spinner.classList.remove("gt-do-spin");
        }

        window.addEventListener("resize", () => resizeEditors(modules));

        resizeEditors(modules);
    };
