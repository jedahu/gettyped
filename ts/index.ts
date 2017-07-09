import {getTsOpts} from "./tsconfig";
import {assertNever, objMap, objValues, objEntries, arrayFlatMap} from "./util";
import {manageFocusOutlines} from "./focus-outline";
import monaco from "./monaco";
import ts from "./ts-services";

declare function requestIdleCallback(f : () => void) : void;

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
    clearButton : HTMLElement;
    output : HTMLElement;
};

type Modules = {[path : string] : Module};

const esc = (...s : Array<string>) : string => {
    const div = document.createElement("div");
    div.textContent = s.join("");
    return div.innerHTML;
};

const stringify = (x : any) : string =>
    typeof x === "string"
    ? esc(x)
    : typeof x === "undefined"
    ? "<span class='gt-log-special'>undefined</span>"
    : esc(JSON.stringify(x, null, 2));

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
                    range: model.getFullModelRange(),
                    text: model.getValue(),
                    forceMoveMarkers: true
                }]);
                editor.setSelection(new monaco.Range(0, 0, 0, 0));
                editor.setPosition(pos);
            });
        }
    });

type DiagType = "syntax" | "types";
type Diag = ts.Diagnostic & {diagType : DiagType};

const getDiagnostics = async (uri : monaco.Uri) : Promise<Array<Diag>> => {
    const client = await getClient(uri);
    const syntactic = await client.getSyntacticDiagnostics(uri.toString());
    const semantic = await client.getSemanticDiagnostics(uri.toString());
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

type DiagInfo = {
    module : string;
    message : string;
    diagType : DiagType;
    position? : {line : number; column : number;}
};

type RunRet =
    {tag : "diagnostics"; val : Array<DiagInfo>}
    | {tag : "value"; val : any}
    | {tag : "runtime"; val : any}
    | {tag : "require"; val : any};

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

const withLogCapture =
    async <A>(
        log : (...xs : Array<any>) => void,
        f : () => Promise<A>
    ) : Promise<A> => {
        const clog = console.log;
        console.log = log;
        try {
            return await f();
        }
        finally {
            console.log = clog;
        }
    };

const mkElement = (...html : Array<string>) : HTMLElement => {
    const div = document.createElement("div");
    div.innerHTML = html.join("");
    return div.firstElementChild as HTMLElement;
};

type LogTag = "result" | "log" | "syntax" | "types" | "runtime";

const logTagInfo = (tag : LogTag) : string =>
    ({
        result: "result",
        log: "info",
        syntax: "syntax error",
        types: "type error",
        runtime: "runtime error"
    })[tag];

const writeToOutput =
    (m : Module) =>
    (tag : LogTag) =>
    (html : Array<string>, data? : {[k : string] : string}) => {
        const entry =
            mkElement(
                `<li class='gt-log-entry gt-log-entry-${tag}'>`,
                "<span class='gt-log-tag'>",
                logTagInfo(tag),
                ":</span> ",
                html.join(""),
                // esc(xs.map(x => x.toString()).join(" ")),
                "</li>"
            );
        for (const [k, v] of objEntries(data || {})) {
            entry.dataset[k] = v;
        };
        m.output.appendChild(entry);
    };

const writeLog = (m : Module) => (...xs : Array<any>) =>
    writeToOutput(m)("log")(
        xs.map(
            x => "<span class='gt-log-item'>" +
                stringify(x) +
                "</span> "
        ));

const writeDiag = (m : Module) => (diag : DiagInfo) => {
    const pos = diag.position;
    writeToOutput(m)(diag.diagType)(
        [
            "<span class='gt-log-diag-module'>",
            esc(diag.module + ".ts"),
            "</span>: ",
            "<span class='gt-log-diag-message'>",
            esc(diag.message),
            "</span>"
        ],
        {
            line: pos === undefined ? "" : pos.line.toString(),
            column: pos === undefined ? "" :  pos.column.toString()
        });
};

const writeRuntime = (m : Module) => (err : any) => {
    const requireMap : any = (err as any).requireMap;
    if (err instanceof Error && requireMap) {
        writeToOutput(m)("runtime")([
            "<span class='gt-log-runtime-message'>",
            err.message,
            "</span>"
        ]);
    }
    else if (err instanceof Error) {
        writeToOutput(m)("runtime")([
            "<span class='gt-log-runtime-error-name'>",
            err.name,
            "</span>: ",
            "<span class='gt-log-runtime-message'>",
            err.message,
            "</span>"
        ]);
    }
    else {
        writeToOutput(m)("runtime")([stringify(err)]);
    }
};

const writeResult = (m : Module) => (x : any) =>
    writeToOutput(m)("result")([
        "<span class='gt-log-result'>",
        stringify(x),
        "</span>"
    ]);

const clearOutput = (m : Module) => {
    m.output.innerHTML = "";
};

const runModuleDisplay =
    async (mod : Module, modules : Modules) : Promise<void> => {
        mod.runButton.classList.add("gt-run-spinner");
        clearOutput(mod);
        try {
            const x = await withLogCapture(
                writeLog(mod),
                () => runModule(mod, modules));
            if (x.tag === "diagnostics") {
                for (const diag of x.val) {
                    writeDiag(mod)(diag)
                }
            }
            else if (x.tag === "value") {
                writeResult(mod)(x.val);
            }
            else if (x.tag === "runtime" || x.tag === "require") {
                writeRuntime(mod)(x.val);
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
    clearOutput(m);
};

const contentChangeHandler = (m : Module, ms : Modules) => {
    resizeEditor(m);
    updateImports(m);
    refreshDependentModules(m.path, ms);
};

const handleOutputClick = (m : Module, ms : Modules) => (e : MouseEvent) => {
    const elem = e.target;
    if (elem instanceof Element) {
        const entry = elem.closest(".gt-log-entry-syntax, .gt-log-entry-types");
        if (entry instanceof HTMLElement) {
            const line = parseInt(entry.dataset.line || "", 10);
            const column = parseInt(entry.dataset.column || "", 10);
            if (isFinite(line) && isFinite(column)) {
                m.editor.setPosition({lineNumber: line, column});
                m.editor.focus();
            }
        }
    }
};

export const init =
    () => {
        manageFocusOutlines(document, "visible-focus-outline");
        const opts = getTsOpts(ts);
        const m = monaco;
        const mts = m.languages.typescript;
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
                        runButton.textContent = "play_circle_filled";
                        runButton.setAttribute("title", "Run");
                        toolbar.appendChild(runButton);
                        const revertButton = document.createElement("button");
                        revertButton.className = "gt-action-revert material-icons md-24 md-dark";
                        revertButton.textContent = "restore";
                        revertButton.setAttribute("title", "Revert code");
                        toolbar.appendChild(revertButton);
                        const clearButton = document.createElement("button");
                        clearButton.className = "gt-action-clear material-icons md-24 md-dark";
                        clearButton.textContent = "clear";
                        clearButton.setAttribute("title", "Clear output");
                        toolbar.appendChild(clearButton);
                        const output = sec.getElementsByClassName("gt-module-output")[0];
                        return [path, {
                            editor,
                            originalText: text,
                            container: sel,
                            model,
                            path,
                            uri,
                            runButton,
                            revertButton,
                            clearButton,
                            output,
                            imports: []
                        }];
                    }));

        for (const m of objValues(modules)) {
            m.model.setValue(m.originalText);
            m.model.onDidChangeContent(() => contentChangeHandler(m, modules));
            m.runButton.addEventListener("click", () => runModuleDisplay(m, modules));
            m.revertButton.addEventListener("click", () => revertModule(m, modules));
            m.output.addEventListener("click", handleOutputClick(m, modules));
            m.clearButton.addEventListener("click", () => clearOutput(m));
        }

        window.addEventListener("resize", () => resizeEditors(modules));

        resizeEditors(modules);
    };
