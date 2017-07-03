import {getTsOpts} from "./tsconfig";
import global from "./global";

declare function requestIdleCallback(f : () => void) : void;

type TS = typeof ts;
type Ed = monaco.editor.IStandaloneCodeEditor;
type Mod = monaco.editor.IModel;

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
        mts.typescriptDefaults.setCompilerOptions({
            ...opts,
            noEmit: false,
            baseUrl: "/",
            typeRoots: [],
            allowNonTsExtensions: false,
            lib: ["es6", "dom"],
            module: mts.ModuleKind.AMD,
            jsx: undefined as any,
            paths: undefined as any
        });
        mts.typescriptDefaults.setDiagnosticsOptions({
            noSemanticValidation: false,
            noSyntaxValidation: false
        });

        const snippetElems = document.getElementsByClassName("rundoc-block");
        const things : Array<{editor : Ed; text : string; container : HTMLElement;}> =
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
                return {editor, text, container: sel};
            });

        const resizeEditors = () =>
            things.forEach(({editor, container}) => {
                editor.layout();
                window.requestAnimationFrame(() => {
                    container.style.height = `${editor.getScrollHeight()}px`;
                    editor.layout();
                });
            });

        const refreshSubsequentModels = (m : Mod) =>
            requestIdleCallback(() => {
                const idx =
                    things.findIndex(({editor}) => editor.getModel().id === m.id);
                if (idx >= 0) {
                    things.slice(idx + 1).forEach(({editor}) => {
                        const model = editor.getModel();
                        requestIdleCallback(() => model.setValue(model.getValue()));
                    });
                }
            });

        things.forEach(({editor, text, container}) => {
            const model = editor.getModel();
            model.setValue(text);
            model.onDidChangeContent(() => refreshSubsequentModels(model));
        });

        window.addEventListener("resize", resizeEditors);

        global.__gt.editors = things.map(({editor, container}) => ({editor, container}));

        resizeEditors();
    }));
