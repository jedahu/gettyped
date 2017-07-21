import * as array from "fp-ts/lib/Array";
import * as option from "./option";
import * as str from "./string";
import {Config} from "./types";
import {Module} from "./module";
import {Page} from "./page";
import {addTs} from "./path";
import {getTsOpts} from "./tsconfig";
import {manageFocusOutlines} from "./focus-outline";
import {monaco} from "./monaco";
import {pair} from "./pair";
import {siteRoot, scrollbarSize} from "./config";

const fetchText = (url : string) : Promise<string> =>
    fetch(url).then(r => r.text());

const libs_d_ts = () =>
    [
        "libs.d.ts"
    ].map(async name => {
        const content = await fetchText(siteRoot + "/" + name);
        return {name, content};
    });

const initEditors = (config : Config) : void => {
    const opts = getTsOpts();
    const m = monaco;
    const mts = m.languages.typescript;
    mts.typescriptDefaults.setCompilerOptions({
        ...opts,
        noEmit: false,
        baseUrl: "/",
        noEmitHelpers: true,
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
    mts.typescriptDefaults.setEagerModelSync(true);
    mts.typescriptDefaults.setMaximunWorkerIdleTime(10);
    monaco.editor.defineTheme("lighter-default", {
        base: "vs",
        inherit: true,
        colors: {
            "editor.background": "#f0f0f0"
        },
        rules: [{token: "", foreground: "444444"}]
    });
    for (const p of libs_d_ts()) {
        p.then(({name, content}) => {
            mts.typescriptDefaults.addExtraLib(content, name);
        });
    }

    const sections =
        array.mapOption(
            target => {
                const path = target.getAttribute("rundoc-module");
                const test =
                    target instanceof HTMLElement
                    && !!path
                    && !str.isBlank(path);
                return option.guard(
                    test,
                    () => pair(addTs(path as string), target as HTMLElement));
            },
            Array.from(
                document.querySelectorAll(
                    "[rundoc-language='ts'][rundoc-module]")));

    const mods = array.map(([path, target]) => {
        const text = target.innerText.trim();
        const cwd = config.pageCwd;
        const invisible = !!target.getAttribute("rundoc-invisible");
        return Module.mk({
            path,
            cwd,
            text,
            target,
            replaceTarget: true,
            scrollbarSize,
            editorOnly: true,
            invisible
        });
    }, sections);

    (window as any).__gtPage = Page.mk({
        cwd: config.pageCwd,
        modules: mods
    });
};

export const init = (config : Config) => {
    manageFocusOutlines(document, "visible-focus-outline");
    const editToggle = document.querySelector(".gt-edit-toggle");
    if (localStorage.getItem("gt-edit-toggle-on")) {
        requestIdleCallback(() => {
            initEditors(config);
            if (editToggle) {
                editToggle.classList.add("gt-edit-toggle-on");
            }
        });
    }
    if (editToggle) {
        const editIcon = editToggle.getElementsByTagName("i")[0];
        editToggle.addEventListener("click", () => {
            if (editToggle.classList.contains("gt-edit-toggle-on")) {
                editToggle.classList.remove("gt-edit-toggle-on");
                localStorage.removeItem("gt-edit-toggle-on");
                window.location.reload();
            }
            else {
                editIcon.classList.add("gt-run-spinner");
                requestAnimationFrame(() => {
                    initEditors(config);
                    requestAnimationFrame(() => {
                        editIcon.classList.remove("gt-run-spinner");
                        editToggle.classList.add("gt-edit-toggle-on");
                        localStorage.setItem("gt-edit-toggle-on", "1");
                    });
                });
            }
        });
    }
};
