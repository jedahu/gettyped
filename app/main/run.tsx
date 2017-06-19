import * as React from "react";
import * as ReactDOM from "react-dom";
import * as injectTapEventPlugin from "react-tap-event-plugin";
import {FocusStyleManager} from "@blueprintjs/core";

import "normalize.css/normalize.css";
import "@blueprintjs/core/dist/blueprint.css";
import "../../scss/typeset.scss";
import "../../scss/main.scss";

import tsconfig from "./tsconfig";

type Global = typeof window & {require : any};

const global : Global = window as any;

global.addEventListener(
    "load",
    () => global.require(["vs/editor/editor.main"], () => {
        injectTapEventPlugin();
        FocusStyleManager.onlyShowFocusOnTabs();

        const App = require("./index");
        const AppElem = App.mk();

        const m = monaco;
        const mts = m.languages.typescript;
        mts.typescriptDefaults.setCompilerOptions({
            ...tsconfig,
            noEmit: false,
            baseUrl: "/",
            typeRoots: ["node_modules/@types"],
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

        const elem = document.createElement("div");
        elem.setAttribute("id", "gt-app");
        document.body.appendChild(elem);
        ReactDOM.render(
            <AppElem
                showNav={true}
                editorHeight={200}
                currentModulePath="/welcome.md"
            />,
            elem);
    }));
