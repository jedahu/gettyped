import * as React from "react";
import * as ReactDOM from "react-dom";
import * as injectTapEventPlugin from "react-tap-event-plugin";

import "normalize.css/normalize.css";
import "@blueprintjs/core/dist/blueprint.css";
import {FocusStyleManager} from "@blueprintjs/core";

import "../../scss/typeset.scss";
import "../../scss/main.scss";

import {App} from "./index";

type Global = typeof window & {require : any};

const global : Global = window as any;

injectTapEventPlugin();
FocusStyleManager.onlyShowFocusOnTabs();

global.addEventListener(
    "load",
    () => global.require(["vs/editor/editor.main"], () => {
        const elem = document.createElement("div");
        elem.setAttribute("id", "gt-app");
        document.body.appendChild(elem);
        ReactDOM.render(
            <App
                monaco={monaco}
                showNav={true}
                editorHeight={200}
                currentPath="/demo/welcome.ts"
            />,
            elem);
    }));
