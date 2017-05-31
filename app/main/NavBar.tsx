import * as React from "react";
import * as part from "./part";
import {Val} from "./adt";

export class HomeClick extends Val<{
},
"27c0bf92-6d40-45d7-a96c-0386509fea1d"> {}

export class MenuClick extends Val<{
},
"27c0bf92-6d40-45d7-a96c-0386509fea1d"> {}

type In = {
    title : string,
    home? : string,
};

export type Out = HomeClick | MenuClick;

export const mk = part.mk<In, {}, Out>(
    ({signal}) => ({
        render: ({props}) =>
            <nav className="pt-navbar">
                <div className="pt-navbar-group pt-align-left">
                    <button
                        className="pt-button pt-minimal pt-icon-menu"
                        onClick={signal.emit(_ => new MenuClick({}))}
                    ></button>
                    <span className="pt-navbar-divider"></span>
                    <div className="pt-navbar-heading">{props.title}</div>
                </div>
                <div className="pt-navbar-group pt-align-right">
                    <button
                        className="pt-button pt-minimal pt-icon-home"
                        onClick={signal.emit(_ => new HomeClick({}))}
                    >{props.home || "Home"}</button>
                </div>
        </nav>
    }));
