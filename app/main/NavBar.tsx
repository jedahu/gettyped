import * as React from "react";
import * as part from "./part";
import * as adt from "./adt";

declare module "./adt" {
    interface Cases<A, B, C> {
        "MenuClick-76a038a1-9543-4a00-b8c8-a93e9a6df666" : {};
        "HomeClick-96e75c52-2b87-45b9-bfb5-672810af8835" : {};
    }
}

export const MenuClick = "MenuClick-76a038a1-9543-4a00-b8c8-a93e9a6df666";
export const HomeClick = "HomeClick-96e75c52-2b87-45b9-bfb5-672810af8835";

type In = {
    title : string,
    home? : string,
};

export type Out =
    adt.Case<typeof MenuClick> |
    adt.Case<typeof HomeClick>;

export const mk = part.mk<In, {}, Out>(
    ({signal}) => ({
        render: ({props}) =>
            <nav className="pt-navbar">
                <div className="pt-navbar-group pt-align-left">
                    <button
                        className="pt-button pt-minimal pt-icon-menu"
                        onClick={part.emit(signal, _ => adt.mk(MenuClick, {}))}
                    ></button>
                    <span className="pt-navbar-divider"></span>
                    <div className="pt-navbar-heading">{props.title}</div>
                </div>
                <div className="pt-navbar-group pt-align-right">
                    <button
                        className="pt-button pt-minimal pt-icon-home"
                        onClick={part.emit(signal, _ => adt.mk(HomeClick, {}))}
                    >{props.home || "Home"}</button>
                </div>
        </nav>
    }));
