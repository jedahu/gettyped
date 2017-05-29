import * as React from "react";
import * as part from "./part";
import * as adt from "./adt";

export class HomeClick {
    [Symbol.species] : "27c0bf92-6d40-45d7-a96c-0386509fea1d";

    constructor() {}

    static mk() {
        return new HomeClick();
    }

    static is<Z>(x : HomeClick | Z) : x is HomeClick {
        return x instanceof HomeClick;
    }
}

export class MenuClick {
    [Symbol.species] : "27c0bf92-6d40-45d7-a96c-0386509fea1d";

    constructor() {}

    static mk() {
        return new MenuClick();
    }

    static is<Z>(x : MenuClick | Z) : x is MenuClick {
        return x instanceof MenuClick;
    }
}

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
                        onClick={signal.emit(_ => MenuClick.mk())}
                    ></button>
                    <span className="pt-navbar-divider"></span>
                    <div className="pt-navbar-heading">{props.title}</div>
                </div>
                <div className="pt-navbar-group pt-align-right">
                    <button
                        className="pt-button pt-minimal pt-icon-home"
                        onClick={signal.emit(_ => HomeClick.mk())}
                    >{props.home || "Home"}</button>
                </div>
        </nav>
    }));
