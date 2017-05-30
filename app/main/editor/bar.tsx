import * as React from "react";
import * as part from "../part";

import * as Selectivity from "selectivity/react";
import "selectivity/styles/selectivity-react.css";

type PathChange_ = {
    path : string;
};

export class PathChange {
    [Symbol.species] : "343badc0-ef34-479e-96e8-e3ec31c2732a";
    readonly path : string;

    constructor(args : PathChange_) {
        this.path = args.path;
    }

    static mk(args : PathChange_) {
        return new PathChange(args);
    }

    static is<Z>(x : PathChange | Z) : x is PathChange {
        return x instanceof PathChange;
    }
}

type ToggleDisplay_ = {
    visible : boolean;
};

export class ToggleDisplay {
    [Symbol.species] : "1d7513cf-b2e2-4947-ad6e-d2aeaa422ab4";
    readonly visible : boolean;

    constructor(args : ToggleDisplay_) {
        this.visible = args.visible;
    }

    static mk(args : ToggleDisplay_) {
        return new ToggleDisplay(args);
    }

    static is<Z>(x : ToggleDisplay | Z) : x is ToggleDisplay {
        return x instanceof ToggleDisplay;
    }
}

type In = {
    paths : Array<{path : string, error : boolean, open : boolean}>;
    currentPath : string;
    display : {visible : boolean};
    width : number;
};

export type Out = PathChange | ToggleDisplay;

export const mk = part.mk<In, {}, Out>(
    ({signal}) => {
        return {
            render: ({props}) => {
                const merrs = props.paths.filter(x => x.error).map(x => x.path);
                const mopen = props.paths.filter(x => !x.error && x.open).map(x => x.path);
                const mrest = props.paths.filter(x => !x.error && !x.open).map(x => x.path);
                const oerrs = merrs.length > 0 ? [{text: "Errors", children: merrs}] : [];
                const oopen = mopen.length > 0 ? [{text: "Open", children: mopen}] : [];
                const orest = mrest.length > 0 ? [{text: "Closed", children: mrest}] : [];
                return <nav
                        className="pt-navbar"
                        style={{width: props.width}}>
                    <div className="pt-navbar-group pt-align-left">
                        <Selectivity.React
                            data={{
                                id: props.currentPath,
                                text: props.currentPath
                            }}
                            items={[].concat.apply([], [oerrs, oopen, orest])}
                            onChange={signal.emit((x : {value : string}) => PathChange.mk({path: x.value}))}
                        />
                    </div>
                    <div className="pt-navbar-group pt-align-right">{
                        props.display.visible
                        ? <button
                            className="pt-button pt-minimal pt-icon-double-chevron-down"
                            onClick={signal.emit(_ => ToggleDisplay.mk({visible: false}))}
                        ></button>
                        : <button
                            className="pt-button pt-minimal pt-icon-double-chevron-up"
                            onClick={signal.emit(_ => ToggleDisplay.mk({visible: true}))}
                        ></button>
                    }</div>
                </nav>;
            }
        };
    });
