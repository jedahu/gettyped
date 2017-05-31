import * as React from "react";
import * as part from "../part";
import {Val} from "../adt";

import * as Selectivity from "selectivity/react";
import "selectivity/styles/selectivity-react.css";

export class PathChange extends Val<{
    readonly path : string;
},
"343badc0-ef34-479e-96e8-e3ec31c2732a"> {}

export class ToggleDisplay extends Val<{
    readonly visible : boolean;
},
"1d7513cf-b2e2-4947-ad6e-d2aeaa422ab4"> {}

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
                            onChange={signal.emit((x : {value : string}) => new PathChange({path: x.value}))}
                        />
                    </div>
                    <div className="pt-navbar-group pt-align-right">{
                        props.display.visible
                        ? <button
                            className="pt-button pt-minimal pt-icon-double-chevron-down"
                            onClick={signal.emit(_ => new ToggleDisplay({visible: false}))}
                        ></button>
                        : <button
                            className="pt-button pt-minimal pt-icon-double-chevron-up"
                            onClick={signal.emit(_ => new ToggleDisplay({visible: true}))}
                        ></button>
                    }</div>
                </nav>;
            }
        };
    });
