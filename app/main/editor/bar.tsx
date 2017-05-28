import * as React from "react";
import * as part from "../part";
import * as adt from "../adt";

import * as Selectivity from "selectivity/react";
import "selectivity/styles/selectivity-react.css";

declare module "../adt" {
    interface Cases<A, B, C> {
        "PathChange-d1a1880b-93a8-40c9-bf69-eac1938d4be5" : {
            path : string;
        };

        "ToggleDisplay-d67e80dc-1927-4aa9-9440-8f5cfd3ef42c" : {
            visible : boolean;
        };
    }
}

export const PathChange = "PathChange-d1a1880b-93a8-40c9-bf69-eac1938d4be5";
export const ToggleDisplay = "ToggleDisplay-d67e80dc-1927-4aa9-9440-8f5cfd3ef42c";

type In = {
    paths : Array<{path : string, error : boolean, open : boolean}>;
    currentPath : string;
    display : {visible : boolean};
    width : number;
};

export type Out =
    adt.Case<typeof PathChange> |
    adt.Case<typeof ToggleDisplay>;

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
                            onChange={part.emit(signal, (x : any) => adt.mk(PathChange, x.value))}
                        />
                    </div>
                    <div className="pt-navbar-group pt-align-right">{
                        props.display.visible
                        ? <button
                            className="pt-button pt-minimal pt-icon-double-chevron-down"
                            onClick={part.emit(signal, _ => adt.mk(ToggleDisplay, {visible: false}))}
                        ></button>
                        : <button
                            className="pt-button pt-minimal pt-icon-double-chevron-up"
                            onClick={part.emit(signal, _ => adt.mk(ToggleDisplay, {visible: true}))}
                        ></button>
                    }</div>
                </nav>;
            }
        };
    });
