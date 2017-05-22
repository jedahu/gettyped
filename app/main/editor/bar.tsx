import * as React from "react";
import * as Selectivity from "selectivity/react";
import "selectivity/styles/selectivity-react.css";

type Props = {
    paths : Array<{path : string, error : boolean, open : boolean}>;
    currentPath : string;
    display : "up"|"down";
    onPathChange : (path : string) => void;
    onToggleClick : (state : "up"|"down") => void;
    width : number;
};

export default (props : Props) => {
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
                onChange={(x : any) => props.onPathChange(x.value)}
            />
        </div>
        <div className="pt-navbar-group pt-align-right">{
            props.display === "up"
            ? <button
                className="pt-button pt-minimal pt-icon-double-chevron-down"
                onClick={() => props.onToggleClick("down")}
            ></button>
            : <button
                className="pt-button pt-minimal pt-icon-double-chevron-up"
                onClick={() => props.onToggleClick("up")}
            ></button>
        }</div>
    </nav>;
}

