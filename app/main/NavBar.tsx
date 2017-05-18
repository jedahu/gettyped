import * as React from "react";

type Props = {
    title : string,
    home? : string,
    onMenuClick : () => void,
    onHomeClick : () => void
};

export default (props : Props) =>
    <nav className="pt-navbar">
        <div className="pt-navbar-group pt-align-left">
            <button
                className="pt-button pt-minimal pt-icon-menu"
                onClick={props.onMenuClick}
            ></button>
            <span className="pt-navbar-divider"></span>
            <div className="pt-navbar-heading">{props.title}</div>
        </div>
        <div className="pt-navbar-group pt-align-right">
            <button
                className="pt-button pt-minimal pt-icon-home"
                onClick={props.onHomeClick}
            >{props.home || "Home"}</button>
        </div>
    </nav>;
