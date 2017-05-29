import * as React from "react";
import * as part from "../part";
import * as adt from "../adt";

type ViewStatus_ = {
    path : string;
    state : ViewState;
};

export class ViewStatus {
    [Symbol.species] : "a0c14342-839e-4bcc-8554-ade4695108b6";
    readonly path : string;
    readonly state : ViewState;

    constructor(args : ViewStatus_) {
        Object.assign(this, args);
    }

    static mk(args : ViewStatus_) {
        return new ViewStatus(args);
    }

    static is<Z>(x : ViewStatus | Z) : x is ViewStatus {
        return x instanceof ViewStatus;
    }
}

type ValidationStatus_ = {
    valid : boolean;
};

export class ValidationStatus {
    [Symbol.species] : "cf685aa9-4912-4c3f-a47d-f689e6928902";
    readonly valid : boolean;

    constructor(args : ValidationStatus_) {
        this.valid = args.valid;
    }

    static mk(args : ValidationStatus_) {
        return new ValidationStatus(args);
    }

    static is<Z>(x : ValidationStatus | Z) : x is ValidationStatus {
        return x instanceof ValidationStatus;
    }
}

const global = window;

type Model = monaco.editor.IModel;
type ViewState = monaco.editor.IEditorViewState;
type MEditor = monaco.editor.IStandaloneCodeEditor;

type In = {
    height : number;
    width : number;
    modelData : {
        path : string;
        model : Model;
        viewState? : ViewState;
    };
};

export type Out = ViewStatus | ValidationStatus;

type State = {
};

export const mk = part.mk<In, State, Out>(
    ({signal}) => {
        let editor : MEditor;
        let domPeer : HTMLDivElement;
        const autoResize = () => editor.layout();
        return {
            render: ({props}) => {
                const style = {
                    height: props.height,
                    width: "100%"
                };
                return <div style={style} ref={a => {domPeer = a;}}></div>;
            },
            update: ({event}) => {
                if (event instanceof part.Begin) {
                    const {props} = event;
                    editor = monaco.editor.create(domPeer, {
                        model: props.modelData.model,
                        lineNumbers: "off"
                    });
                    editor.onDidChangeModelDecorations(
                        signal.emit(_ => ValidationStatus.mk({
                            valid: !editor.getModel().getAllDecorations().find(
                                d => d.isForValidation)
                        })));
                    setTimeout(() => {
                        global.requestAnimationFrame(() => {
                            editor.layout({height: props.height, width: props.width});
                            global.addEventListener("resize", autoResize);
                        });
                    }, 1);
                    return;
                }
                if (event instanceof part.Change) {
                    const {prevProps, props} = event;
                    if (editor) {
                        if (prevProps.modelData.path !== props.modelData.path) {
                            signal.run(ViewStatus.mk({
                                path: prevProps.modelData.path,
                                state: editor.saveViewState()
                            }));
                            editor.setModel(props.modelData.model);
                            if (props.modelData.viewState) {
                                editor.restoreViewState(props.modelData.viewState);
                            }
                        }
                        editor.layout({height: props.height, width: props.width});
                        editor.focus();
                    }
                    return;
                }
                if (event instanceof part.End) {
                    global.removeEventListener("resize", autoResize);
                    return;
                }
                return adt.done(event);
            }
        };
    });
