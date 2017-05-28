import * as React from "react";
import * as part from "../part";
import * as adt from "../adt";

declare module "../adt" {
    interface Cases<A, B, C> {
        "ViewStatus-11ccfdff-711d-4500-b28a-779d62c3cd6c" : {
            path : string;
            state : ViewState;
        };

        "ValidationStatus-b7c5b119-4d9e-496b-bed4-2b7bfc19348e" : {
            valid : boolean;
        };
    }
}

export const ViewStatus = "ViewStatus-11ccfdff-711d-4500-b28a-779d62c3cd6c";
export const ValidationStatus = "ValidationStatus-b7c5b119-4d9e-496b-bed4-2b7bfc19348e";

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

export type Out =
    adt.Case<typeof ViewStatus> |
    adt.Case<typeof ValidationStatus>;

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
                if (event._tag === part.Begin) {
                    const {props} = event._val;
                    editor = monaco.editor.create(domPeer, {
                        model: props.modelData.model,
                        lineNumbers: "off"
                    });
                    editor.onDidChangeModelDecorations(
                        part.emit(signal, _ => adt.mk(ValidationStatus, {
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
                if (event._tag === part.Change) {
                    const {prevProps, props} = event._val;
                    if (editor) {
                        if (prevProps.modelData.path !== props.modelData.path) {
                            signal(adt.mk(ViewStatus, {
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
                if (event._tag === part.End) {
                    global.removeEventListener("resize", autoResize);
                    return;
                }
                return adt.assertExhausted(event);
            }
        };
    });
