import * as React from "react";
import * as part from "../part";
import {Val} from "../adt";

export class ViewStatus extends Val<{
    readonly path : string;
    readonly state : ViewState;
},
"177a8882-5c0d-4302-a431-f155d656466b"> {}

export class ValidationStatus extends Val<{
    readonly valid : boolean;
},
"cf685aa9-4912-4c3f-a47d-f689e6928902"> {}

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
        // const autoResize = () => editor.layout();
        return {
            render: ({props}) => {
                const style = {
                    height: props.height,
                    width: "100%"
                };
                return <div style={style} ref={a => {domPeer = a;}}></div>;
            },
            update:
                part.Signal.
                    handle<part.Begin<In, State>>(
                        part.Begin,
                        ({val : {props}}) => {
                            editor = monaco.editor.create(domPeer, {
                                model: props.modelData.model,
                                lineNumbers: "off"
                            });
                            editor.onDidChangeModelDecorations(
                                signal.emit(_ => new ValidationStatus({
                                    valid: !editor.getModel().getAllDecorations().find(
                                        d => d.isForValidation)
                                })));
                            global.setTimeout(() => {
                                global.requestAnimationFrame(() => {
                                    editor.layout({height: props.height, width: props.width});
                                });
                            }, 1);
                        }).
                    handle<part.Change<In, State>>(
                        part.Change,
                        ({val: {prevProps, props}}) => {
                            if (editor) {
                                if (prevProps.modelData.path !== props.modelData.path) {
                                    signal.run(new ViewStatus({
                                        path: prevProps.modelData.path,
                                        state: editor.saveViewState()
                                    }));
                                    editor.setModel(props.modelData.model);
                                    if (props.modelData.viewState) {
                                        editor.restoreViewState(props.modelData.viewState);
                                    }
                                }
                                editor.layout({height: props.height, width: props.width});
                                // editor.focus();
                            }
                        }).
                    handle<part.End<In, State>>(
                        part.End,
                        () => {
                            // global.removeEventListener("resize", autoResize)
                        })
        };
    });
