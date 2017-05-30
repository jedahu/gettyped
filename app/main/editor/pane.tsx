import * as ts from "typescript";
import * as React from "react";
import * as Buffer from "./buffer";
import * as Bar from "./bar";
import * as welcome from "./welcome.md";
import {makeApi} from "ts-rpc/main";
import * as WAPI from "../../worker/api";
import * as worker from "worker-loader?name=worker.[hash].js!../../worker";
import tsconfig from "../tsconfig";
import * as part from "../part";
import * as Future from "fluture";
import {fetchText} from "../fetch";

const wapi = makeApi<WAPI.Arg, WAPI.Ret>(new worker(), {
    dependencies: "dependencies",
    initialize: "initialize",
    setModule: "setModule",
    evalModule: "evalModule"
});

type Model = monaco.editor.IModel;
type ViewState = monaco.editor.IEditorViewState;

type In = {
    id : string;
    height : number;
    width : number;
    currentPath? : string;
    display : {visible : boolean};
};

export {PathChange, ToggleDisplay} from "./bar";
export type Out = Bar.Out;

type State = {
    currentPath : string;
    models : {[path : string] : ModelData};
};

type GetCreateModel_ = {
    path : string;
    data : ModelData;
};

export class GetCreateModel {
    [Symbol.species] : "f963eb3e-9eba-47df-b970-c77ff5413b08";
    readonly path : string;
    readonly data : ModelData;

    constructor(args : GetCreateModel_) {
        Object.assign(this, args);
    }

    static mk(args : GetCreateModel_) {
        return new GetCreateModel(args);
    }

    static is<Z>(x : GetCreateModel | Z) : x is GetCreateModel {
        return x instanceof GetCreateModel;
    }
}

export const mk = part.mk<In, State, Out>(
    ({updateState, signal}) => {
        const pathToLoad = (props : In) =>
            props.currentPath || placeholderPath;

        const currentPath = (state : State) =>
            state.currentPath;

        const currentModelData = (state : State) =>
            state.models[currentPath(state)];

        const internal =
            part.Signal.
                handle<Buffer.ViewStatus>(
                    Buffer.ViewStatus,
                    ({path, state: vs}) =>
                        updateState((state : State, props : In) => {
                            const md = state.models[path];
                            md.viewState = vs;
                            return state;
                        })).
                handle<Buffer.ValidationStatus>(
                    Buffer.ValidationStatus,
                    ({valid}) =>
                        updateState((state : State, props : In) => {
                            const md = currentModelData(state);
                            md.status = valid ? md.status & ~ModelStatus.Error : md.status | ModelStatus.Error;
                            return state;
                        })).
                handle<GetCreateModel>(
                    GetCreateModel,
                    ({path, data}) =>
                        updateState((state : State) => ({
                            currentPath: path,
                            models: {
                                ...state.models,
                                [path]: data
                            }
                        })));

        const BarElem = Bar.mk(signal);
        const BufferElem = Buffer.mk(internal);

        return {
            initialState: props => ({
                currentPath: placeholderPath,
                models: {
                    [placeholderPath]: {
                        model: placeholderModel,
                        status: ModelStatus.Open
                    }
                }
            }),
            render: ({props, state}) => {
                const path = currentPath(state);
                const {model, viewState} = currentModelData(state);
                const ks = Object.keys(state.models);
                ks.sort();
                const paths = ks.map(k => {
                    const status = state.models[k].status;
                    return {
                        path: k,
                        error: (status & ModelStatus.Error) > 0,
                        open : (status & ModelStatus.Open) > 0
                    };
                });
                return <div>
                    <BarElem
                        width={props.width}
                        display={props.display}
                        currentPath={currentPath(state)}
                        paths={paths}
                    />
                    <div
                        style={{ display: props.display.visible ? "" : "none" }}>
                        <BufferElem
                            modelData={{path, model, viewState}}
                            height={props.height}
                            width={props.width}
                        />;
                    </div>
                </div>
            },
            update:
                part.Signal.
                     ignore<part.Begin<In, State>>(part.Begin).
                     handle<part.Change<In, State>>(
                         part.Change,
                         ({prevProps, props, state}) => {
                             const prevPath = prevProps.currentPath;
                             const ptl = pathToLoad(props);
                             if (ptl != prevPath) {
                                 getCreateModelTransitively(ptl, state).value(
                                     x =>
                                         internal.run(GetCreateModel.mk({
                                             path: ptl,
                                             data: x
                                         })))
                             }
                         }).
                     ignore<part.End<In, State>>(part.End)
        }
});

const getCreateModel = (path : string, state : State,): Future<string, ModelData> => {
    const uri = monaco.Uri.parse(path);
    const x = state.models[path] || monaco.editor.getModel(uri);
    return x
         ? Future.of(x)
         : fetchText(path).
                map(({text: s}) => ({
                    model: monaco.editor.createModel(s, undefined, uri),
                    status: ModelStatus.Open
                })).
                mapRej(({reason}) => reason);
};

const getCreateModelTransitively = (path : string, state : State) : Future<string, ModelData> => {
    const deps = (m : Model) : Future<string, Array<string>> =>
        Future.node(
            k =>
                wapi.dependencies({
                    path,
                    code: m.getValue(undefined, false),
                    target: tsconfig.target || ts.ScriptTarget.ES5,
                    kind: ts.ScriptKind.TS
                },
                                  ds => {
                                      k(null, ds);
                                  }
                ));
    const go = (path : string) : Future<string, ModelData> =>
        getCreateModel(path, state).chain(
            md => deps(md.model).chain(
                ds => Future.parallel(Infinity, ds.map(s => `/${s}.ts`).map(go)).map(
                    _ => md)));
    return go(path);
};

enum ModelStatus {
    None = 0,
    Open = 1,
    Error = 2
}

type ModelData = {
    model : Model;
    viewState? : ViewState;
    status : ModelStatus
};

const placeholderPath = "/welcome.md";
const placeholderUri = monaco.Uri.parse(placeholderPath);

const placeholderModel =
    monaco.editor.createModel(
        welcome,
        "markdown",
        placeholderUri);
