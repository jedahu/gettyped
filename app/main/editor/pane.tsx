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
import * as adt from "../adt";

const global = window;

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

export const PathChange = Bar.PathChange;
export const ToggleDisplay = Bar.ToggleDisplay;
export type Out = Bar.Out;

type State = {
    currentPath : string;
    models : {[path : string] : ModelData};
};

declare module "../adt" {
    interface Cases<A, B, C> {
        "GetCreateModel-41b85fe0-26d5-4a4f-b3eb-a7300fbe2599" : {
            path : string;
            data : ModelData;
        };
    }
}

export const GetCreateModel = "GetCreateModel-41b85fe0-26d5-4a4f-b3eb-a7300fbe2599";

type Internal =
    Buffer.Out |
    adt.Case<typeof GetCreateModel>;

export const mk = part.mk<In, State, Out>(
    ({updateState, signal}) => {
        const pathToLoad = (props : In) =>
            props.currentPath || placeholderPath;

        const currentPath = (state : State) =>
            state.currentPath;

        const currentModelData = (state : State) =>
            state.models[currentPath(state)];

        const handler = (event : Internal) => {
            if (event._tag === Buffer.ViewStatus) {
                const {path, state: vs} = event._val;
                return updateState((state : State, props : In) => {
                    const md = state.models[path];
                    md.viewState = vs;
                    return state;
                });
            }
            if (event._tag === Buffer.ValidationStatus) {
                const isValid = event._val;
                return updateState((state : State, props : In) => {
                    const md = currentModelData(state);
                    md.status = isValid ? md.status & ~ModelStatus.Error : md.status | ModelStatus.Error;
                    return state;
                });
            };
            if (event._tag === GetCreateModel) {
                const {path, data} = event._val;
                return updateState((state : State) => ({
                    currentPath: path,
                    models: {
                        ...state.models,
                        [path]: data
                    }
                }));
            }
            return adt.assertExhausted(event);
        };

        const BarElem = Bar.mk(signal);
        const BufferElem = Buffer.mk(handler);

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
            update: ({event}) => {
                if (event._tag === part.Begin) {
                    return Future.of((x : State) => x);
                }
                if (event._tag === part.Change) {
                    const {prevProps, props, state} = event._val;
                    const prevPath = prevProps.currentPath;
                    const ptl = pathToLoad(props);
                    if (ptl != prevPath) {
                        return getCreateModelTransitively(ptl, state).value(
                            x =>
                                handler(adt.mk(GetCreateModel, {
                                    path: ptl,
                                    data: x
                                })))
                    }
                    return;
                }
                if (event._tag === part.End) {
                    return Future.of((x : State) => x);
                }
                return adt.assertExhausted(event);
            }
        }
});

const getCreateModel = (path : string, state : State,): Future<Error, ModelData> => {
    const uri = monaco.Uri.parse(path);
    const x = state.models[path] || monaco.editor.getModel(uri);
    return x
         ? Future.of(x)
         : Future.tryP(() => global.fetch(path).then(r => r.text())).
                  map((s : string) => ({
                      model: monaco.editor.createModel(s, undefined, uri),
                      status: ModelStatus.Open
                  }));
};

const getCreateModelTransitively = (path : string, state : State) : Future<Error, ModelData> => {
    const deps = (m : Model) : Future<Error, Array<string>> =>
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
    const go = (path : string) : Future<Error, ModelData> =>
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
