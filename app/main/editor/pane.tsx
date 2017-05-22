import * as ts from "typescript";
import * as React from "react";
import Buffer from "./buffer";
import Bar from "./bar";
import * as welcome from "./welcome.md";
import {makeApi} from "ts-rpc/main";
import * as WAPI from "../../worker/api";
import * as worker from "worker-loader?name=worker.[hash].js!../../worker";
import tsconfig from "../tsconfig";

const global = window;

const wapi = makeApi<WAPI.Arg, WAPI.Ret>(new worker(), {
    dependencies: "dependencies",
    initialize: "initialize",
    setModule: "setModule",
    evalModule: "evalModule"
});

type Model = monaco.editor.IModel;
type ViewState = monaco.editor.IEditorViewState;

type Props = {
    id : string;
    height : number;
    width : number;
    currentPath? : string;
    display : "up"|"down";
    onPathChange : (path : string) => void;
    onToggleDisplay : (state : "up" | "down") => void;
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

type State = {
    currentPath : string;
    models : {[path : string] : ModelData};
};

const placeholderPath = "/welcome.md";
const placeholderUri = monaco.Uri.parse(placeholderPath);

const placeholderModel =
    monaco.editor.createModel(
        welcome,
        "markdown",
        placeholderUri);

export default class EditorPane extends React.Component<Props, State> {
    placeholder : Model;
    getViewState : () => ViewState;
    setModel : (m : Model) => void;

    constructor(props : Props) {
        super(props);
        this.state = {
            currentPath: placeholderPath,
            models: {
                [placeholderPath]: {
                    model: placeholderModel,
                    status: ModelStatus.None
                }
            }
        };
    }

    pathToLoad() : string {
        return this.props.currentPath || placeholderPath;
    }

    currentPath() : string {
        return this.state.currentPath;
    }

    currentModelData() : ModelData {
        return this.state.models[this.currentPath()];
    }

    currentModule() : Model {
        return this.currentModelData().model;
    }

    getCreateModel(path: string): Promise<ModelData> {
        const uri = monaco.Uri.parse(path);
        const x = this.state.models[path] || monaco.editor.getModel(uri);
        return x
            ? Promise.resolve(x)
            : global.fetch(path).
                then(r => r.text()).
                then(s => ({
                    model: monaco.editor.createModel(s, undefined, uri),
                    status: ModelStatus.Open
                }));
    }

    getCreateModelTransitively(path : string) : Promise<ModelData> {
        const deps = (m : Model) : Promise<Array<string>> =>
            new Promise(
                resolve =>
                    wapi.dependencies({
                        path,
                        code: m.getValue(undefined, false),
                        target: tsconfig.target || ts.ScriptTarget.ES5,
                        kind: ts.ScriptKind.TS
                    },
                    ds => {
                        resolve(ds);
                    }
                    ));
        const go = async (path : string) : Promise<ModelData> => {
            const md = await this.getCreateModel(path);
            const ds = await deps(md.model);
            await Promise.all(ds.map(s => `/${s}.ts`).map(go));
            this.setModel(md.model);
            return md;
        };
        return go(path);
    }

    onValidStateChange = (isValid : boolean) => {
        const s = this.currentModelData().status;
        this.currentModelData().status = isValid ? s & ~ModelStatus.Error : s | ModelStatus.Error;
        this.setState(this.state);
    }

    componentDidUpdate(prevProps : Props) {
        const prevPath = prevProps.currentPath;
        const ptl = this.pathToLoad();
        if (ptl != prevPath) {
            this.getCreateModelTransitively(ptl).
            then(x => {
                if (prevPath && this.state.models[prevPath]) {
                    const {model: prevModel, status: prevStatus} = this.state.models[prevPath];
                    const prevVs = this.getViewState();
                    this.setState({
                        currentPath: ptl,
                        models: {
                            ...this.state.models,
                            [prevPath]: {
                                model: prevModel,
                                viewState: prevVs,
                                status: prevStatus
                            },
                            [ptl]: x
                        }
                    });
                }
                this.setState({
                    currentPath: ptl,
                    models: {
                        ...this.state.models,
                        [ptl]: x
                    }
                });
            });
        }
    }

    render() {
        const {model: m, viewState: vs} = this.currentModelData();
        const ks = Object.keys(this.state.models);
        ks.sort();
        const paths = ks.map(k => {
            const status = this.state.models[k].status;
            return {
                path: k,
                error: (status & ModelStatus.Error) > 0,
                open : (status & ModelStatus.Open) > 0
            };
        });
        return <div>
            <Bar
                width={this.props.width}
                display={this.props.display}
                currentPath={this.currentPath()}
                paths={paths}
                onPathChange={this.props.onPathChange}
                onToggleClick={this.props.onToggleDisplay}
            />
            <div
                style={{ display: this.props.display === "up" ? "" : "none" }}>
                <Buffer
                    model={m}
                    viewState={vs}
                    height={this.props.height}
                    width={this.props.width}
                    getViewState={f => { this.getViewState = f; }}
                    setModel={f => { this.setModel = f; }}
                    onValidStateChange={this.onValidStateChange}
                />;
            </div>
        </div>
    }
}
