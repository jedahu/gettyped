import * as ts from "typescript";
import * as React from "react";
import Buffer from "./buffer";
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
};

type State = {
    currentPath : string;
    models : {[path : string] : [Model, undefined | ViewState]}
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
                [placeholderPath]: [placeholderModel, undefined]
            }
        };
    }

    getCreateModel(path: string): Promise<[Model, undefined | ViewState]> {
        const uri = monaco.Uri.parse(path);
        const x = this.state.models[path] || monaco.editor.getModel(uri);
        return x
            ? Promise.resolve(x)
            : global.fetch(path).
                then(r => r.text()).
                then(s => [
                    monaco.editor.createModel(s, undefined, uri),
                    undefined
                ] as [Model, undefined | ViewState]);
    }

    getCreateModelTransitively(path : string) : Promise<[Model, undefined | ViewState]> {
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
        const go = async (path : string) : Promise<[Model, undefined | ViewState]> => {
            const [m, sv] = await this.getCreateModel(path);
            const ds = await deps(m);
            await Promise.all(ds.map(s => `/${s}.ts`).map(go));
            this.setModel(m);
            return [m, sv];
        };
        return go(path);
    }

    componentDidUpdate(prevProps : Props) {
        const prevPath = prevProps.currentPath;
        const currentPath = this.props.currentPath || placeholderPath;
        if (currentPath != prevPath) {
            this.getCreateModelTransitively(currentPath || placeholderPath).
            then(x => {
                if (prevPath && this.state.models[prevPath]) {
                    const [prevModel,] = this.state.models[prevPath];
                    const prevVs = this.getViewState();
                    this.setState({
                        currentPath,
                        models: {
                            ...this.state.models,
                            [prevPath]: [prevModel, prevVs],
                            [currentPath]: x
                        }
                    });
                }
                this.setState({
                    currentPath,
                    models: {
                        ...this.state.models,
                        [currentPath]: x
                    }
                });
            });
        }
    }

    render() {
        const [m, vs] = this.state.models[this.state.currentPath];
        return <Buffer
            model={m}
            viewState={vs}
            height={this.props.height}
            width={this.props.width}
            getViewState={f => { this.getViewState = f; }}
            setModel={f => { this.setModel = f; }}
        />;
    }
}
