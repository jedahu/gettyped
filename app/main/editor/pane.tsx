import * as React from "react";
import Buffer from "./buffer";
import * as welcome from "./welcome.md";

const global = window;

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
        const x = this.state.models[path];
        return x
            ? Promise.resolve(x)
            : global.fetch(path).
                then(r => r.text()).
                then(s => [
                    monaco.editor.createModel(s, undefined, uri),
                    undefined
                ] as [Model, undefined | ViewState]);
    }

    componentDidUpdate(prevProps : Props) {
        const prevPath = prevProps.currentPath;
        const currentPath = this.props.currentPath || placeholderPath;
        if (currentPath != prevPath) {
            this.getCreateModel(currentPath || placeholderPath).
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
        />;
    }
}
