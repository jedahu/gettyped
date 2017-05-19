import * as React from "react";

const global = window;

type Model = monaco.editor.IModel;
type ViewState = monaco.editor.IEditorViewState;
type MEditor = monaco.editor.IStandaloneCodeEditor;

type Props = {
    onChange? : (newValue: string) => any;
    height : number;
    width : number;
    model : Model;
    viewState? : ViewState;
    getViewState? : (f : () => ViewState) => void;
};

export default class EditorBuffer extends React.Component<Props, {}> {
    domPeer : HTMLDivElement;
    editor: MEditor;

    render(): JSX.Element {
        const style = {
            height: this.props.height,
            width: "100%"
        };
        return <div
                   style={style}
                   ref={elem => { this.domPeer = elem; }}>
        </div>;
    }

    resizeListener = () => {
        this.editor.layout();
    }

    resize(height : number) {
        this.editor.layout({
            height,
            width: this.props.width
        });
    }

    componentDidMount() {
        // Monaco requires the AMD module loader to be present on the page. It is not yet
        // compatible with ES6 imports. Once that happens, we can get rid of this.
        // See https://github.com/Microsoft/monaco-editor/issues/18

        this.editor = monaco.editor.create(this.domPeer, {
            model: this.props.model,
            lineNumbers: "off"
        });
        if (this.props.getViewState) {
            this.props.getViewState(() => this.editor.saveViewState());
        }

        const onChange = this.props.onChange;
        if (onChange) {
            this.editor.onDidChangeModelContent(event => {
                onChange(this.editor.getValue());
            });
        }

        setTimeout(() => {
        global.requestAnimationFrame(() => {
            this.resize(this.props.height);
            global.addEventListener("resize", this.resizeListener);
        });
        }, 2);
    }

    componentWillUnmount() {
        global.removeEventListener("resize", this.resizeListener);
    }

    componentDidUpdate(prevProps: Props) {
        if (this.editor) {
            this.editor.setModel(this.props.model);
            if (this.props.viewState) {
                this.editor.restoreViewState(this.props.viewState);
            }
            this.resize(this.props.height);
            this.editor.focus();
        }
    }
}
