import * as React from "react";

const global = window;

type Model = monaco.editor.IModel;
type ViewState = monaco.editor.IEditorViewState;
type MEditor = monaco.editor.IStandaloneCodeEditor;

type Props = {
    onValidStateChange? : (isValid : boolean) => any;
    height : number;
    width : number;
    model : Model;
    viewState? : ViewState;
    getViewState? : (f : () => ViewState) => void;
    setModel : (f : (m : Model) => void) => void;
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
        this.editor = monaco.editor.create(this.domPeer, {
            model: this.props.model,
            lineNumbers: "off"
        });
        if (this.props.getViewState) {
            this.props.getViewState(() => this.editor.saveViewState());
        }
        if (this.props.setModel) {
            this.props.setModel(m => this.editor.setModel(m));
        }

        const onValidStateChange = this.props.onValidStateChange;
        if (onValidStateChange) {
            this.editor.onDidChangeModelDecorations(
                _ => onValidStateChange(
                    !this.editor.getModel().getAllDecorations().find(
                        d => d.isForValidation)));
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
