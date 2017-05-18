/// <reference path="../../node_modules/monaco-editor/monaco.d.ts" />
import * as React from "react";
// import MonacoEditor from "react-monaco-editor";

type Global = Window & {require : any};

const global : Global = window as any;

interface Props {
    value: string;
    language: string;
    onChange: (newValue: string) => any;
    height: number;
}

export default class Editor extends React.Component<Props, {}> {
    domPeer : HTMLDivElement;
    editor: monaco.editor.IStandaloneCodeEditor;

    render(): JSX.Element {
        return <div
                   id="gt-code-editor"
                   style={{height: this.props.height, width: "100%"}}
                   ref={elem => { this.domPeer = elem; }}>
        </div>;
    }

    resizeListener = () => {
        this.editor.layout();
    }

    resize(height : number) {
        this.editor.layout({
            height,
            width: this.editor.getLayoutInfo().width
        });
    }

    componentDidMount() {
        // Monaco requires the AMD module loader to be present on the page. It is not yet
        // compatible with ES6 imports. Once that happens, we can get rid of this.
        // See https://github.com/Microsoft/monaco-editor/issues/18
        (global["require"])(["vs/editor/editor.main"], () => {
            this.editor = monaco.editor.create(this.domPeer, {
                value: this.props.value,
                language: this.props.language,
                lineNumbers: "off"
            });

            this.editor.onDidChangeModelContent(event => {
                this.props.onChange(this.editor.getValue());
            });

            this.resize(this.props.height);
        });
        window.addEventListener("resize", this.resizeListener);
    }

    componentWillUnmount() {
        window.removeEventListener("resize", this.resizeListener);
    }

    componentDidUpdate(prevProps: Props) {
        if (prevProps.value !== this.props.value && this.editor) {
            this.editor.setValue(this.props.value);
        }

        if (prevProps.language !== this.props.language) {
            throw new Error('<MonacoEditor> language cannot be changed.');
        }

        if (this.editor) {
            this.resize(this.props.height);
        }
    }
}
