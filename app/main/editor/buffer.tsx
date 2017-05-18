import * as React from "react";

const global = window;

type Props = {
    onChange? : (newValue: string) => any;
    key : string;
    isCurrent : boolean;
    height : number;
    width : number;
    path : string;
    monaco : typeof monaco;
};

export default class EditorBuffer extends React.Component<Props, {}> {
    domPeer : HTMLDivElement;
    editor: monaco.editor.IStandaloneCodeEditor;

    render(): JSX.Element {
        const display = this.props.isCurrent ? "" : "none";
        const style = {
            height: this.props.height,
            width: "100%",
            display
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

        global.fetch(this.props.path).then(r => r.text()).then(txt => {
            this.editor = monaco.editor.create(this.domPeer, {
                value: txt,
                language: "typescript",
                lineNumbers: "off"
            });

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
        });
    }

    componentWillUnmount() {
        global.removeEventListener("resize", this.resizeListener);
    }

    componentDidUpdate(prevProps: Props) {
        if (this.editor) {
            this.resize(this.props.height);
        }
    }
}
