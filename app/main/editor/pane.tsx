import * as React from "react";
import Buffer from "./buffer";

type Props = {
    id : string;
    height : number;
    width : number;
    monaco : typeof monaco;
    currentPath : string;
    paths : Array<string>;
};

export default class EditorPane extends React.Component<Props, {}> {
    constructor(props : Props) {
        super(props);
    }

    render() {
        const nodes = this.props.paths.map(
            p =>
                <Buffer
                    key={p}
                    isCurrent={this.props.currentPath === p}
                    height={this.props.height}
                    width={this.props.width}
                    path={p}
                    monaco={this.props.monaco}
                />);
        return <div id={this.props.id}>
            {nodes}
        </div>
    }
}
