import * as React from "react";
import {ITreeNode, Tree, Tabs2 as Tabs, Tab2 as Tab} from "@blueprintjs/core";
import {Location} from "history";
import {history} from "./history";

const global = window;

const getNavText = () : Promise<string> =>
    global.fetch("doc/typenav").
           then(r => r.text());

type TypesListState = {
    typeNames: Array<string>;
    currentName: null | string;
    unregister: () => void;
};

class TypesList extends React.Component<{}, TypesListState> {
    constructor() {
        super();
        this.state = {typeNames: [], currentName: null, unregister: () => {}};
        getNavText().then(this.receiveNavText);
    }

    receiveNavText = (text : string) => {
        this.setState({
            typeNames: text.split(/\s+/)
        });
    };

    showTypePage = (node : ITreeNode) =>
        history.push({
            pathname: "type:" + node.label
        });

    listNodes = () : Array<ITreeNode> =>
        this.state.typeNames.map(label => ({
            id: "gt-types-list-" + label,
            label,
            isSelected: this.state.currentName === label
        }));

    setSelectionFromPath = (path : string) =>
        this.setState({
            currentName:
            path.startsWith("/type:")
                ? path.substring("/type:".length)
                : null
        });

    historyListener = ({pathname} : Location) => {
        this.setSelectionFromPath(pathname);
    }

    componentDidMount = () => {
        var unregister = history.listen(this.historyListener);
        this.setState({unregister});
        this.historyListener(history.location);
    }

    componentWillUnmount = () => {
        this.state.unregister();
    }

    render() {
        return (
            <Tree
                contents={this.listNodes()}
                onNodeClick={this.showTypePage}
            />
        );
    }
}

type Props = {visible : boolean};

export default (props : Props) =>
    <div
        id="gt-nav-drawer"
        style={{display: props.visible ? "block" : "none"}}>
        <Tabs
            id="92e5b1f2-55e0-4df8-a5d9-9077613b44ce"
            defaultSelectedTabId="d4cae0ce-09a4-4ffb-8d2d-83767698e788">
            <Tab
                id="d4cae0ce-09a4-4ffb-8d2d-83767698e788"
                title="Types"
                panel={<TypesList/>}
            />
            <Tab
                id="3657b991-924f-434d-90f7-1401e7cfeaf0"
                title="Modules"
                panel={<TypesList/>}
            />
        </Tabs>
    </div>;
