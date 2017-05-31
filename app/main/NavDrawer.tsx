import * as React from "react";
import {ITreeNode, Tree, Tabs2 as Tabs, Tab2 as Tab} from "@blueprintjs/core";
import * as Future from "fluture";
import * as part from "./part";
import {fetchText} from "./fetch";
import {Val} from "./adt";

export class NavChanged extends Val<{
    readonly type : string;
},
"7ffa141c-fd01-45d5-9625-f8097fe03602"> {}

const getNavText = () : Future<string, string> =>
    fetchText("/doc/type.nav").bimap(
        ({reason}) => reason,
        ({text}) => text
    );

type In = {
    currentName? : string;
};

type State = {
    typeNames: Array<string>;
};

export type Out = NavChanged;

const typesList = part.mk<In, State, Out>(
    ({updateState, signal}) => {
        getNavText().value(
            text => updateState((s : State) => ({typeNames: text.split(/\s+/)})));
        return {
            initialState: _ => ({typeNames: []}),
            render: ({props, state}) => {
                const nodeClick =
                    signal.emit(
                        (n : ITreeNode) =>
                            new NavChanged({type: "" + n.label}));
                return <Tree
                    contents={listNodes(state.typeNames, props.currentName)}
                    onNodeClick={nodeClick}
                />
            }
        };
    });

const listNodes = (names : Array<string>, currentName? : string) : Array<ITreeNode> =>
    names.map(label => ({
        id: "gt-types-list-" + label,
        label,
        isSelected: currentName === label
    }));

type NavIn = {
    visible : boolean;
    currentTypeName? : string;
};

export const mk = part.mk<NavIn, {}, Out>(
    ({signal}) => {
        const TypesList = typesList(signal);
        return {
            render: ({props}) =>
                <div
                    id="gt-nav-drawer"
                    style={{display: props.visible ? "block" : "none"}}>
                    <Tabs
                        id="92e5b1f2-55e0-4df8-a5d9-9077613b44ce"
                        defaultSelectedTabId="d4cae0ce-09a4-4ffb-8d2d-83767698e788">
                        <Tab
                            id="d4cae0ce-09a4-4ffb-8d2d-83767698e788"
                            title="Types"
                            panel={<TypesList currentName={props.currentTypeName}/>}
                        />
                        <Tab
                            id="3657b991-924f-434d-90f7-1401e7cfeaf0"
                            title="Modules"
                            panel={<TypesList currentName={props.currentTypeName}/>}
                        />
                    </Tabs>
                </div>
        }
    });
