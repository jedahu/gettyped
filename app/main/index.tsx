import * as React from "react";
import NavDrawer from "./NavDrawer";
import NavBar from "./NavBar";
import {Location} from "history";
import {history} from "./history";
import Editor from "./editor";
import * as SplitPane from "react-split-pane";

const global = window;

type Props = {
    showNav : boolean;
    article? : string | JSX.Element;
    editorHeight : number;
};

export class App extends React.Component<Props, Props> {
    historyUnlisten : () => void;

    constructor(props : Props) {
        super(props)
        this.state = {...props, unregister: () => {}};
    }

    toggleNav = () => {
        this.setState({showNav: !this.state.showNav});
    }


    goHome = () => history.push({pathname: "/"});

    typeHandler = (type : string) : Promise<void> =>
        global.
        fetch(`/doc/type/${type}.html`).
        then(r => r.text()).
        then(
            s => this.setState({article: s}),
            _ => this.setState({article: <p>For Oh For</p>})
        );

    routePath = (path : string) => {
        if (path === "/") {
            fetch("/doc/index.html").
                then(r => r.text()).
                then(s => this.setState({article: s}));
        }
        if (path.startsWith("/type:")) {
            this.typeHandler(path.substring("/type:".length));
        }
        else if (path.startsWith("module:")) {
        }
    }

    historyListener = ({pathname} : Location) => {
        console.log("history", pathname);
        this.routePath(pathname);
    }

    componentDidMount = () => {
        this.historyUnlisten = history.listen(this.historyListener);
        this.routePath(history.location.pathname);
    }

    componentWillUnmount = () => {
        this.historyUnlisten();
    }

    resizeEditor = (height : number) => {
        this.setState({editorHeight: height});
    }

    render() {
        const article = this.state.article;
        const eh = this.state.editorHeight;
        return (
            <div>
                <NavBar
                    title="Get typed"
                    onMenuClick={this.toggleNav}
                    onHomeClick={this.goHome}
                />
                <NavDrawer visible={this.state.showNav}/>
                <div id="gt-main">
                    <SplitPane
                        split="horizontal"
                        primary="second"
                        defaultSize={eh}
                        onDragFinished={this.resizeEditor}>
                        <Article
                            article={article}
                            height={`calc(100vw - ${eh}px)`}
                        />
                        <Editor
                            language="typescript"
                            value="hellow"
                            height={this.state.editorHeight}
                            onChange={_ => {}}
                        />
                    </SplitPane>
                </div>
            </div>
            );
    }
};

type ArticleProps = {
    article? : string | JSX.Element;
    height: string;
};

class Article extends React.Component<ArticleProps, undefined> {
    constructor(props : ArticleProps) {
        super(props);
    }

    render() {
        const article = this.props.article;
        const content = typeof article === "string"
              ? <div dangerouslySetInnerHTML={{__html: article}}></div>
              : <div>{article}</div>;
        return (
            <main
                id="gt-main-article"
                style={{height: this.props.height}}
                className="typeset">
                {content}
            </main>
        );
    }
}

// type Editor = any;

// type ModuleUi = {
//     module : string;
//     editor : Editor;
//     console : Element;
//     runButton : Element;
// };

// type ModuleUis = {[key : string] : ModuleUi};

// const worker = new appWorker();

// const allModuleElems = () : Array<HTMLElement> => {
//     const elems =
//         Array.from(document.querySelectorAll(".ts-edit")).
//               filter(e => e instanceof HTMLElement);
//     return elems as Array<HTMLElement>;
// };

// const setupModuleEdit = (elem : HTMLElement) => {
//     const editor = ace.edit(elem);
//     editor.setTheme("ace/theme/chrome");
//     editor.getSession().setMode("ace/mode/typescript");
//     editor.setOptions({
//         tabSize: 4,
//         useSoftTabs: true,
//         minLines: 1,
//         maxLines: 1000
//     });
//     return editor;
// };

// const setupModuleUi = (elem : HTMLElement) : ModuleUi => {
//     const module = elem.id;
//     const parent = elem.parentElement;
//     if (!parent) {
//         throw new Error("null parent");
//     }
//     const wrap = document.createElement("div");
//     const label = document.createElement("p");
//     const text = document.createTextNode("module " + module);
//     const console = document.createElement("pre");
//     const runButton = document.createElement("button");
//     const runText = document.createTextNode("eval");
//     const editor = setupModuleEdit(elem);
//     label.appendChild(text);
//     runButton.appendChild(runText);
//     parent.insertBefore(wrap, elem);
//     wrap.appendChild(label);
//     wrap.appendChild(elem);
//     wrap.appendChild(runButton);
//     wrap.appendChild(console);
//     const ui = {
//         module,
//         editor,
//         console,
//         runButton
//     };
//     editor.on("blur", () => sendSource(ui));
//     runButton.addEventListener("click", () => evaluate(ui));
//     return ui;
// };

// const setupModuleUis = (elems : Array<HTMLElement>) : ModuleUis => {
//     const uis : {[key : string] : ModuleUi} = {};
//     elems.forEach(el => {
//         const ui = setupModuleUi(el);
//         uis[ui.module] = ui;
//     });
//     return uis;
// };

// const recalculateLineNumbers =
//     (order : Array<string>, uis : ModuleUis) => {
//         let count = 1;
//         for (const module of order) {
//             const editor = uis[module].editor;
//             count += 1;
//             editor.setOption("firstLineNumber", count);
//             count += editor.getValue().split("\n").length;
//             count += 1;
//         }
//     };

// export const src = (kind: "clean" | "dirty" | "first") : S.Storer<string, wapi.Module> =>
//     kind === "first"
//         ? {
//             key: (k, s) =>
//                 S.bless(s).get(src("clean"))(k).fold(
//                     () => "source-dirty-" + k,
//                     () => "source-clean-" + k
//                 ),
//             unkey: key => key.substring(13),
//             getKey: m => m.name,
//             print: m => m.code,
//             parse: name => code => ({ name, code })
//         }
//         : {
//             key: k => "source-" + kind + "-" + k,
//             unkey: key => key.substring(13),
//             getKey: m => m.name,
//             print: m => m.code,
//             parse: name => code => ({ name, code })
//         };

// const sendSource = (ui : ModuleUi) => {
//     const m = {
//         name: ui.module,
//         code: ui.editor.getValue()
//     };
//     store.set(src("first"))(m);
//     api.setModule(m, _ => {});
// };

// const evaluate = (ui : ModuleUi) =>
//     api.evalModule({moduleName: ui.module}, _ => {});

// const api = makeApi<wapi.Arg, wapi.Ret>(worker, {
//     initialize: "initialize",
//     setModule: "setModule",
//     evalModule: "evalModule"
// });

// window.addEventListener("load", () => {
//     const elems = allModuleElems();
//     const moduleOrder = elems.map(el => el.id);
//     const moduleUis = setupModuleUis(elems);
//     const moduleUisInOrder =
//         moduleOrder.map(m => [m, moduleUis[m]] as [string, ModuleUi]);

//     recalculateLineNumbers(moduleOrder, moduleUis);

//     global.addEventListener("storage", e => {
//         if (e.storageArea === global.localStorage) {

//         }
//     });

//     moduleUisInOrder.forEach(
//         ([name, ui]) => store.set(src("clean"))({name, code: ui.editor.getValue()}));
//     api.initialize({
//         modules:
//         moduleUisInOrder.map(
//             ([name, ui]) => ({
//                 name,
//                 code: store.getset(src("clean"))(name, () => ({name, code: ui.editor.getValue()})).code
//             })),
//     },
//     _ => {});
// });
