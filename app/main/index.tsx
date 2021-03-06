import * as React from "react";
import * as NavDrawer from "./NavDrawer";
import * as NavBar from "./NavBar";
import * as History from "./history";
import * as Editor from "./editor/pane";
import * as SplitPane from "react-split-pane";
import * as part from "./part";
import * as Future from "fluture";
import {Val} from "./adt";

import style from "../../scss/vars";

const global = window;

export class EditorResize extends Val<{
    size: number;
}, "ab645753-4486-4d77-92ed-8989d67f4e6c"> {
    static mk(size : number) : EditorResize {
        return new EditorResize({size});
    }
}

export class EditorWidth extends Val<{
    width: number;
}, "0386e151-22da-4ca5-8f3f-d95c3585218a"> {
    static mk(width : number) : EditorWidth {
        return new EditorWidth({width});
    }
}

export class PossibleModuleClick extends Val<{
event: MouseEvent;
}, "0386e151-22da-4ca5-8f3f-d95c3585218a"> {
    static mk(event: MouseEvent) : PossibleModuleClick {
        return new PossibleModuleClick({event});
    }
}

type In = {
    showNav : boolean;
    article? : string | JSX.Element;
    editorHeight : number;
    currentModulePath : string;
};

type State = In & {
    modulePaths : Array<string>;
    editorWidth : number;
    editorDisplay : {visible : boolean};
    currentPath : string;
};

export const mk = part.mk<In, State, {}>(
    ({updateState}) => {
        let articleDom : HTMLElement;

        const typeHandler = (type : string) =>
            Future.tryP<Error, string>(
                () => global.fetch(`/doc/type/${type}.html`).
                             then(r => r.text())
            ).value(
                html =>
                    updateState((_ : State) => ({article: html})));

        const routePath = (path : string) => {
            if (path === "/") {
                return Future.tryP<Error, string>(
                    () => global.fetch("/doc/index.html").
                                 then(r => r.text())
                ).value(
                    html =>
                        updateState((_ : State) => ({article: html})));
            }
            if (path.startsWith("/type:")) {
                return typeHandler(path.substring("/type:".length));
            }
            return updateState((_ : State) => ({article: "<p>404</p>"}));
        }

        const internal =
            part.Signal.
                handle(
                    History.LocationChange,
                    ({location: {pathname}}) => {
                        routePath(pathname);
                        updateState((_ : State) => ({currentPath: pathname}));
                    }).
                handle(
                    NavBar.MenuClick,
                    () => updateState((s : State) => ({showNav: !s.showNav}))).
                handle(
                    NavBar.HomeClick,
                    () => updateState((_ : State) => ({currentPath: "/"}))).
                handle(
                    NavDrawer.NavChanged,
                    ({val: {type}}) => updateState((_ : State) => ({currentPath: `/type:${type}`}))).
                handle(
                    Editor.PathChange,
                    ({val: {path}}) => updateState((_ : State) => ({currentModulePath: path}))).
                handle(
                    Editor.ToggleDisplay,
                    () => updateState(
                        (s : State) => ({
                            editorDisplay: {visible : !s.editorDisplay.visible}
                        }))).
                handle(
                    EditorResize,
                    ({val: {size}}) => updateState((_ : State) => ({editorHeight: size}))).
                handle(
                    EditorWidth,
                    ({val: {width}}) => updateState((_ : State) => ({editorWidth: width}))).
                handle(
                    PossibleModuleClick,
                    ({val: {event: {target}}}) => {
                        if (target instanceof HTMLElement &&
                            target.hasAttribute("rundoc-module")) {
                            const module = target.getAttribute("rundoc-module");
                            const path = `/${module}.ts`;
                            updateState(
                                (state : State) =>
                                    state.modulePaths.indexOf(path) >= 0
                                        ? {currentModulePath: path}
                                        : {
                                            moduelPaths: state.modulePaths.concat([path]),
                                            currentModulePath: path
                                        });
                        }
                    });

        const HistoryElem = History.mk(internal);
        const NavBarElem = NavBar.mk(internal);
        const NavDrawerElem = NavDrawer.mk(internal);
        const EditorElem = Editor.mk(internal);
        return {
            initialState: (props : In) => ({
                ...props,
                modulePaths: [props.currentModulePath],
                editorWidth: 0,
                editorDisplay: {visible: true},
                currentPath: global.location.pathname
            }),
            render: ({state}) => {
                const article = state.article;
                const up = state.editorDisplay.visible;
                const deh = state.editorHeight;
                const eh = up ? deh : style.navBarHeight;
                const typeName =
                    state.currentPath.startsWith("/type:")
                    ? state.currentPath.substring("/type:".length)
                    : undefined;
                return <div className={state.showNav ? "gt-visibleNav" : "gt-hiddenNav"}>
                    <HistoryElem path={state.currentPath}/>
                    <NavBarElem title="Get typed"/>
                    <NavDrawerElem
                        visible={state.showNav}
                        currentTypeName={typeName}
                    />
                    <div id="gt-main">
                        <SplitPane
                            split="horizontal"
                            primary="second"
                            defaultSize={deh}
                            size={eh}
                            allowResize={up}
                            onChange={internal.emit(EditorResize.mk)}>
                            <div ref={a => { articleDom = a; }}>
                                <Article
                                    article={article}
                                    height={`calc(100vh - ${eh}px - ${style.navBarHeight})`}
                                />
                            </div>
                            <EditorElem
                                id="gt-main-editor"
                                currentPath={state.currentModulePath}
                                display={state.editorDisplay}
                                height={state.editorHeight}
                                width={state.editorWidth}
                            />
                        </SplitPane>
                    </div>
                </div>
            },
            update:
                part.Signal.
                     handle<part.Begin<In, State>>(
                        part.Begin,
                        () => {
                            routePath(global.location.pathname);
                            const main = document.getElementById("gt-main") as HTMLElement;
                            global.setTimeout(() => {
                                global.requestAnimationFrame(() => {
                                    internal.run(EditorWidth.mk(main.offsetWidth));
                                });
                            }, 1);
                            articleDom.addEventListener("click", internal.emit(PossibleModuleClick.mk));
                            window.addEventListener("resize", internal.emit(_ => EditorWidth.mk(main.offsetWidth)));
                        }).
                    handle<part.End<In, State>>(
                        part.End,
                        () => {
                            articleDom.removeEventListener(
                                "click",
                                internal.emit(PossibleModuleClick.mk)
                            );
                        }).
                    ignore<part.Change<In, State>>(part.Change)
        };
    });


type ArticleProps = {
    article? : string | JSX.Element;
    height: string;
};

const Article = (props : ArticleProps) => {
    const article = props.article;
    const content = typeof article === "string"
                  ? <div dangerouslySetInnerHTML={{__html: article}}></div>
                  : <div>{article}</div>;
    return (
        <main
            id="gt-main-article"
            style={{height: props.height}}
            className="typeset">
            {content}
        </main>
    );
};

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
