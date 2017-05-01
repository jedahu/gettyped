import * as api from "../worker/api";

declare const ace : any;

type Editor = any;

type ModuleUi = {
    module : string;
    editor : Editor;
    console : Element;
    runButton : Element;
};

type ModuleUis = {[key : string] : ModuleUi};

const worker = new Worker("worker.js");

const allModuleElems = () =>
    Array.from(document.querySelectorAll(".ts-edit"));

const setupModuleEdit = (elem : Element) => {
    const editor = ace.edit(elem);
    editor.setTheme("ace/theme/chrome");
    editor.getSession().setMode("ace/mode/typescript");
    editor.setOptions({
        tabSize: 4,
        useSoftTabs: true,
        minLines: 1,
        maxLines: 1000
    });
    return editor;
};

const setupModuleUi = (elem : Element) : ModuleUi => {
    const module = elem.id;
    const parent = elem.parentElement;
    if (!parent) {
        throw new Error("null parent");
    }
    const wrap = document.createElement("div");
    const label = document.createElement("p");
    const text = document.createTextNode("module " + module);
    const console = document.createElement("pre");
    const runButton = document.createElement("button");
    const runText = document.createTextNode("eval");
    const editor = setupModuleEdit(elem);
    label.appendChild(text);
    runButton.appendChild(runText);
    parent.insertBefore(wrap, elem);
    wrap.appendChild(label);
    wrap.appendChild(elem);
    wrap.appendChild(runButton);
    wrap.appendChild(console);
    const ui = {
        module,
        editor,
        console,
        runButton
    };
    editor.on("blur", () => sendSource(ui));
    runButton.addEventListener("click", () => evaluate(ui));
    return ui;
};

const setupModuleUis = (elems : Array<Element>) : ModuleUis => {
    const uis : {[key : string] : ModuleUi} = {};
    elems.forEach(el => {
        const ui = setupModuleUi(el);
        uis[ui.module] = ui;
    });
    return uis;
};

const recalculateLineNumbers =
    (order : Array<string>, uis : ModuleUis) => {
        let count = 1;
        for (const module of order) {
            const editor = uis[module].editor;
            count += 1;
            editor.setOption("firstLineNumber", count);
            count += editor.getValue().split("\n").length;
            count += 1;
        }
    };

const updateErrorLines = (lines : Array<[number, string]>) => {
    const cells = Array.from(document.querySelectorAll(".ace_gutter-cell"));
    cells.forEach(c => {
        c.removeAttribute("title");
        c.classList.remove("gt-error");
    });
    lines.forEach(
        ([n, err]) =>
            cells.filter(c => c.textContent === "" + n).forEach(c => {
                c.setAttribute("title", err);
                c.classList.add("gt-error");
            }));
};

const clearDisplays = (uis : ModuleUis) =>
    Object.keys(uis).forEach(k => uis[k].console.textContent = "");

const displayValue = (
    {module, value} : {module : string, value : string},
    uis : ModuleUis
) => {
    clearDisplays(uis);
    uis[module].console.textContent = value;
};

const displayError = (
    {module, error} : {module : string, error : string},
    uis : ModuleUis
) => {
    clearDisplays(uis);
    uis[module].console.textContent = error;
};

const sendSource = (ui : ModuleUi) =>
    worker.postMessage(<api.Source>{
        action: "source",
        module: ui.module,
        code: ui.editor.getValue()
    });

const evaluate = (ui : ModuleUi) =>
      worker.postMessage(<api.Evaluate>{
          action: "evaluate",
          module: ui.module,
      });

const handlers : {[key : string] : Function} = {
    evaluate: ({value} : api.EvaluateSuccess) => {
        console.log("evaluated", value);
    }
};

window.addEventListener("load", () => {
    const elems = allModuleElems();
    const moduleOrder = elems.map(el => el.id);
    const moduleUis = setupModuleUis(elems);
    const moduleUisInOrder =
        moduleOrder.map(m => <[string, ModuleUi]>[m, moduleUis[m]]);

    recalculateLineNumbers(moduleOrder, moduleUis);

    worker.postMessage(<api.Initialize>{
        action: "initialize",
        modules: moduleUisInOrder.map(([m, ui]) => [m, ui.editor.getValue()])
    });

    worker.onmessage = e => {
        const data = e.data;
        const handler = handlers[data.action];
        if (handler) {
            if (data.status === "ok") {
                handler(data);
                displayValue(data, moduleUis);
            }
            else {
                console.warn(data.action + " error", data.error);
                updateErrorLines(data.errorLines || []);
                displayError(data, moduleUis);
            }
        }
    };
});
