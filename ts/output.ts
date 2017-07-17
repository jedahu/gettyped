import {DiagInfo, Module, Modules, RunRet} from "./types";
import {html as h, text as t} from "./dom";
import {assertNever, arrayFlatMap} from "./util";
import {mapStackTrace} from "./trace";

const unnamedTypes = [Object, String, Number, RegExp, Date];

const stringify = (x : any, asJson : boolean = false) : Node =>
    typeof x === "string" && !asJson
    ? document.createTextNode(x)
    : typeof x === "undefined"
    ? h("span", {class: "gt-log-special"}, ["undefined"])
    : x instanceof Error
    ? document.createTextNode(x.toString())
    : document.createTextNode(
        ( x.constructor && !unnamedTypes.includes(x.constructor)
        ? x.constructor.name + " "
        : ""
        ) + JSON.stringify(x, null, 2));

type LogTag = "result" | "log" | "syntax" | "types" | "runtime" | "note" | "canvas";

const logTagInfo = (tag : LogTag) : string =>
    ({
        result: "result:",
        log: "info:",
        syntax: "syntax error:",
        types: "semantic error:",
        runtime: "runtime error:",
        note: "!!",
        canvas: ""
    })[tag];

const writeToOutput =
    (m : Module) =>
    (tag : LogTag) =>
    (html : Array<Node>, data? : {[k : string] : any}) => {
        m.output.classList.remove("gt-do-close");
        m.output.style.height = "auto";
        const entry =
            h("li",
              {class: `gt-log-entry gt-log-entry-${tag}`},
              [ h("span",
                  {class: "gt-log-tag"},
                  [logTagInfo(tag), " "]),
                ...html
              ],
              {data});
        m.output.appendChild(entry);
    };

export const writeLog = (m : Module) => (...xs : Array<any>) =>
    writeToOutput(m)("log")(
        arrayFlatMap(
            xs,
            x => [
                h("span", {class: "gt-log-item"}, [stringify(x)]),
                t(" ")
            ]));

export const writeDiag = (m : Module) => (diag : DiagInfo) => {
    const {line, column} =
        diag.position || {line: undefined, column: undefined};
    writeToOutput(m)(diag.diagType)([
        h("span",
          {class: "gt-log-goto"},
          [ h("span",
              {class: "gt-log-diag-message"},
              [diag.message]),
            t(" "),
            h("span",
              {class: "gt-log-diag-module"},
              [`(${diag.module}.ts:${line}:${column})`]),
          ],
          {data: {path: diag.module, line, column}})
    ]);
};

export const writeRuntime = (m : Module, ms : Modules) => (err : any) => {
    if (err instanceof Error && err.stack) {
        writeToOutput(m)("runtime")([
            h("span",
              {class: "gt-log-trace"},
              mapStackTrace(err.stack, ms))
        ]);
    }
    else if (err instanceof Error) {
        writeToOutput(m)("runtime")([
            h("span",
              {class: "gt-log-trace"},
              [ stringify(err),
                "\n",
                h("span",
                  {class: "gt-log-trace-note"},
                  ["Note: use Chrome or Firefox to enable clickable source mapped traces."])
              ])
        ]);
    }
    else {
        writeToOutput(m)("runtime")([
            h("span",
              {class: "gt-log-trace"},
              [ stringify(err),
                "\n",
                h("span",
                  {class: "gt-log-trace-note"},
                  ["Note: throw an instance of Error to enable clickable source mapped traces."])
              ])
        ]);
    }
};

export const writeRet = (m : Module) => (x : any) =>
    writeToOutput(m)("result")([
        h("span", {class: "gt-log-result"}, [stringify(x, true)])
    ]);

export const writeNote = (m : Module) => (s : string) =>
    writeToOutput(m)("note")([
        h("span", {class: "gt-log-note"}, [s])
    ]);

export const writeCanvas =
    (m : Module) => (w : number, h : number) : CanvasRenderingContext2D => {
        const canvas = document.createElement("canvas");
        canvas.width = w;
        canvas.height = h;
        writeToOutput(m)("canvas")([canvas]);
        return canvas.getContext("2d") as CanvasRenderingContext2D;
    };

export const writeResult = async (m : Module, ms : Modules, x : RunRet) => {
    if (x.tag === "diagnostics") {
        writeNote(m)("Compile failed.");
        for (const diag of x.val) {
            writeDiag(m)(diag)
        }
    }
    else if (x.tag === "run") {
        const [tag, val] = await x.val;
        if (tag === "runtime") {
            writeRuntime(m, ms)(val);
        }
        else if (tag === "value") {
            writeRet(m)(val);
        }
        else {
            assertNever(tag);
        }
    }
    else {
        assertNever(x);
    }
}

export const clearOutput = (m : Module) => {
    m.output.style.height = `${m.output.offsetHeight}px`;
    m.output.innerText = "";
    // const children = [].slice.call(m.output.childNodes);
    setTimeout(() => m.output.classList.add("gt-do-close"));
    // setTimeout(() => {
    //     for (const c of children) {
    //         c.remove();
    //     }
    // }, 300);
};
