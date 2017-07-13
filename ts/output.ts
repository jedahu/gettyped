import {DiagInfo, Module, Modules, RunRet} from "./types";
import {html as h} from "./dom";
import {assertNever} from "./util";
import {mapStackTrace} from "./trace";

const unnamedTypes = [Object, String, Number, RegExp, Date];

const stringify = (x : any) : Node =>
    typeof x === "string"
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

type LogTag = "result" | "log" | "syntax" | "types" | "runtime";

const logTagInfo = (tag : LogTag) : string =>
    ({
        result: "result",
        log: "info",
        syntax: "syntax error",
        types: "semantic error",
        runtime: "runtime error"
    })[tag];

const writeToOutput =
    (m : Module) =>
    (tag : LogTag) =>
    (html : Array<Node>, data? : {[k : string] : any}) => {
        const entry =
            h("li",
              {class: `gt-log-entry gt-log-entry-${tag}`},
              [ h("span",
                  {class: "gt-log-tag"},
                  [logTagInfo(tag), ": "]),
                ...html
              ],
              {data});
        m.output.appendChild(entry);
    };

export const writeLog = (m : Module) => (...xs : Array<any>) =>
    writeToOutput(m)("log")(
        xs.map(
            x => h("span", {class: "gt-log-item"}, [stringify(x)])));

export const writeDiag = (m : Module) => (diag : DiagInfo) => {
    const pos = diag.position;
    writeToOutput(m)(diag.diagType)(
        [
            h("span",
              {class: "gt-log-diag-module gt-log-goto"},
              [`${diag.module}.ts`]),
            document.createTextNode(":"),
            h("span",
              {class: "gt-log-diag-message"},
              [diag.message])
        ],
        {
            path: diag.module,
            line: pos === undefined ? "" : pos.line.toString(),
            column: pos === undefined ? "" :  pos.column.toString()
        });
};

export const writeRuntime = (m : Module, ms : Modules) => (err : any) => {
    // const requireMap : any = (err as any).requireMap;
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
                h("span",
                  {class: "gt-log-trace-note"},
                  ["Note: throw an instance of Error to enable clickable source mapped traces."])
              ])
        ]);
    }
};

export const writeRet = (m : Module) => (x : any) =>
    writeToOutput(m)("result")([
        h("span", {class: "gt-log-result"}, [stringify(x)])
    ]);

export const writeCanvas =
    (m : Module) => (w : number, h : number) : CanvasRenderingContext2D => {
        const canvas = document.createElement("canvas");
        canvas.width = w;
        canvas.height = h;
        m.output.appendChild(canvas);
        return canvas.getContext("2d") as CanvasRenderingContext2D;
    };

export const writeResult = (m : Module, ms : Modules, x : RunRet) => {
    if (x.tag === "diagnostics") {
        for (const diag of x.val) {
            writeDiag(m)(diag)
        }
    }
    else if (x.tag === "value") {
        writeRet(m)(x.val);
    }
    else if (x.tag === "runtime" || x.tag === "require") {
        writeRuntime(m, ms)(x.val);
    }
    else {
        assertNever(x);
    }
}

export const clearOutput = (m : Module) => {
    m.output.innerHTML = "";
};
