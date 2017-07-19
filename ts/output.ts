import * as tss from "./ts-services";
import {Config, Diag, Module, Modules, RunRet} from "./types";
import {html as h, text as t} from "./dom";
import {assertNever, arrayFlatMap, addTs, lastSegment} from "./util";
import {mapStackTrace} from "./trace";

const unnamedTypes = [Object, String, Number, RegExp, Date];

export const stringify = (x : any, asJson : boolean = false) : Node =>
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

const logTagInfo = (tag : LogTag) : string | null =>
    ({
        result: "result",
        log: "info",
        syntax: "syntax error",
        types: "semantic error",
        runtime: "runtime error",
        note: null,
        canvas: null
    })[tag];

const writeToOutput =
    (m : Module) =>
    (tag : LogTag) =>
    (html : Array<Node>, data? : {[k : string] : any}) => {
        m.output.classList.remove("gt-do-close");
        m.output.style.height = "auto";
        const title = logTagInfo(tag);
        const entry =
            h("li",
              {class: `gt-log-entry gt-log-entry-${tag}`},
              [ h("i",
                  { class: "gt-log-tag material-icons md-24",
                    ...(title ? {title} : {})
                  },
                  []),
                h("span", {}, html)
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

const diagsHost = (pageNs : string) => ({
    getCurrentDirectory() : string {
        return `"/${pageNs}`;
    },
    getCanonicalFileName(name : string) : string {
        return name.startsWith("/")
            ? addTs(name)
            : name.startsWith("./")
            ? addTs(`/${pageNs}/${lastSegment(name)}`)
            : name.includes("/")
            ? addTs(`/${name}`)
            : addTs(`/${pageNs}/${name}`);
    },
    getNewLine() : string {
        return "\n";
    }
});

export const writeDiag = (pageNs : string, m : Module) => (diag : Diag) => {
    const pos =
        diag.start !== undefined
        ? m.model.getPositionAt(diag.start)
        : undefined;
    writeToOutput(m)(diag.diagType)([
        h("span",
          {class: pos ? "gt-log-goto" : ""},
          [ h("span", {class: "gt-log-diag-message"}, [
              tss.formatDiagnostics([diag], diagsHost(pageNs)),
              ` (${m.path}`,
              pos ? `:${pos.lineNumber}:${pos.column}` : "",
              ")"
          ]),
          ],
          pos
          ? {data: {path: diag.module, line: pos.lineNumber, column: pos.column}}
          : undefined)
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

export const writeResult =
    async (
        {pageNs} : Pick<Config, "pageNs">,
        m : Module,
        ms : Modules,
        x : RunRet
    ) => {
        if (x.tag === "diagnostics") {
            writeNote(m)("Compile failed.");
            for (const diag of x.val) {
                writeDiag(pageNs, m)(diag)
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
    };

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
