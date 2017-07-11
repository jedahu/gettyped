import {DiagInfo, Module, RunRet} from "./types";
import {esc, mkElement} from "./dom";
import {assertNever, objEntries} from "./util";

const stringify = (x : any) : string =>
    typeof x === "string"
    ? esc(x)
    : typeof x === "undefined"
    ? "<span class='gt-log-special'>undefined</span>"
    : esc(
        ( x.constructor && x.constructor !== Object
          ? x.constructor.name + " "
          : ""
        ) +
            JSON.stringify(x, null, 2));

type LogTag = "result" | "log" | "syntax" | "types" | "runtime";

const logTagInfo = (tag : LogTag) : string =>
    ({
        result: "result",
        log: "info",
        syntax: "syntax error",
        types: "type error",
        runtime: "runtime error"
    })[tag];

const writeToOutput =
    (m : Module) =>
    (tag : LogTag) =>
    (html : Array<string>, data? : {[k : string] : string}) => {
        const entry =
            mkElement(
                `<li class='gt-log-entry gt-log-entry-${tag}'>`,
                "<span class='gt-log-tag'>",
                logTagInfo(tag),
                ":</span> ",
                html.join(""),
                // esc(xs.map(x => x.toString()).join(" ")),
                "</li>"
            );
        for (const [k, v] of objEntries(data || {})) {
            entry.dataset[k] = v;
        };
        m.output.appendChild(entry);
    };

export const writeLog = (m : Module) => (...xs : Array<any>) =>
    writeToOutput(m)("log")(
        xs.map(
            x => "<span class='gt-log-item'>" +
                stringify(x) +
                "</span> "
        ));

export const writeDiag = (m : Module) => (diag : DiagInfo) => {
    const pos = diag.position;
    writeToOutput(m)(diag.diagType)(
        [
            "<span class='gt-log-diag-module'>",
            esc(diag.module + ".ts"),
            "</span>: ",
            "<span class='gt-log-diag-message'>",
            esc(diag.message),
            "</span>"
        ],
        {
            line: pos === undefined ? "" : pos.line.toString(),
            column: pos === undefined ? "" :  pos.column.toString()
        });
};

export const writeRuntime = (m : Module) => (err : any) => {
    const requireMap : any = (err as any).requireMap;
    if (err instanceof Error && requireMap) {
        writeToOutput(m)("runtime")([
            "<span class='gt-log-runtime-message'>",
            err.message,
            "</span>"
        ]);
    }
    else if (err instanceof Error) {
        writeToOutput(m)("runtime")([
            "<span class='gt-log-runtime-error-name'>",
            err.name,
            "</span>: ",
            "<span class='gt-log-runtime-message'>",
            err.message,
            "</span>"
        ]);
    }
    else {
        writeToOutput(m)("runtime")([stringify(err)]);
    }
};

export const writeRet = (m : Module) => (x : any) =>
    writeToOutput(m)("result")([
        "<span class='gt-log-result'>",
        stringify(x),
        "</span>"
    ]);

export const writeCanvas =
    (m : Module) => (w : number, h : number) : CanvasRenderingContext2D => {
        const canvas = document.createElement("canvas");
        canvas.width = w;
        canvas.height = h;
        m.output.appendChild(canvas);
        return canvas.getContext("2d") as CanvasRenderingContext2D;
    };

export const writeResult = (m : Module, x : RunRet) => {
    if (x.tag === "diagnostics") {
        for (const diag of x.val) {
            writeDiag(m)(diag)
        }
    }
    else if (x.tag === "value") {
        writeRet(m)(x.val);
    }
    else if (x.tag === "runtime" || x.tag === "require") {
        writeRuntime(m)(x.val);
    }
    else {
        assertNever(x);
    }
}

export const clearOutput = (m : Module) => {
    m.output.innerHTML = "";
};
