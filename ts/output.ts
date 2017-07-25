import * as array from "./array";
import {CompileResult} from "./types";
import {Diag} from "./types"
import {ObjMap} from "./objmap";
import {WriteDiagHost} from "./types";
import {assertNever} from "./util";
import {fromNullable} from "./option";
import {html as h} from "./dom";
import {mapStackTrace} from "./trace";
import {text as t} from "./dom";
import {tss} from "./ts-services";

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

type OutputArgs = {
    target : HTMLElement;
};

export class Output {
    "@nominal" : "7cd20569-869c-4c23-87fa-82a538e82cb6";

    readonly target : HTMLElement;

    private constructor(a : OutputArgs) {
        this.target = a.target;
    }

    static mk(a : OutputArgs) : Output {
        return new Output(a);
    }

    write(tag : LogTag, html : Array<Node>, data? : {[k : string] : any}) : void {
        this.target.classList.remove("gt-do-close");
        this.target.style.height = "auto";
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
        this.target.appendChild(entry);
    };

    writeLog(xs : Array<any>) : void {
        this.write(
            "log",
            array.chain(
                x => [
                    h("span", {class: "gt-log-item"}, [stringify(x)]),
                    t(" ")
                ],
                xs));
    }

    writeRet(x : any) : void {
        this.write("result", [
            h("span", {class: "gt-log-result"}, [stringify(x, true)])
        ]);
    }

    writeNote(s : string) : void {
        this.write("note", [
            h("span", {class: "gt-log-note"}, [s])
        ]);
    }

    writeCanvas(w : number, h : number) : CanvasRenderingContext2D {
        const canvas = document.createElement("canvas");
        canvas.width = w;
        canvas.height = h;
        this.write("canvas", [canvas]);
        return canvas.getContext("2d") as CanvasRenderingContext2D;
    }

    writeDiag(host : WriteDiagHost, diag : Diag) : void {
        const path = diag.module;
        const pos =
            fromNullable(diag.start).chain(
                start => host.getPositionFor(path, start));
        this.write(diag.diagType, [
            h("span",
              {class: pos ? "gt-log-goto" : ""},
              [ h("span", {class: "gt-log-diag-message"}, [
                  tss.formatDiagnostics([diag], host),
                  ` (${path}`,
                  pos.fold(() => "", ([l, c]) => `:${l}:${c}`),
                  ")"
              ]),
              ],
              pos.fold(
                  () => undefined,
                  ([line, column]) => ({data: {path, line, column}})))
        ]);
    }

    writeRuntime(sources : ObjMap<string>, err : any) : void {
        if (err instanceof Error && err.stack) {
            this.write("runtime", [
                h("span",
                  {class: "gt-log-trace"},
                  mapStackTrace(err.stack, sources))
            ]);
        }
        else if (err instanceof Error) {
            this.write("runtime", [
                h("span",
                  {class: "gt-log-trace"},
                  [ stringify(err),
                    "\n",
                    h("span", {class: "gt-log-trace-note"}, [
                        "Note: use Chrome to enable",
                        " clickable source mapped traces."
                    ])
                  ])
            ]);
        }
        else {
            this.write("runtime", [
                h("span",
                  {class: "gt-log-trace"},
                  [ stringify(err),
                    "\n",
                    h("span", {class: "gt-log-trace-note"}, [
                        "Note: throw an instance of Error to",
                        " enable clickable source mapped traces."
                    ])
                  ])
            ]);
        }
    }

    async writeResult(
        sources : ObjMap<string>,
        host : WriteDiagHost,
        x : CompileResult
    ) : Promise<void> {
        if (x.tag === "diagnostics") {
            this.writeNote("Compile failed");
            for (const diag of x.val) {
                this.writeDiag(host, diag)
            }
        }
        else if (x.tag === "run") {
            const [tag, val] = await x.val;
            if (tag === "runtime") {
                this.writeRuntime(sources, val);
            }
            else if (tag === "value") {
                this.writeRet(val);
            }
            else {
                assertNever(tag);
            }
        }
        else {
            assertNever(x);
        }
    }
}
