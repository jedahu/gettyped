// import {inIdleTime} from "./utils";
// import {none} from "./option";
import * as RT from "./refined";
import * as array from "./array";
import * as omap from "./objmap";
import {Abs} from "./path";
import {FilePath} from "./path";
import {Module} from "./module";
import {Option} from "./option";
import {Path} from "./path";
import {RunRet} from "./types";
import {TsFile} from "./path";
import {WriteDiagHost} from "./types";
import {dataset} from "./dom";
import {flatten} from "fp-ts/lib/Chain";
import {fromNullable} from "./option";
import {normaliseTsPath} from "./path";
import {pair} from "./pair";
import {prequire} from "./prequire";
import {unrequire} from "./prequire";
import {whenSome} from "./option";
import {withGtLib} from "./gt-lib";

type PageArgs = {
    cwd : string;
    modules : Array<Module>;
};

const mkWriteDiagHost = (p : Page) : WriteDiagHost => ({
    getCurrentDirectory() : string {
        return p.cwd;
    },

    getCanonicalFileName(name : string) : Path<Abs & TsFile> {
        return normaliseTsPath(p.cwd, RT.lift(name, FilePath));
    },

    getNewLine() : string {
        return "\n";
    },

    getPositionFor(name : string, start : number) : Option<[number, number]> {
        return p.moduleByPath(RT.lift(name, FilePath)).chain(m => {
            const pos = fromNullable(m.model.getPositionAt(start));
            return pos.map(p => pair(p.lineNumber, p.column));
        });
    }
});

// const refreshModule = ({editor} : Module) : void => {
//     const pos = editor.getPosition();
//     const sel = editor.getSelection();
//     editor.executeEdits("force-recheck", [
//         { identifier: {major: 1, minor: 1},
//           range: new monaco.Range(0, 0, 0, 0),
//           text: " ",
//           forceMoveMarkers: false
//         },
//         { identifier: {major: 1, minor: 2},
//           range: new monaco.Range(0, 0, 0, 1),
//           text: "",
//           forceMoveMarkers: false
//         }
//     ]);
//     editor.setSelection(sel);
//     editor.setPosition(pos);
// };

// const refreshModules = (ms : Array<Module>) : Promise<void> =>
//     inIdleTime(function*() : IterableIterator<void> {
//         for (const m of ms) {
//             refreshModule(m);
//             yield;
//         }
//     });

export class Page {
    "@nominal" : "8f477dd8-46d7-4d0e-9ad4-a15443e215b5";

    readonly cwd : Path<Abs>;
    readonly modules : Readonly<{[p : string] : Module}>;
    readonly writeDiagHost : WriteDiagHost;
    private changePropagation : Promise<void>;

    private constructor(a : PageArgs) {
        this.cwd = RT.lift(a.cwd, FilePath, Abs);
        this.modules = Object.freeze(omap.keyMk(m => m.absPath, a.modules));
        this.writeDiagHost = mkWriteDiagHost(this);
        this.changePropagation = Promise.resolve();

        window.addEventListener("resize", () => this.resizeEditors());

        for (const m of a.modules) {
            m.runTarget.addEventListener("click", () => this.runDisplay(m));
            m.outputTarget.
                addEventListener("click", this.handleOutputClick);
            // m.editor.onDidChangeModelContent(e => {
            //     this.changePropagation.then(
            //         () => refreshModules(Object.values(this.modules)));
            // });
        }

        this.resizeEditors();
    }

    static mk(a : PageArgs) {
        return new Page(a);
    }

    moduleByPath(path : Path) : Option<Module> {
        const absPath = normaliseTsPath(this.cwd, path);
        return fromNullable(this.modules[absPath]);
    }

    resizeEditors() : void {
        for (const m of Object.values(this.modules)) {
            m.resizeEditor();
        }
    }

    importsFor(m : Module) : Array<Module> {
        return array.mapOption(
            p => this.moduleByPath(p),
            m.importedPaths);
    }

    transitiveImportsFor(m : Module) : Array<Module> {
        const deps : Array<Module> = [];
        const depset = new Set<string>();
        const go = (m : Module) => {
            const ms = this.importsFor(m);
            for (const m of ms) {
                go(m);
            }
            for (const m of ms) {
                if (!depset.has(m.path)) {
                    depset.add(m.path);
                    deps.push(m);
                }
            }
        }
        go(m);
        return deps;
    }

    transitiveImportsIncluding(m : Module) : Array<Module> {
        return this.transitiveImportsFor(m).concat([m]);
    }

    async run(m : Module) : Promise<RunRet> {
        const ms = await this.transitiveImportsIncluding(m);
        const diags =
            flatten(array)(await Promise.all(ms.map(m => m.diagnostics)));
        const sources =
            await omap.awaitValues(
                omap.mapValues(
                    m => m.emittedJs, omap.keyMk(m => m.absPath, ms)));
        if (diags.length > 0) {
            return {
                sources,
                host: this.writeDiagHost,
                result: {
                    tag: "diagnostics",
                    val: diags
                }
            };
        }
        return {
            sources,
            host: this.writeDiagHost,
            result: {
                tag: "run",
                val: withGtLib(m, async () => {
                    for (const m of ms) {
                        unrequire(m.modulePath)
                    }
                    try {
                        for (const m of ms) {
                            // new Function() messes up source map refs
                            eval(await m.emittedJs)
                        }
                        const [x] = await prequire([m.modulePath]);
                        const ret =
                            typeof x.run === "function"
                            ? x.run()
                            : undefined;
                        const val =
                            ret instanceof Promise
                            ? await ret
                            : ret;
                        return ["value", val] as ["value", any];
                    }
                    catch (e) {
                        return ["runtime", e] as ["runtime", any];
                    }
                })
            }
        };
    }

    runDisplay(m : Module) : Promise<void> {
        return m.displayOutput(() => this.run(m));
    }

    handleOutputClick = (e : MouseEvent) => {
        const elem = e.target;
        if (elem instanceof Element) {
            const entry = elem.closest(".gt-log-goto");
            if (entry instanceof HTMLElement) {
                const {path, line, column} = dataset(entry);
                if (isFinite(line) && isFinite(column)) {
                    whenSome(
                        m => {
                            m.editor.setPosition({lineNumber: line, column});
                            m.editor.focus();
                        },
                        this.moduleByPath(path));
                }
            }
        }
    }
}
