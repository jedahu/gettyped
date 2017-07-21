import * as fpath from "./path";
import {Diag} from "./types";
import {Editor} from "./types";
import {FilePath} from "./path";
import {Model} from "./types";
import {ModulePath} from "./path";
import {Output} from "./output";
import {RunRet} from "./types";
import {Uri} from "./types";
import {filter} from "fp-ts/lib/Filterable";
import {fromNullable} from "./option";
import {getClient} from "./ts-services";
import {html as h} from "./dom";
import {option} from "./option";
import {tss} from "./ts-services"
import {unTs} from "./path";
import {whenSome} from "./option";

export type EditorOpts = {
    scrollbarSize : number;
    readOnly? : boolean;
    fontFamily? : string;
    fontSize? : number;
};

export type EditorArgs = EditorOpts & {
    model : Model;
};

export type ModuleArgs = EditorOpts & {
    path : string;
    cwd : string;
    text : string;
    target : HTMLElement;
    replaceTarget? : boolean;
    title? : string;
    editorOnly? : boolean;
    titleTag? : string;
    invisible? : boolean;
};

type TitleTarget = {
    text : HTMLElement;
    summary? : HTMLElement;
};

type ToolbarTarget = {
    container : HTMLElement;
    run : HTMLElement;
    revert : HTMLElement;
    clear : HTMLElement;
};

type Targets = {
    container : HTMLElement;
    editorContainer : HTMLElement;
    editor : HTMLElement;
    toolbar : ToolbarTarget;
    output : HTMLElement;
    content : HTMLElement;
    title? : TitleTarget;
};

const createEditor = (a : EditorArgs, t : Pick<Targets, "editor">) : Editor =>
    monaco.editor.create(t.editor, {
        model: a.model,
        lineNumbers: "off",
        fontFamily: a.fontFamily || "Droid Sans Mono",
        fontSize: a.fontSize || 13,
        theme: "lighter-default",
        scrollBeyondLastLine: false,
        overviewRulerBorder: false,
        dragAndDrop: true,
        renderLineHighlight: "none",
        minimap: {
            enabled: false
        },
        scrollbar: {
            vertical: "hidden",
            horizontalScrollbarSize: a.scrollbarSize
        },
        readOnly: a.readOnly
    });

const createModel = (uri : Uri, text : string) =>
    monaco.editor.createModel(text, "typescript", uri);

const iconClasses = "material-icons md-24 md-dark";

const prepareTarget = (a : ModuleArgs) : Targets => {
    a.target.innerText = "";
    const editor = h("div", {class: "gt-module-editor"});
    const editorContainer = h("div", {}, [editor]);
    const run =
        h("button",
          { class: `gt-action-run ${iconClasses}`,
            title: "Run"
          });
    const revert =
        h("button",
          { class: `gt-action-revert ${iconClasses}`,
            title: "Revert code"
          });
    const clear =
        h("button",
          { class: `gt-action-clear ${iconClasses}`,
            title: "Clear output"
          });
    const toolbarContainer =
        h("div", {class: "gt-module-toolbar"}, [run, revert, clear]);
    const toolbar = {container: toolbarContainer, run, revert, clear};
    const output = h("div", {class: "gt-module-output"});
    const content =
        h("div", {class: "gt-module-content"}, [
            editorContainer,
            toolbarContainer,
            output
        ]);
    const insert = (elem : HTMLElement) : void => {
        if (a.replaceTarget) {
            (a.target.parentNode as Element).replaceChild(elem, a.target);
        }
        else {
            a.target.appendChild(elem);
        }
    };
    if (a.editorOnly) {
        const container = h("div", {class: "gt-module-container"}, [content]);
        insert(container);
        return {container, editor, editorContainer, toolbar, output, content};
    }
    else {
        const tag = a.titleTag || "summary";
        const isSummary = tag === "summary";
        const text = h("span");
        const indicator = isSummary ? [h("i", {class: iconClasses})] : [];
        const titleElem =
            h(tag, {class: "gt-module-title"}, [
                text,
                ...indicator
            ]);
        const summary = isSummary ? titleElem : undefined;
        const title = {text, summary};
        const containerTag = isSummary ? "details" : "div";
        const container =
            h(containerTag, {class: "gt-module-container"}, [
                titleElem,
                content
            ]);
        insert(container);
        return {container, editor, editorContainer, toolbar, output, content, title}
    }
};

const getAbsPath = (a : ModuleArgs) : string =>
    fpath.join(a.cwd, a.path);

type InitRet = {
    editor : Editor;
    model : Model;
    uri : Uri;
    absPath : string;
    targets : Targets;
};

const initialize = (a : ModuleArgs) : InitRet => {
    const t = prepareTarget(a);
    const absPath = getAbsPath(a);
    const uri = Uri.parse(absPath);
    const model = createModel(Uri.parse(getAbsPath(a)), a.text);
    const editorArgs = {...a, model};
    const editor = createEditor(editorArgs, t);
    if (t.title) {
        t.title.text.innerText = a.title || a.path;
    }
    return {editor, model, absPath, uri, targets: t};
};

export class Module {
    "@nominal": "9d888206-d39d-466f-9d41-e16fb9ba5f24";

    readonly absPath : FilePath;
    readonly uri : Uri;
    readonly path : FilePath;
    readonly modulePath : ModulePath;
    readonly cwd : FilePath;
    readonly originalText : string;
    readonly editor : Editor;
    readonly model : Model;
    readonly output : Output;
    private readonly targets : Targets;
    private readonly scrollbarSize : number;

    private constructor(a : ModuleArgs, i : InitRet) {
        this.path = a.path as FilePath;
        this.cwd = a.cwd as FilePath;
        this.originalText = a.text;
        this.targets = i.targets;
        this.editor = i.editor;
        this.model = i.model;
        this.absPath = i.absPath as FilePath;
        this.modulePath = unTs(this.absPath);
        this.uri = i.uri;
        this.scrollbarSize = a.scrollbarSize;
        this.output = Output.mk({target: i.targets.output});

        // this.model.onDidChangeDecorations(() => {
        //     // Hide decorations because editor module resolution fails after the
        //     // worker times out.
        //     const ds = this.model.getAllDecorations();
        //     this.model.deltaDecorations(ds.map(d => d.id), []);
        // });

        this.model.onDidChangeContent(() => this.resizeEditor());

        this.targets.toolbar.revert.
            addEventListener("click", () => this.revert());

        this.targets.toolbar.clear.
            addEventListener("click", () => this.clearOutput());

        const summary =
            filter(option)(
                x => x.tagName.toLowerCase() === "summary",
                fromNullable(this.targets.editorContainer.closest("details")).
                    chain(d => fromNullable(d.firstElementChild)));

        whenSome(
            s => s.addEventListener("click", () => this.resizeEditor()),
            summary);
    }

    static mk(a : ModuleArgs) : Module {
        return new Module(a, initialize(a));
    }

    get runTarget() : EventTarget {
        return this.targets.toolbar.run;
    }

    get outputTarget() : EventTarget {
        return this.targets.output;
    }

    resizeEditor() : void {
        requestAnimationFrame(() => {
            const lh = this.editor.getConfiguration().lineHeight;
            const lc = this.model.getLineCount();
            const height = (lh * lc) + this.scrollbarSize;
            const c = this.targets.editorContainer;
            const width = c.offsetWidth;
            c.style.height = `${height}px`;
            this.editor.layout({height, width});
        });
    }

    revert() : void {
        this.model.setValue(this.originalText);
        this.clearOutput();
    }

    get diagnostics() : Promise<Array<Diag>> {
        return (async () => {
            const client = await getClient(this.uri);
            const [syn, sem] = await Promise.all([
                client.getSyntacticDiagnostics(this.absPath),
                client.getSemanticDiagnostics(this.absPath)
            ]);
            const synm =
                syn.map((d) : Diag => ({
                    ...d,
                    diagType : "syntax",
                    module: this.absPath
                }));
            const semm =
                sem.map((d) : Diag => ({
                    ...d,
                    diagType: "types",
                    module: this.absPath
                }));
            return synm.concat(semm);
        })();
    }

    get importedPaths() : Array<string> {
        return tss.preProcessFile(this.model.getValue(), true, false).
            importedFiles.
            map(f => f.fileName);
    }

    get emittedJs() : Promise<string> {
        return (async () => {
            const client = await getClient(this.uri)
            const emitted = await client.getEmitOutput(this.absPath);
            return emitted.
                outputFiles[0].
                text.
                replace(/^define\(/, `define('${fpath.unTs(this.absPath)}',`) +
                `\n//# sourceURL=${this.absPath}`;
        })();
    }

    clearOutput() : void {
        const output = this.targets.output;
        output.style.height = `${output.offsetHeight}px`;
        output.innerText = "";
        setTimeout(() => output.classList.add("gt-do-close"));
    }

    private startRunSpinner() : void {
        this.targets.toolbar.run.classList.add("gt-do-spin");
    }

    private stopRunSpinner() : void {
        this.targets.toolbar.run.classList.remove("gt-do-spin");
    }

    private async withRunSpinner<A>(f : () => Promise<A>) : Promise<A> {
        this.startRunSpinner();
        try {
            return await f();
        }
        finally {
            this.stopRunSpinner();
        }
    }

    async displayOutput(f : () => Promise<RunRet>) : Promise<void> {
        return this.withRunSpinner(async () => {
            this.clearOutput();
            const {sources, host, result} = await f();
            await this.output.writeResult(sources, host, result);
        });
    }
}
