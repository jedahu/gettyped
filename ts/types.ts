export type Config = {
    pageNs: string;
};

export type Editor = monaco.editor.IStandaloneCodeEditor;
export type Model = monaco.editor.IModel;

export type Module = {
    editor : Editor;
    model : Model;
    name : string;
    path : string;
    uri : monaco.Uri;
    imports : Array<string>;
    originalText : string;
    section: HTMLElement;
    container : HTMLElement;
    runButton : HTMLElement;
    revertButton : HTMLElement;
    clearButton : HTMLElement;
    output : HTMLElement;
    js : string;
};

export type Modules = {[path : string] : Module};

export type DiagType = "syntax" | "types";
export type Diag = ts.Diagnostic & {
    diagType : DiagType;
    module : string;
};

export type DiagInfo = {
    module : string;
    message : string;
    diagType : DiagType;
    position? : {line : number; column : number;}
};

export type RunRet =
    {tag : "diagnostics"; val : Array<Diag>}
    | {tag : "run"; val : Promise<["value"|"runtime", any]>};

export type ObjMap<A> = {[k : string] : A};
