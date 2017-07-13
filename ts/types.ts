export type Editor = monaco.editor.IStandaloneCodeEditor;
export type Model = monaco.editor.IModel;

export type Module = {
    editor : Editor;
    model : Model;
    path : string;
    uri : monaco.Uri;
    imports : Array<string>;
    originalText : string;
    container : HTMLElement;
    runButton : HTMLElement;
    revertButton : HTMLElement;
    clearButton : HTMLElement;
    output : HTMLElement;
    js : string;
};

export type Modules = {[path : string] : Module};

export type DiagType = "syntax" | "types";
export type Diag = ts.Diagnostic & {diagType : DiagType};

export type DiagInfo = {
    module : string;
    message : string;
    diagType : DiagType;
    position? : {line : number; column : number;}
};

export type RunRet =
    {tag : "diagnostics"; val : Array<DiagInfo>}
    | {tag : "run"; val : Promise<["value"|"runtime", any]>};

export type ObjMap<A> = {[k : string] : A};
