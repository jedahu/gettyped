import {Option} from "./option";
import {Path} from "./path";
import {Abs} from "./path";
import {TsFile} from "./path";

import Editor = monaco.editor.IStandaloneCodeEditor;
import Model = monaco.editor.IModel;
import Uri = monaco.Uri;
export {Uri, Editor, Model};

export type Config = {
    pageCwd: string;
};

export type DiagType = "syntax" | "types";
export type Diag = ts.Diagnostic & {
    diagType : DiagType;
    module : Path<Abs & TsFile>;
};

export type DiagMap = {[absPath : string] : Array<Diag>};

export type DiagInfo = {
    module : string;
    message : string;
    diagType : DiagType;
    position? : {line : number; column : number;}
};

export type WriteDiagHost = ts.FormatDiagnosticsHost & {
    getPositionFor(path : string, start : number) : Option<[number, number]>;
};

export type CompileResult =
    {tag : "diagnostics"; val : Array<Diag>}
    | {tag : "run"; val : Promise<["value"|"runtime", any]>};

export type RunRet = {
    result : CompileResult;
    sources : ObjMap<string>;
    host : WriteDiagHost;
};

export type ObjMap<A> = {[k : string] : A};
