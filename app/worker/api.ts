import * as ts from "typescript";
import {Api as RpcApi} from "ts-rpc/common";
import {Either} from "fp-ts/lib/Either";

export type Arg = {
    initialize : {modules : Array<Module>};
    setModule : Module;
    evalModule : {moduleName : string};
    dependencies : {
        path : string;
        code : string;
        target : ts.ScriptTarget;
        kind : ts.ScriptKind;
    };
}

export type Ret = {
    initialize : Result<Diagnostics>;
    setModule : Result<Diagnostics>;
    evalModule : Result<Either<Diagnostics, any>>;
    dependencies : Array<string>;
}

export type Api = RpcApi<Arg, Ret>;

export type Result<A> = Either<string, A>;

export type Module = {name : string; code : string};

export type DiagnosticMessage = {
    text : string;
    category : ts.DiagnosticCategory;
    code : number;
};

export type Diagnostic = {
    length: number;
    fileName: string;
    moduleName: string;
    messages: Array<DiagnosticMessage>;
    source: string | undefined;
    start: number;
};

export type Diagnostics = Array<Diagnostic>;