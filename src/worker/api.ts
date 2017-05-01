export type Source = {
    action : "source";
    module : string;
    code : string;
};

export type Success<A> = {
    action : A;
    status : "ok";
};

export type Failure<A> = {
    action : A;
    status : "error";
    error : string;
};

export type Evaluate = {
    action : "evaluate";
    module : string;
};

export type EvaluateSuccess = {
    action : "evaluate";
    module : string;
    status : "ok";
    value : any;
};

export type RuntimeFailure = {
    action : "evaluate";
    module : string;
    status : "error";
    kind : "runtime";
    error : string;
};

export type CompileFailure = {
    action : "evaluate";
    module : string;
    status : "error";
    kind : "compile";
    error : string;
    errorLines : Array<[number, string]>;
};

export type EvaluateResponse =
    EvaluateSuccess | RuntimeFailure | CompileFailure;

export type Initialize = {
    action : "initialize";
    modules : Array<[string, string]>;
};

export type InitializeResponse =
    Success<"initialize"> | Failure<"initialize">;

export type Action =
    Initialize | Source | Evaluate;
