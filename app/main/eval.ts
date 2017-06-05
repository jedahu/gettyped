import * as ts from "typescript";

type Model = monaco.editor.IModel;

export const tsWorker =
    (s : string) =>
    monaco.languages.typescript.getTypeScriptWorker().
    then((worker : (_:string) => monaco.Promise<any>) => worker(s));

export const withTsWorker =
    <A>(model : Model, f : (client : any) => A | monaco.Promise<A>) : monaco.Promise<A> =>
    monaco.languages.typescript.getTypeScriptWorker().
    then((worker : (_:string) => monaco.Promise<any>) => worker(model.uri.toString()).then(f));

export const emitOutput =
    (model : Model) : monaco.Promise<ts.EmitOutput> =>
    tsWorker(model.uri.toString()).
    then((client : any) => client.getEmitOutput(model.uri.toString()));
