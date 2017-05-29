import * as ts from "typescript";

type Model = monaco.editor.IModel;

export const emitOutput = (model : Model) =>
    monaco.languages.typescript.getTypeScriptWorker()
    .then(worker => {
        worker(model.uri)
            .then((client : any) => {
                client.getEmitOutput(model.uri.toString()).then((eo : ts.EmitOutput) => {
                });
            });
    });
