const TSS = require("../lib/typescript-simple.js").TypeScriptSimple;
const tss = new TSS({});

let jsString = "";

const errorJson = e =>
      e && e.message
      ? e.message
      : JSON.stringify(e);

const compile = s => {
    try {
        jsString = tss.compile(s);
        return {
            action: "compile",
            status: "ok"
        };
    }
    catch (error) {
        return {
            action: "compile",
            status: "error",
            error: errorJson(error)
        };
    }
};

const evaluate = (namespace) => {
    try {
        return {
            action: "evaluate",
            status: "ok",
            namespace,
            value: new Function(`
                ${jsString};
                return ${namespace}.__eval();
            `)()
        };
    }
    catch (error) {
        return {
            action: "evaluate",
            status: "error",
            namespace,
            error: errorJson(error)
        };
    }
};

onmessage = e => {
    const data = e.data;
    const action = data.action;
    if (action === "compile") postMessage(compile(data.code));
    if (action === "evaluate") postMessage(evaluate(data.namespace));
};
