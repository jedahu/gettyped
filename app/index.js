const worker = new Worker("worker.js");

const compile = () =>
      worker.postMessage({
          action: "compile",
          code: jQuery(".org-src-container").codeblock("text")
      });

const evaluate = namespace =>
      worker.postMessage({
          action: "evaluate",
          namespace
      });

const onCompiled = (status, error) => {
    if (status === "ok") {
        console.log("compiled");
    }
    else {
        console.warn("compile error", error);
    }
};

const onEvaluated = (status, valOrErr) => {
    if (status === "ok") {
        console.log("evaluated", valOrErr);
    }
    else {
        console.warn("evaluation error", valOrErr);
    }
};

window.__gt_compile = compile;
window.__gt_evaluate = evaluate;

jQuery($ => {
    $(".org-src-container").codeblock();

    worker.onmessage = e => {
        const data = e.data;
        const action = data.action;
        if (action === "compile") {
            onCompiled(data.status, data.error);
        }
        if (action === "evaluate") {
            onEvaluated(data.status, data.value || data.error);
        }
    };
});
