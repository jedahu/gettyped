;(function() {
    const siteRoot = window.__gt.config.siteRoot;

    requirejs.config({
        baseUrl: `${siteRoot}/modules`,
        paths: {vs: "https://unpkg.com/monaco-editor@0.9.0/min/vs"},
        waitSeconds: 0
    });

    window.MonacoEnvironment = {
        getWorkerUrl: function(/* workerId, label */) {
            return `${siteRoot}/static/monaco-editor-worker-loader-proxy.js`;
        }
    };
})();
