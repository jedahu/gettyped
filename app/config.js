System.config({
    typescriptOptions: {
        module: "system",
        typeCheck: "strict",
        noImplicitAny: true,
        noImplicitReturns: true,
        noImplicitThis: true,
        strictNullChecks: true,
        tsconfig: false
    },
    packages: {
        "ts": {
            "main": "lib/plugin.js"
        },
        "typescript": {
            "main": "lib/typescript.js",
            "meta": {
                "lib/typescript.js": {
                    "exports": "ts"
                }
            }
        },
        "app": {
            defaultExtension: false
        },
        "src": {
            defaultExtension: false,
            meta: {
                "*": {
                    loader: "exloader"
                }
            }
        }
    },
    map: {
        "ts": "node_modules/plugin-typescript",
        "typescript": "node_modules/typescript",
        "exloader": "app/inline-loader.js"
    },
    transpiler: 'ts'
});
