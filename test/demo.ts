import * as painless from "painless";
import * as globfs from "glob-fs";
import * as fs from "fs";

const glob = () => globfs({gitignore: false});

const paths : Array<string> =
    glob().readdirSync("modules/**/*.ts");

const withSilentConsole = (go : () => void) => {
    const log = console.log;
    console.log = (..._ : any[]) => {};
    try {
        go();
    }
    finally {
        console.log = log;
    }
}

const test = painless.createGroup();

const expectStaticError = (p : string) =>
    test(
        "should fail to compile: " + p,
        () =>
            withSilentConsole(
                () =>
                    painless.assert.throws(
                        () => require(p),
                        "Unable to compile TypeScript")));

const expectRuntimeError = (p : string) =>
    test(
        "should fail at runtime: " + p,
        () =>
            withSilentConsole(
                () =>
                    painless.assert.throws(
                        () => require(p),
                        /^(?!.*Unable to compile TypeScript)/)));

const expectNoError = (p : string) =>
    test(
        "should succeed at runtime: " + p,
        () =>
            withSilentConsole(
                () =>
                    painless.assert.doesNotThrow(
                        () => require(p))));

for (const p of paths) {
    fs.existsSync(p + ".se")
        ? expectStaticError
        : fs.existsSync(p + ".re")
        ? expectRuntimeError(p)
        : expectNoError(p);
}
