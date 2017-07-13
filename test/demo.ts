import * as painless from "painless";
import * as globfs from "glob-fs";
import * as fs from "fs";
import {assert, assertp} from "gt-lib";

const mockCanvasContext : CanvasRenderingContext2D = {
    fillRect(..._ : any[]) : any {}
} as any;

const gtlib : $GT = {
    assert,
    assertp,
    log: (...xs : Array<any>) => {},
    canvas: <A>(
        size : number | [number, number],
        f : (ctx : CanvasRenderingContext2D) => A
    ) : A =>
        f(mockCanvasContext)
};

(global as any).$gt = gtlib;

const glob = () => globfs({gitignore: false});

const paths : Array<string> =
    glob().readdirSync("modules/**/*.ts");

const withSilentConsole = <A>(go : () => A) : A => {
    const log = console.log;
    console.log = (..._ : any[]) => {};
    try {
        return go();
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
                    painless.assert.isRejected(
                        new Promise((res, rej) => {
                            try {
                                const m = require(p);
                                if (typeof m.run === "function") {
                                    const x = m.run();
                                    if (x instanceof Promise) {
                                        x.then(res);
                                    }
                                    else {
                                        res();
                                    }
                                }
                                else {
                                    res();
                                }
                            }
                            catch (e) {
                                rej(e);
                            }
                        }),
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
