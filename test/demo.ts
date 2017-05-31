import * as painless from "painless";
import * as glob from "glob-fs";

const rePaths =
    glob().readdirSync("demo/re/**/*.ts");

const cePaths =
    glob().readdirSync("demo/ce/**/*.ts");

const demoPaths =
    glob().
        readdirSync("demo/**/*.ts").
        filter((p : string) => !/\/(ce|re)\//.test(p));


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

cePaths.forEach(
    (p : string) =>
        test(
            "should fail to compile: " + p,
            () =>
                withSilentConsole(
                    () =>
                        painless.assert.throws(
                            () => require(p),
                            "Unable to compile TypeScript"))));

rePaths.forEach(
    (p : string) =>
        test(
            "should fail at runtime: " + p,
            () =>
                withSilentConsole(
                    () =>
                        painless.assert.throws(
                            () => require(p),
                            /^(?!.*Unable to compile TypeScript)/))));

demoPaths.forEach(
    (p : string) =>
        test(
            "should succeed at runtime: " + p,
            () =>
                withSilentConsole(
                    () =>
                        painless.assert.doesNotThrow(
                            () => require(p)))));
