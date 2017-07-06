import * as painless from "painless";
import * as globfs from "glob-fs";

const glob = () => globfs({gitignore: false});

const rePaths : Array<string> =
    glob().readdirSync("demo/re/**/*.ts");

const cePaths : Array<string> =
    glob().readdirSync("demo/ce/**/*.ts");

const demoPaths : Array<string> =
    glob().
        readdirSync("demo/**/*.ts").
        filter((p : string) => !/\/(ce|re)\//.test(p));


if (rePaths.length === 0 ||
    cePaths.length === 0 ||
    demoPaths.length === 0
   ) {
    throw new Error("Not all modules found");
}


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
