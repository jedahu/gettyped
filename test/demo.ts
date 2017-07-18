import * as painless from "painless";
import * as globfs from "glob-fs";
import * as fs from "fs";
import * as gt from "gt-lib-shared";
import * as matchPattern from "lodash-match-pattern";
import * as yaml from "js-yaml";

const noop = (..._ : Array<any>) : any => {};

const mockCanvasContext : CanvasRenderingContext2D = new Proxy({}, {
    get: () => noop
}) as any;

const doCheck = (name : string, val : any, check : string) : void => {
    const fail = matchPattern(val, check);
    if (null !== fail) {
        throw new Error(`Output check fail: ${name}: ${fail}.`);
    }
};

const logCheck = (checks : {[k : string] : string}) => (...xs : Array<any>) : void => {
    const name = xs && xs[0];
    const check = name && xs.length >= 2 && checks[name];
    if (check) {
        doCheck(name, xs[1], check);
    }
};

const prompt = (
    message : string,
    placeholder? : string,
    defaultValue? : string
) : Promise<string | undefined> =>
    Promise.resolve(defaultValue);

const gtlib = (checks : {}) : $GT => ({
    assert: gt.assert,
    assertp: gt.assertp,
    randomFloat: gt.randomFloat,
    randomInt: gt.randomInt,
    log: logCheck(checks),
    canvas: <A>(
        size : number | [number, number],
        f : (ctx : CanvasRenderingContext2D) => A
    ) : A =>
        f(mockCanvasContext),
    prompt
});

const glob = () => globfs({gitignore: false});

const paths : Array<string> =
    glob().readdirSync("modules/**/*.ts");

const withGtLib =
    async <A>(
        path : string,
        f : (retCheck? : string) => Promise<A>
    ) : Promise<A> => {
        const checkPath = path + ".check";
        const checks =
            fs.existsSync(checkPath)
            ? yaml.safeLoad(fs.readFileSync(checkPath, "utf8"))
            : {};
        (global as any).$gt = gtlib(checks);
        const retCheck = checks["$result"];
        try {
            return await f(retCheck);
        }
        finally {
            delete (global as any).$gt;
        }
    };

const test = painless.createGroup();

const expectStaticError = (p : string) =>
    test(
        "should fail to compile: " + p,
        () => painless.assert.throws(
            () => require(p),
            "Unable to compile TypeScript"));

const expectRuntimeError = (p : string) =>
    test(
        "should fail at runtime: " + p,
        () => withGtLib(
            p,
            () => painless.assert.isRejected(
                (async () => {
                    const m = require(p);
                    if (typeof m.run === "function") {
                        const x = m.run();
                        if (x instanceof Promise) {
                            await x;
                        }
                    }
                })(),
                /^(?!.*Unable to compile TypeScript)/)));

const expectNoError = (p : string) =>
    test(
        "should succeed at runtime: " + p,
        () => withGtLib(
            p,
            retCheck =>
                painless.assert.isFulfilled(
                    (async () => {
                        const m = require(p);
                        if (typeof m.run === "function") {
                            const x = m.run();
                            const ret =
                                x instanceof Promise
                                ? await x
                                : x;
                            if (retCheck !== undefined) {
                                doCheck("$result", ret, retCheck);
                            }
                        }
                    })()
                )));

for (const p of paths) {
    fs.existsSync(p + ".se")
        ? expectStaticError(p)
        : fs.existsSync(p + ".re")
        ? expectRuntimeError(p)
        : expectNoError(p);
}
