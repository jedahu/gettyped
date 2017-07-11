import {Module} from "./types";
import {writeLog, writeCanvas} from "./output";

export const assert = (x : boolean, msg? : string) : void => {
    if (!x) {
        throw new Error("Assertion failed. " + (msg || ""));
    }
};

export const assertp =
    async (p : Promise<boolean>, msg? : string) : Promise<void> =>
    assert(await p, msg);

const mkWithCanvas =
    <A>(m : Module) => (
        f : (ctx : CanvasRenderingContext2D) => A,
        width : number = 300,
        height : number = 150
    ) : A =>
    f(writeCanvas(m)(width, height));

export const mk$GT = (m : Module) : $GT => ({
    assert,
    assertp,
    log: writeLog(m),
    withCanvas: mkWithCanvas(m)
});

export const withGtLib =
    async <A>(m : Module, f : () => Promise<A>) : Promise<A> => {
        (window as any).$gt = mk$GT(m);
        try {
            return await f();
        }
        finally {
            delete (window as any).$gt;
        }
    };
