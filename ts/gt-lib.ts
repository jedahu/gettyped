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

const mkCanvas =
    <A>(m : Module) => (
        size : number | [number, number],
        f : (ctx : CanvasRenderingContext2D) => A
    ) : A => {
        const [width, height] =
            typeof size === "number"
            ? [size, size]
            : size;
        return f(writeCanvas(m)(width, height));
    }

export const mk$GT = (m : Module) : $GT => ({
    assert,
    assertp,
    log: writeLog(m),
    canvas: mkCanvas(m)
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
