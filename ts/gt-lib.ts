import {Module} from "./types";
import {writeLog, writeCanvas} from "./output";
import {animals, assert, assertp, randomInt, randomFloat} from "./gt-lib-shared";
import {alert, prompt} from "./dialog";

const mkCanvas =
    (m : Module) => <A>(
        size : number | [number, number],
        f : (ctx : CanvasRenderingContext2D) => A
    ) : A => {
        const [width, height] =
            typeof size === "number"
            ? [size, size]
            : size;
        return f(writeCanvas(m)(width, height));
    };

export const mk$GT = (m : Module) : $GT => Object.freeze({
    assert,
    assertp,
    randomInt,
    randomFloat,
    log: writeLog(m),
    canvas: mkCanvas(m),
    prompt,
    alert: (message : string) => alert(message),
    animals
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
