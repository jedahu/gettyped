import {Module} from "./module";
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
        return f(m.output.writeCanvas(width, height));
    };

const mkLog =
    (m : Module) => (...xs : Array<any>) =>
    m.output.writeLog(xs);

export const mk$GT = (m : Module) : $GT => Object.freeze({
    assert,
    assertp,
    randomInt,
    randomFloat,
    log: mkLog(m),
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
