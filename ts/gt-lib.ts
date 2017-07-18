import {Module} from "./types";
import {writeLog, writeCanvas} from "./output";
import {assert, assertp, randomInt, randomFloat} from "./gt-lib-shared";
import * as vex from "vex-js";
import * as vexDialog from "vex-dialog";
import "vex-js/dist/css/vex.css";
import "vex-js/dist/css/vex-theme-plain.css";

vex.registerPlugin(vexDialog);
vex.defaultOptions.className = "vex-theme-plain";

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
    };

const prompt =
    async (
        message : string,
        placeholder? : string,
        defaultValue? : string
    ) : Promise<string | undefined> =>
    new Promise<string | undefined>((res, rej) => {
        try {
            vex.dialog.prompt({
                message,
                placeholder,
                buttons: [vex.dialog.buttons.YES],
                callback: (val? : string) => res(val || defaultValue)
            });
        }
        catch (e) {
            rej(e);
        }
    });

export const mk$GT = (m : Module) : $GT => ({
    assert,
    assertp,
    randomInt,
    randomFloat,
    log: writeLog(m),
    canvas: mkCanvas(m),
    prompt
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
