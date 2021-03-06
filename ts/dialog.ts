import * as vex from "vex-js";
import * as vexDialog from "vex-dialog";
import {html as h} from "./dom";

vex.registerPlugin(vexDialog);
vex.defaultOptions.className = "vex-theme-plain";

export const prompt =
    async (
        message : string,
        placeholder? : string
    ) : Promise<string | undefined> =>
    new Promise<string | undefined>((res, rej) => {
        try {
            vex.dialog.prompt({
                message,
                placeholder,
                buttons: [vex.dialog.buttons.YES],
                callback: res
            });
        }
        catch (e) {
            rej(e);
        }
    });

export type AlertOpts = string | {
    title? : string;
    message : string | HTMLElement | Array<HTMLElement>;
};

const ensureArray =
    (x : string | HTMLElement | Array<HTMLElement>)
: Array<string | HTMLElement> =>
    Array.isArray(x) ? x : [x];

const ensureElem = (x : HTMLElement | Array<HTMLElement>) : HTMLElement =>
    Array.isArray(x) ? h("div", {}, x) : x;

export const alert =
    async (opts : AlertOpts) : Promise<void> =>
    vex.dialog.alert(
        typeof opts === "string"
            ? {message: opts}

            : opts.title
            ? { unsafeMessage:
                h("div", {}, [
                    h("div", {}, [h("strong", {}, [opts.title])]),
                    h("div", {}, ensureArray(opts.message))
                ])
            }

            : typeof opts.message === "string"
            ? {message: opts.message}

            : {unsafeMessage: ensureElem(opts.message)});
