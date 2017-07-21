import * as tsss from "vs/language/typescript/lib/typescriptServices";
import {withTimeout, rejectIfTimedOut} from "./task";
import * as dialog from "./dialog";
import {html as h} from "./dom";
import {stringify} from "./output";

export const tss : typeof ts = tsss;

const getWorker = () =>
    monaco.languages.typescript.getTypeScriptWorker();

export const getClient = (uri : monaco.Uri) : Promise<ts.LanguageService> =>
    withTimeout(async token => {
        const worker = await getWorker();
        rejectIfTimedOut(token);
        return await worker(uri);
    }, {
        tries: 5,
        timeout: 10000
    }).
    catch(e => {
        dialog.alert({
            title: "Failed to load typescript language service",
            message: [
                h("p", {}, [
                    "Sometimes the web worker takes too long to spin up.",
                    " Try reloading the page."
                ]),
                h("p", {}, [
                    "Error: ",
                    stringify(e)
                ])
            ]
        });
        return Promise.reject(e);
    });
