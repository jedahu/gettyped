import {assertNever} from "./util";

export const esc = (...s : Array<string>) : string => {
    const div = document.createElement("div");
    div.textContent = s.join("");
    return div.innerHTML;
};

type Attrs = {[k : string] : string};

type Handlers = {
    [K in keyof HTMLElementEventMap]?: (this : HTMLElement, evt : HTMLElementEventMap[K]) => void;
};

const dataPrefix = "gt";

export const text = (s : string) : Text =>
    document.createTextNode(s);

export const html = (
    name : string,
    attrs? : Attrs,
    children? : Array<string | Node>,
    opts? : {
        data?: {[k : string] : any},
        handlers?: Handlers
    }
) : HTMLElement => {
    const {data, handlers} = opts || {data: {}, handlers: {}};
    const elem = document.createElement(name);
    for (const [k, v] of Object.entries(attrs || {})) {
        elem.setAttribute(k, v);
    }
    for (const [k, v] of Object.entries(data || {})) {
        elem.dataset[`${dataPrefix}${k}`] = JSON.stringify(v);
    }
    for (const [k, v] of Object.entries(handlers || {})) {
        elem.addEventListener(k, v);
    }
    for (const c of children || []) {
        if (typeof c === "string") {
            elem.appendChild(document.createTextNode(c));
        }
        else if (c instanceof Node) {
            elem.appendChild(c);
        }
        else {
            assertNever(c);
        }
    }
    return elem;
};

export const dataset = (elem : HTMLElement) : {[k : string] : any} => {
    const x : {[k : string] : any} = {};
    for (const [k, v] of Object.entries(elem.dataset)) {
        if (k.startsWith(dataPrefix) && v) {
            x[k.substring(dataPrefix.length)] = JSON.parse(v);
        }
    }
    return x;
};

export const setDataset = (elem : HTMLElement, ds : {[k : string] : any}) => {
    for (const [k, v] of Object.entries(ds)) {
        elem.dataset[`${dataPrefix}${k}`] = JSON.stringify(v);
    }
};
