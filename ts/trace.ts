import {SourceMapConsumer} from "source-map";
import {Module, Modules} from "./types";
import {html as h} from "./dom";
import {arrayFlatMap, lastSegment, unTs} from "./util";
import {siteRoot} from "./config";

type TraceInfo = {
    regex : RegExp;
    fields : number;
    skip : number;
};

const chromeInfo : TraceInfo = {
    regex: /^\s+at\s+(\S+)\s+\((\S+?):([0-9]+):([0-9]+)\)/,
    fields: 4,
    skip: 1
};

const firefoxInfo : TraceInfo = {
    regex: /@(.*):([0-9]+):([0-9]+)/,
    fields: 4,
    skip: 0
};

const isChrome = () : boolean =>
    navigator.userAgent.toLowerCase().indexOf('chrome') > -1;

const isFirefox = () : boolean =>
    navigator.userAgent.toLowerCase().indexOf('firefox') > -1;

const getTraceInfo = () : TraceInfo | undefined =>
    isChrome() ? chromeInfo :
    isFirefox() ? firefoxInfo :
    undefined;

const getSourceMap = (m : Module) : SourceMapConsumer | undefined => {
    const js = m.js;
    const match = js.match(/\/\/# [s]ourceMappingURL=(.*)[\s]*$/m);
    if (match && match.length === 2) {
        const uri = match[1];
        const smap = uri.match(/data:application\/json;(charset=[^;]+;)?base64,(.*)/);
        if (smap && smap[2]) {
            return new SourceMapConsumer(atob(smap[2]));
        }
    }
    return undefined;
};

type SLC = {
    source : string;
    line : number;
    column : number;
};

const lineSpan = ({source, line, column} : SLC, name : string) : HTMLElement =>
    // source map columns start at 0, monaco's at 1
    h("span",
      {class: "gt-trace-line gt-log-goto"},
      [`    at ${name} (${source}:${line}:${column + 1})\n`],
      {data: {path: unTs(source), line, column: column + 1}});

const lineSpan0 = (text : string) : HTMLElement =>
    h("span",
      {class: "gt-trace-line"},
      [`${text}\n`]);

const renderLine = (
    uri : string,
    line : number,
    column : number,
    name : string,
    smap? : SourceMapConsumer
) : HTMLElement => {
    const pos = smap && smap.originalPositionFor({line, column});
    return smap && pos
        ? lineSpan(pos, pos.name || name)
        : lineSpan({source: uri, line, column}, name);
};

const getName = (line : string) : string | undefined => {
    const match =
        line.match(
            isChrome()
                ? / +at +([^ ]*).*/
                : /([^@]*)@.*/);
    return match ? match[1] : undefined;
};

export const mapStackTrace = (trace : string, modules : Modules) : Array<HTMLElement> => {
    const info = getTraceInfo();
    const lines = trace.split("\n");
    if (!info) {
        return lines.map(lineSpan0);
    }
    const msg = lines.slice(0, info.skip);
    const stack = lines.slice(info.skip);
    return msg.map(lineSpan0).concat(
        arrayFlatMap(stack, x => {
            const fields = x.match(info.regex);
            if (fields && fields.length === info.fields + 1) {
                const [,, uri, ln, cn] = fields;
                if (uri.includes(siteRoot) || /https?:\/\//.test(uri)) {
                    return [];
                }
                const line = parseInt(ln, 10);
                const column = parseInt(cn, 10);
                const m = modules[unTs(lastSegment(uri))];
                const name = getName(x) || "(unknown)";
                return [renderLine(uri, line, column, name, m ? getSourceMap(m) : undefined)];
            }
            return [];
        }));
};
