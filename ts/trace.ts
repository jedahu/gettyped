import * as array from "./array";
import {ObjMap} from "./objmap"
import {SourceMapConsumer} from "source-map";
import {html as h} from "./dom";
import {none} from "./option";
import {some} from "./option";

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

const sourceMapComment = /\/\/# [s]ourceMappingURL=(.*)[\s]*$/m;
const sourceMapUri = /data:application\/json;(charset=[^;]+;)?base64,(.*)/;

const getSourceMap = (src : string) : SourceMapConsumer | undefined => {
    const match = src.match(sourceMapComment);
    if (match && match.length === 2) {
        const uri = match[1];
        const smap = uri.match(sourceMapUri);
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
      {data: {path: source, line, column: column + 1}});

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

export const mapStackTrace =
    (trace : string, sources : ObjMap<string>) : Array<HTMLElement> => {
    const info = getTraceInfo();
    const lines = trace.split("\n");
    if (!info) {
        return lines.map(lineSpan0);
    }
    const msg = lines.slice(0, info.skip);
    const stack = lines.slice(info.skip);
    return msg.map(lineSpan0).concat(
        array.mapOption(
            x => {
                const fields = x.match(info.regex);
                if (fields && fields.length === info.fields + 1) {
                    const [,, uri, ln, cn] = fields;
                    if (/https?:\/\//.test(uri)) {
                        return none;
                    }
                    const line = parseInt(ln, 10);
                    const column = parseInt(cn, 10);
                    const src = sources[uri];
                    const name = getName(x) || "(unknown)";
                    return some(
                        renderLine(
                            uri,
                            line,
                            column,
                            name,
                            src ? getSourceMap(src) : undefined
                        ));
                }
                return none;
            },
            stack));
};
