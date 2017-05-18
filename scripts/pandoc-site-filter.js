#!/usr/bin/env node

const fs = require("fs-extra");
const pandoc = require("pandoc-filter");

pandoc.stdio((type, value, _format, _meta) => {
    if (type === "CodeBlock") {
        const [meta, src] = value;
        const [id, classes, attrs] = meta;
        const attrSet = new Map(attrs);
        const lang = attrSet.get("rundoc-language");
        const file = attrSet.get("rundoc-file");
        const module = attrSet.get("rundoc-module");
        const name = attrSet.get("rundoc-name");
        if (file) {
            fs.outputFileSync(file, src, {encoding: "utf8"});
        }
        else if (lang && module) {
            const path = module + "." + lang;
            fs.outputFileSync(path, src, {encoding: "utf8"});
        }
        const header =
              name && module
              ? [pandoc.Header(
                  5,
                  ["", ["gt-module-section"], []],
                  [pandoc.Str('"' + module + '"')])
                ]
              : [];
        const codeId = id || (module && "module:" + module) || "";
        const code = pandoc.CodeBlock([codeId, classes, attrs], src);
        return header.concat([code]);
    }
    else if (type === "Link") {
        const [meta, text, [ref, title]] = value;
        if (ref.startsWith("module:")) {
            return pandoc.Link(meta, text, ["#" + ref, title]);
        }
        else if (ref.startsWith("type:")) {
            return pandoc.Link(meta, text, ["#" + ref, title]);
        }
    }
    return undefined;
});
