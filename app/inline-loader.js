var ts = require("ts");

exports.translate = function() {
    return ts.translate.apply(ts, arguments);
};

exports.instantiate = function() {
    return ts.instantiate.apply(ts, arguments);
};

exports.fetch = function(url) {
    return new Promise(function(resolve, reject) {
        if (url.address.startsWith(document.location.href)) {
            var path = url.address.substring(document.location.href.length - 1);
            var container = $(".codeblock-container").filter(function() {
                return $(this).prev().text().trim() === "Module: " + path;
            });
            if (container) {
                resolve(container.find(".org-src-container").codeblock("text"));
            }
            else {
                reject("no such module " + path);
            }
        }
        else {
            reject("unparsable module path " + url.address);
        }
    });
};
