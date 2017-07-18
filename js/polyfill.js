import * as values from "object.values";
import * as entries from "object.entries";
import "whatwg-fetch";

if (!Object.values) {
    values.shim();
}

if (!Object.entries) {
    entries.shim();
}
