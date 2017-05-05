import {createGroup, assert} from "painless";
import * as glob from "glob-fs";

const test = createGroup();

const rePaths = glob().readdirSync("out/demo-re/**/*.js");
const demoPaths = glob().readdirSync("out/demo/**/*.js");

rePaths.forEach(
    (p : string) =>
        test("Will fail at runtime: " + p, () => {
            const m = require(p.substring(4));
            assert.throws(m.__eval);
        }));

demoPaths.forEach(
    (p : string) =>
        test("Will succeed at runtime: " + p, () => {
            const m = require(p.substring(4));
            if (m.__eval) {
                assert.doesNotThrow(m.__eval);
            }
        }));
