import {Path} from "./path";
import {Abs} from "./path";
import {TsModule} from "./path";

const resetRequireError = () => {
    requirejs.onError = (e : any) => { throw e; };
};

export const prequire = async (paths : Array<Path<Abs & TsModule>>) : Promise<any> =>
    new Promise((res, rej) => {
        requirejs.onError = rej;
        requirejs(
            paths,
            function() {
                res([].slice.call(arguments));
            });
    }).
    then(
    x => {
        resetRequireError();
        return x;
    },
    e => {
        resetRequireError();
        throw e;
    });

export const unrequire = (path : Path<Abs & TsModule>) : void =>
    requirejs.undef(path);
