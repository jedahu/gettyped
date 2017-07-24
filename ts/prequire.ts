import {Path} from "./path";
import {_Abs} from "./path";
import {_TsModule} from "./path";

const resetRequireError = () => {
    requirejs.onError = (e : any) => { throw e; };
};

export const prequire = async (paths : Array<Path<_Abs & _TsModule>>) : Promise<any> =>
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

export const unrequire = (path : Path<_Abs & _TsModule>) : void =>
    requirejs.undef(path);
