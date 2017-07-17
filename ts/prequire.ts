import {amdRequire} from "./amd";

const resetRequireError = () => {
    amdRequire.onError = (e : any) => { throw e; };
};

export const prequire = async (paths : Array<string>) : Promise<any> =>
    new Promise((res, rej) => {
        amdRequire.onError = rej;
        amdRequire(
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
