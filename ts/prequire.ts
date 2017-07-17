const resetRequireError = () => {
    requirejs.onError = (e : any) => { throw e; };
};

export const prequire = async (paths : Array<string>) : Promise<any> =>
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
