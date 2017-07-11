export const objMap = <A>(xs : Array<[string, A]>) : {[key : string] : A} => {
    const omap : {[key : string] : A} = {};
    for (const [k, v] of xs) {
        omap[k] = v;
    }
    return omap;
};

export const objValues = <A>(o : {[s : string] : A}) : Array<A> =>
    Object.keys(o).map(k => o[k]);

export const objEntries = <A>(o : {[s : string] : A}) : Array<[string, A]> =>
    Object.entries(o);
    // Object.keys(o).map(k => [k, o[k]] as [string, A]);

export const arrayFlatMap = <A, B>(xs : Array<A>, f : (a:A) => Array<B>) : Array<B> => {
    let bs : Array<B> = [];
    for (const a of xs) {
        bs = bs.concat(f(a));
    }
    return bs;
};

export const assertNever = (a : never) : never => {
    throw new Error(`Expected never, got ${a}`);
};
