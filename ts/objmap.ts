import {pair} from "./pair";

export type ObjMap<A> = {[key : string] : A};

export const mk = <A>(xs : Iterable<[string, A]>) : ObjMap<A> => {
    const omap : {[key : string] : A} = {};
    for (const [k, v] of xs) {
        omap[k] = v;
    }
    return omap;
};

export const keyMk =
    <A>(key : (a:A) => string, xs : Iterable<A>) : ObjMap<A> =>
    mk((function*() {
        for (const x of xs) {
            yield pair(key(x), x);
        }
    })());

export const mapValues =
    <A, B>(f : (a:A) => B, amap : ObjMap<A>) : ObjMap<B> => {
        const bmap : ObjMap<B> = {};
        for (const [k, a] of Object.entries(amap)) {
            bmap[k] = f(a);
        }
        return bmap;
    };

export const mapEntries =
    <A, B>(f : (k : string, a : A) => B, amap : ObjMap<A>) : ObjMap<B> => {
        const bmap : ObjMap<B> = {};
        for (const [k, a] of Object.entries(amap)) {
            bmap[k] = f(k, a);
        }
        return bmap;
    };

export const awaitValues =
    async <A>(omap : ObjMap<Promise<A>>) : Promise<ObjMap<A>> => {
        const xs = Object.entries(omap).map(async ([k, a]) => pair(k, await a));
        return mk(await Promise.all(xs));
    };

