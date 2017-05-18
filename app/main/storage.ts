import {option} from "fp-ts";

type Option<A> = option.Option<A>;
const {fromNullable} = option;

const global = window;

export type Storer<K, A> = {
    key : (a : K, s : Storage) => string;
    unkey : (key : string) => K;
    getKey : (a : A) => K;
    print : (a : A) => string;
    parse : (k : K) => (val : string) => A;
}

export const bless = (storage : Storage) => {
    const set =
        <K, A>(s: Storer<K, A>) =>
            (val: A): void =>
                storage.setItem(
                    s.key(s.getKey(val), storage),
                    s.print(val));

    const get =
        <K, A>(s: Storer<K, A>) =>
            (k: K): Option<A> =>
                fromNullable(storage.getItem(s.key(k, storage))).map(s.parse(k));

    const getset =
        <K, A>(s: Storer<K, A>) =>
            (k: K, generate: () => A): A =>
                get(s)(k).getOrElse(
                    () => {
                        const val = generate();
                        set(s)(val);
                        return val;
                    }
                );

    const del =
        <K, A>(s: Storer<K, A>) =>
            (k: K): void =>
                storage.removeItem(s.key(k, storage));

    return {set, get, getset, del};
};

export const local = bless(global.localStorage);
export const session = bless(global.sessionStorage);
