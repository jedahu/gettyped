export const when =
    <A, B, C>(
        x : A | B,
        test : (x : A | B) => x is A,
        f : (_:A) => C,
        g : (_:B) => C
    ) : C =>
    test(x) ? f(x) : g(x);

export const or =
    <A, B, C>(
        test : (x : A | B) => x is A,
        f : (_:A) => C,
        g : (_:B) => C
    ) : ((_ : A | B) => C) =>
    x => test(x) ? f(x) : g(x);

export interface Cases<A = undefined, B = undefined, C = undefined> {
}

export type Case<T extends keyof Cases<A, B, C>, A = undefined, B = undefined, C = undefined> =
    {_tag : T; _val : Cases<A, B, C>[T]};

export const isCase =
    <Z, T extends keyof Cases<A, B, C>, A = undefined, B = undefined, C = undefined>(tag : T, x : Case<T, A, B, C> | Z) : x is Case<T, A, B, C> =>
    x.hasOwnProperty("_tag") && (x as any)._tag === tag;

// type Cases<A> = {
//     [T in keyof A] : Case<T, A[T]>;
// };

// type Sum<A> = Cases<A>[keyof A];

export const mk  =
    <T extends keyof Cases<A, B, C>, A = undefined, B = undefined, C = undefined>(tag : T, val : Cases<A, B, C>[T]) : Case<T, A, B, C> =>
    ({_tag: tag, _val: val});

export const ctor  =
    <T extends keyof Cases<A, B, C>, A = undefined, B = undefined, C = undefined>(tag : T) => (val : Cases<A, B, C>[T]) : Case<T, A, B, C> =>
    ({_tag: tag, _val: val});

export const done = (_ : never) : any => {
    throw new Error("Cases should be exhausted.");
};

// export const mkSum = <C, S extends Sum<C>>() => ({
//     ctor: <T extends keyof C>(tag : T) => (val : C[T]) : Case<T, C[T]> => ({_tag: tag, _val: val}),
//     mk: <T extends keyof C>(tag : T, val : C[T]) : Case<T, C[T]> => ({_tag: tag, _val: val}),
//     fold: <A>(x : S, fs : CaseFold<C, A>) : A => fs[x._tag](x._val)
//     });
