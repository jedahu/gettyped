const enum _Refined {}

export type Refined<A, T> = {
    "@nominal" : _Refined;
    "@tag" : T;
} & A;

export type IsRefined<A, T> = {
    // (a : A) : a is Refined<A, T>;
    <R>(a : Refined<A, R>) : a is Refined<A, R & T>;
    rname : string;
};

export const isRefined =
    <A, T>(name : string, test : (a : A) => boolean) : IsRefined<A, T> => {
        const fn = <R>(a : Refined<A, R>) : a is Refined<A, R & T> => test(a);
        (fn as any).rname = name;
        return fn as IsRefined<A, T>;
    };

type DoRefine<A, T> = {
    // (a : A) : Refined<A, T>;
    <R>(a : Refined<A, R>) : Refined<A, R & T>;
}

export const refine =
    <A, T>(p : IsRefined<A, T>) : DoRefine<A, T> =>
    (a : any) : any => {
        if (p(a)) {
            return a;
        }
        throw new Error(`Refinement error: ${p.rname}: ${a}.`);
    };

const tautology : IsRefined<any, any> =
    isRefined("tautology", a => true);

export const tag =
    <A, T>() => <R>(a : Refined<A, R>) : Refined<A, R & T> =>
    refine<A, T>(tautology)(a);

export const liftUnsafe =
    <A, R>(a : A) : Refined<A, R> => a as Refined<A, R>;

export const lift =
    <A>(a : A) : Refined<A, {}> => liftUnsafe<A, {}>(a);
