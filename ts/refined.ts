export abstract class Refinement<A> {
    abstract test : (a : A) => boolean;
};

type Ctor<A> = {new() : A};

type Guard = {
    <A,
    T extends Refinement<A>
    >(
        a : A,
        t : Ctor<T>
    ) : a is Refined<A, T>;

    <A,
    T extends Refinement<A>,
    U extends Refinement<A>
    >(
        a : A,
        t : Ctor<T>,
        u : Ctor<U>
    ) : a is Refined<A, T & U>;

    <A,
    T extends Refinement<A>,
    U extends Refinement<A>,
    V extends Refinement<A>
    >(
        a : A,
        t : Ctor<T>,
        u : Ctor<U>,
        v : Ctor<V>
    ) : a is Refined<A, T & U & V>;
};

type GuardSum = {
    <A,
    R,
    T extends Refinement<A>
    >(
        a : Refined<A, R>,
        t : Ctor<T>
    ) : a is Refined<A, R & T>;

    <A,
    R,
    T extends Refinement<A>,
    U extends Refinement<A>
    >(
        a : Refined<A, R>,
        t : Ctor<T>,
        u : Ctor<U>
    ) : a is Refined<A, R & T & U>;

    <A,
    R,
    T extends Refinement<A>,
    U extends Refinement<A>,
    V extends Refinement<A>
    >(
        a : Refined<A, R>,
        t : Ctor<T>,
        u : Ctor<U>,
        v : Ctor<V>
    ) : a is Refined<A, R & T & U & V>;
};

type Unit = {
    <A,
    T extends Refinement<A>
    >(
        a : A,
        t : Ctor<T>
    ) : Refined<A, T>;

    <A,
    T extends Refinement<A>,
    U extends Refinement<A>
    >(
        a : A,
        t : Ctor<T>,
        u : Ctor<U>
    ) : Refined<A, T & U>;

    <A,
    T extends Refinement<A>,
    U extends Refinement<A>,
    V extends Refinement<A>
    >(
        a : A,
        t : Ctor<T>,
        u : Ctor<U>,
        v : Ctor<V>
    ) : Refined<A, T & U & V>;
};

type UnitSum = {
    <A,
    R,
    T extends Refinement<A>
    >(
        a : Refined<A, R>,
        t : Ctor<T>
    ) : Refined<A, R & T>;

    <A,
    R,
    T extends Refinement<A>,
    U extends Refinement<A>
    >(
        a : Refined<A, R>,
        t : Ctor<T>,
        u : Ctor<U>
    ) : Refined<A, R & T & U>;

    <A,
    R,
    T extends Refinement<A>,
    U extends Refinement<A>,
    V extends Refinement<A>
    >(
        a : Refined<A, R>,
        t : Ctor<T>,
        u : Ctor<U>,
        v : Ctor<V>
    ) : Refined<A, R & T & U & V>;
};

const guardImpl =
    <A>(a : A, ...xs : Array<Ctor<Refinement<A>>>) : a is Refined<A, any> => {
        for (const x of xs) {
            if (!new x().test(a)) return false;
        }
        return true;
    };

export const guard : Guard = guardImpl;

export const guardSum : GuardSum = guardImpl;

export const lift : Unit =
    <A>(a : A, ...xs : Array<Ctor<Refinement<A>>>) : Refined<A, any> => {
        if (guardImpl(a, ...xs)) {
            return a;
        }
        throw new Error(`Refinement error: ${xs}: ${a}.`);
    };

export const liftSum : UnitSum =
    <A, R extends Refinement<A>>(
        a : Refined<A, R>,
        ...xs : Array<Ctor<Refinement<A>>>
    ) : Refined<A, R> => {
        if (guardImpl(a, ...xs)) {
            return a;
        }
        throw new Error(`Refinement error: ${xs}: ${a}.`);
    };

const enum _Nil {}

export class Nil extends Refinement<any> {
    "@nominal" : _Nil;
    test : (a : any) => true;
}

const enum _Refined {}

export type Refined<A, T> = {
    "@nominal" : _Refined;
    "@tag" : T & Nil;
} & A;

export const liftUnsafe =
    <A, R>(a : A) : Refined<A, R> => a as Refined<A, R>;
