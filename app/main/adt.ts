import * as Func from "./func";
import {Map} from "immutable";

export class Val<A, T> {
    "@nominal" : T;
    "@val" : A;

    constructor(readonly val : A) {}

    static ctor<A, T, V extends Val<A, T>>(ctor : new (val : A) => V) {
        return (val : A) => new ctor(val);
    }
}

export class Union<A, B> {
    [Symbol.species] : "41bd8fea-70cf-43cf-b597-63260e5a8691";

    private constructor(readonly run : (_:A) => B) {}

    static mk<A, B>(run : (_:A) => B) {
        return new Union<A, B>(run);
    }

    static none<A>() {
        return new Union<never, A>(done);
    }

    when<C>(
        test : (x : A|C) => x is C,
        f : (_:C) => B
    ) {
        return new Union<A | C, B>(ab => test(ab) ? f(ab) : this.run(ab));
    }

    ignore<C>(test : (x : A|C) => x is C, b : B) {
        return new Union<A | C, B>(ab => test(ab) ? b : this.run(ab));
    }
}

export class Variant<A, X> {
    "@nominal" : "a649fb98-38ef-4925-89e5-8bf245e9a414";

    private constructor(readonly cases : Map<{}, (_:any) => X>) {}

    static none<X>() : Variant<never, X> {
        return new Variant<never, X>(Map<{}, (_:any) => X>());
    }

    static when<A, X>(
        ctor : new (..._ : any[]) => A,
        f : (_:A) => X
    ) : Variant<A, X> {
        return new Variant<A, X>(Map<{}, (_:any) => X>([[ctor, f]]));
    }

    when<B>(
        ctor : new (..._ : any[]) => B,
        f : (_:B) => X
    ) : Variant<A | B, X> {
        return new Variant<A | B, X>(this.cases.set(ctor, f));
    }

    run(a : A) : X {
        return this.cases.get(a.constructor)(a);
    }
}

export const done = Func.absurd;
