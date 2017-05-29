import * as Func from "./func";

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

export const done = Func.absurd;
