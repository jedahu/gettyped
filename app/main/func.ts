export const absurd = <A>(_ : never) : A => {
    throw new Error();
};

export const map = <A, B, C>(g : (_:A) => B, f : (_:B) => C) : ((_:A) => C) =>
    a => f(g(a));

export const contramap = <A, B, C>(g : (_:A) => B, f : (_:C) => A) : ((_:C) => B) =>
    c => g(f(c));
