export type Tag<A> = {
    "@tag" : A;
};

export type Tagged<A, T> = A & Tag<T>;
