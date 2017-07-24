import {Proxy} from "./proxy";

const enum NominalTag {}

export type Tag<A> = {
    "@nominal" : NominalTag;
    "@tag" : A;
};

export type Tagged<A, T> = A & Tag<T>;

export const tag = <A, T>(a : A, _t : Proxy<T> = {}) : Tagged<A, T> =>
    a as Tagged<A, T>;
