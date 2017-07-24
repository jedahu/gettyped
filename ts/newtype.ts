import {Proxy} from "./proxy";

type Predicate<A> = (a : A) => boolean;

const enum NominalTag {}

export type Newtype<A, T> = {
    "@nominal" : NominalTag;
    "@tag" : T;
};

export const newtype =
    <A, T>(p : Predicate<A>, _t : Proxy<T>) =>
    (a : A) : Newtype<A, T> => {
        if (p(a)) {
            return a as any;
        }
        throw new Error(`Newtype error: ${p.name}: ${a}.`);
    };

export const oldtype = <A, T>(nt : Newtype<A, T>) : A => nt as any;
