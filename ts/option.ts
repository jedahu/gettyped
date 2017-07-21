import {some, none, Option} from "fp-ts/lib/Option";

export const guard = <A>(test : boolean, a : () => A) : Option<A> =>
    test ? some(a()) : none;

export const guardCtor = <A>(ctor : {new(..._:any[]):A}, x : any) : Option<A> =>
    guard(x instanceof ctor, x);

export const whenSome = <A>(f : (a:A) => void, oa : Option<A>) =>
    oa.fold(() => {}, f);

export * from "fp-ts/lib/Option";
