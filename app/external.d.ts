declare module "painless" {
    export const createGroup : any;
    export const assert : any;
}

declare module "glob-fs" {
    const x : any;
    export = x;
}

declare module "ace" {
    const x : any;
    export = x;
}

declare module "react-monaco-editor" {
    const x : any;
    export default x;
}

declare module "selectivity/react" {
    const x : any;
    export = x;
}

declare module "!!sass-variable-loader!../../scss/_vars.scss" {
    const x : any;
    export default x;
}

declare module "raw-loader!*" {
    const x : string;
    export = x;
}

declare module "worker-loader*" {
    const x : any;
    export = x;
}

declare module "*.txt" {
    const x : string;
    export = x;
}

declare module "*.md" {
    const x : string;
    export = x;
}

declare module "*.json" {
    const x : any;
    export = x;
}

declare module "fluture" {
    type Nodeback<E, A> = (err : E | null, val : A) => void;

    interface Future<E, A> {
        map<B>(f : (_:A) => B) : Future<E, B>;
        bimap<E1, A1>(g : (_:E) => E1, f : (_:A) => A1) : Future<E1, A1>;
        chain<B>(f : (_:A) => Future<E, B>) : Future<E, B>;
        mapRej<E1>(f : (_:E) => E1) : Future<E1, A>;
        fork(rej : (_:E) => void, res : (_:A) => void) : () => void;
        value(res : (_:A) => void) : () => void;
        promise() : Promise<A>;
    }

    interface FutureStatic {
        <E, A>(rej : (_:E) => void, res : (_:A) => void) : Future<E, A>;
        of<A>(a : A) : Future<never, A>;
        reject<A>(a : A) : Future<A, never>;
        cache<E, A>(fa : Future<E, A>) : Future<E, A>;
        node<E, A>(k : (_:Nodeback<E, A>) => void) : Future<E, A>;
        tryP<E, A>(p : () => Promise<A>) : Future<E, A>;
        encaseN<A0, E, A>(k : (a0 : A0, k : Nodeback<E, A>) => void) : Future<E, A>;
        encaseP<A0, E, A>(p : (a0 : A0) => Promise<A>) : (a:A) => Future<E, A>;
        ap<E, A, B>(fab : Future<E, (_:A) => B>, fa : Future<E, A>) : Future<A, B>;
        parallel<E, A>(limit : number, fs : Array<Future<E, A>>) : Future<E, Array<A>>;
    }

    const Future : FutureStatic;

    export = Future;
}
