import * as RT from "./refined";
import {Nil} from "./refined";
import {Refined} from "./refined";
import {Refinement} from "./refined";
import {liftUnsafe} from "./refined";

const enum _Path {}
const enum _TsFile {}
const enum _TsModule {}
const enum _Abs {}

export class FilePath extends Refinement<string> {
    "@nominal" : _Path;
    test = (a : string) => true;
}

export class TsFile extends Refinement<string> {
    "@nominal" : _TsFile
    test = (a : string) => a.endsWith(".ts");
}

export class TsModule extends Refinement<string> {
    "@nominal" : _TsModule;
    test = (a : string) => !a.endsWith(".ts");
}

export class Abs extends Refinement<string> {
    "@nominal" : _Abs;
    test = (a : string) => a.startsWith("/");
}

export type Path<A = Nil> = Refined<string, FilePath & A>;

export const join = (p : Path, ...ps : Array<Path>) : Path => {
    let out : string = p;
    for (const p of ps) {
        if (RT.guard(p, Abs)) {
            out = p;
        }
        else {
            const a = out.endsWith("/") ? out.slice(0, -1) : out;
            const b = p.startsWith("./") ? p.slice(2) : p;
            out = `${a}/${b}`;
        }
    }
    return RT.lift(out, FilePath);
};

export const absJoin = (p : Path<Abs>, ...ps : Array<Path>) : Path<Abs> =>
    RT.liftSum(join(p, ...ps), Abs);

export const trimExt = <A extends string>(p : Path, ext : A) : Path =>
    RT.lift(p.substring(0, p.length - ext.length), FilePath);

export const removeTs =
    <R>(p : Path<TsFile & R>) : Path<R> =>
    RT.liftSum(liftUnsafe<string, R>(trimExt(p, ".ts")), FilePath);

export const addTs = <R>(p : Path<R> | Path<TsModule & R>) : Path<TsFile & R> =>
    RT.liftSum(liftUnsafe<string, R>(`${p}.ts`), FilePath, TsFile);

export const ensureNoTs = <R>(p : Path<R> | Path<TsModule & R>) : Path<R> =>
    RT.guardSum(p, TsFile)
    ? removeTs(p)
    : p;

export const ensureTs =
    <R>(p : Path<R> | Path<TsModule & R>) : Path<TsFile & R> =>
    RT.guardSum(p, TsFile)
    ? p
    : addTs(p);

export const lastSegment = (path : Path) : string => {
    const segs = path.split("/");
    return segs[segs.length - 1];
};

export const normaliseTsPath =
    (cwd : Path<Abs>, p : Path) : Path<TsFile & Abs> =>
    RT.guardSum(p, Abs)
    ? ensureTs(p)
    : ensureTs(absJoin(cwd, p));
