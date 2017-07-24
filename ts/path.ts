import {IsRefined} from "./refined";
import {Refined} from "./refined";
import {isRefined} from "./refined";
import {liftUnsafe} from "./refined";
import {lift} from "./refined";
import {refine} from "./refined";
import {tag} from "./refined";

export const enum _Path {}
export const enum _TsFile {}
export const enum _TsModule {}
export const enum _Abs {}

export type Path<R = {}> = Refined<string, R & _Path>;

export const isTsFile : IsRefined<string, _Path & _TsFile> =
    isRefined("TsFile", p => p.endsWith(".ts"));

export const isTsModule : IsRefined<string, _Path & _TsModule> =
    isRefined("TsModule", p => !p.endsWith(".ts"));

export const isAbs : IsRefined<string, _Path & _Abs> =
    isRefined("Abs", p => p.startsWith("/"))

export const path = tag<string, _Path>();

export const tsFilePath = refine(isTsFile);

export const tsModulePath = refine(isTsModule);

export const absPath = refine(isAbs);

export const join = (p : Path, ...ps : Array<Path>) : Path => {
    let out : string = p;
    for (const p of ps) {
        if (isAbs(p)) {
            out = p;
        }
        else {
            const a = out.endsWith("/") ? out.slice(0, -1) : out;
            const b = p.startsWith("./") ? p.slice(2) : p;
            out = `${a}/${b}`;
        }
    }
    return path(lift(out));
};

export const absJoin = (p : Path<_Abs>, ...ps : Array<Path>) : Path<_Abs> =>
    absPath(join(p, ...ps));

export const trimExt = <A extends string>(p : Path, ext : A) : Path =>
    path(lift(p.substring(0, p.length - ext.length)));

export const removeTs =
    <R>(p : Path<R & _TsFile>) : Path<R & _TsModule> =>
    tsModulePath(liftUnsafe<string, R>(trimExt(p, ".ts")));

export const addTs = <R>(p : Path<_TsModule & R>) : Path<_TsFile & R> =>
    tsFilePath(liftUnsafe<string, R>(`${p}.ts`));

export const ensureNoTs = <R>(p : Path<R> | Path<R>) : Path<R> =>
    isTsFile(p)
    ? removeTs(p)
    : tsModulePath(p);

export const ensureTs =
    <R>(p : Path<R> | Path<R & _TsModule>) : Path<R & _TsFile> =>
    isTsModule(p)
    ? addTs(p)
    : tsFilePath(p);

export const lastSegment = (path : Path) : string => {
    const segs = path.split("/");
    return segs[segs.length - 1];
};

export const normaliseTsPath =
    (cwd : Path<_Abs>, p : Path) : Path<_TsFile & _Abs> =>
    isAbs(p)
    ? ensureTs(p)
    : ensureTs(liftUnsafe<string, _Path & _Abs>(join(cwd, p)));
