import {Tagged} from "./tagged";

const enum _FilePath {}
const enum _ModulePath {}

export type FilePath = Tagged<string, _FilePath>;
export type ModulePath = Tagged<string, _ModulePath>;

export const join = (...paths : Array<string>) : string =>
    paths.
    map(p => p.endsWith("/") ? p.slice(0, -1) : p).
    join("/");

export const unTs = (path : string) : ModulePath =>
    ( path.endsWith(".ts")
      ? path.slice(0, path.length - 3)
      : path
    ) as ModulePath;

export const addTs = (path : string) : FilePath =>
    ( path.endsWith(".ts")
      ? path
      : `${path}.ts`
    ) as FilePath;

export const lastSegment = (path : string) : string => {
    const segs = path.split("/");
    return segs[segs.length - 1];
};

export const normaliseTsPath = (cwd : string, path : string) : FilePath =>
    addTs(
        path.startsWith("/")
        ? path
        : path.startsWith("./")
        ? join(cwd, path.slice(2))
        : join(cwd, path));
