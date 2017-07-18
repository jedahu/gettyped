declare module "*.json" {
    const x : any;
    export default x;
}

declare module "object.values" {
    const x : any;
    export = x;
}

declare module "object.entries" {
    const x : any;
    export = x;
}

declare module "vs/language/typescript/lib/typescriptServices" {
    const x : typeof ts;
    export = x;
}

declare module "vex-js" {
    const x : any;
    export = x;
}

declare module "vex-dialog" {
    const x : any; 
    export = x;
}

type RequestIdleCallbackFn =
    (f : (deadline : {timeRemaining() : number}) => void) => void;

declare interface Window {
    requirejs : Require;
    define : RequireDefine;
    requestIdleCallback : RequestIdleCallbackFn;
}

declare const requestIdleCallback : RequestIdleCallbackFn;
