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

declare interface Window {
    require : any;
    __gt : {
        tsconfig : any;
        siteRoot : string;
        scrollbarSize : number;
    };
}
