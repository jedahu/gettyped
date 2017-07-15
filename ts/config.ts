type Config = {
    tsconfig : any;
    config : {
        siteRoot : string;
        scrollbarSize : number;
    };
};

const gt : Config = (window as any).__gt;

export const siteRoot : string = gt.config.siteRoot;

export const scrollbarSize : number = gt.config.scrollbarSize;

export const tsconfig : any = gt.tsconfig;
