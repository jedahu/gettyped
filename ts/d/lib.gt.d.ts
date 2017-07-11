declare type $GT = {
    assert : (x : boolean, msg? : string) => void;
    assertp : (p : Promise<boolean>, msg? : string) => Promise<void>;
    log : (...xs : Array<any>) => void;
    withCanvas : (
        f : (ctx : CanvasRenderingContext2D) => void,
        width? : number,
        height? : number
    ) => void;
};

declare const $gt : $GT;
