declare type $GT = {
    assert : (x : boolean, msg? : string) => void;
    assertp : (p : Promise<boolean>, msg? : string) => Promise<void>;
    log : (...xs : Array<any>) => void;
    canvas : (
        size : number | [number, number],
        f : (ctx : CanvasRenderingContext2D) => void
    ) => void;
};

declare const $gt : $GT;
