declare type $GT = {
    assert : (x : boolean, msg? : string) => void;

    assertp : (p : Promise<boolean>, msg? : string) => Promise<void>;

    log : (...xs : Array<any>) => void;

    randomFloat : (min : number, max : number) => number;

    randomInt : (min : number, max : number) => number;

    canvas : (
        size : number | [number, number],
        f : (ctx : CanvasRenderingContext2D) => void
    ) => void;
};

declare const $gt : $GT;
