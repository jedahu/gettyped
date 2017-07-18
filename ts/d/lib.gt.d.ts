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

    prompt : (
        message : string,
        placeholder? : string,
        defaultValue? : string
    ) => Promise<string | undefined>;
};

declare const $gt : $GT;
