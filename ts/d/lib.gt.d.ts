declare type $GT = Readonly<{
    assert : (x : boolean, msg? : string) => void;

    assertp : (p : Promise<boolean>, msg? : string) => Promise<void>;

    log : (...xs : Array<any>) => void;

    randomFloat : (min : number, max : number) => number;

    randomInt : (min : number, max : number) => number;

    canvas : <A>(
        size : number | [number, number],
        f : (ctx : CanvasRenderingContext2D) => A
    ) => A;

    prompt : (
        message : string,
        placeholder? : string,
        defaultValue? : string
    ) => Promise<string | undefined>;

    animals : ReadonlyArray<string>;
}>;

declare const $gt : $GT;
