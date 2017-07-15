export const assert = (x : boolean, msg? : string) : void => {
    if (!x) {
        throw new Error("Assertion failed. " + (msg || ""));
    }
};

export const assertp =
    async (p : Promise<boolean>, msg? : string) : Promise<void> =>
    assert(await p, msg);

export const randomFloat = (min : number, max : number) : number =>
    Math.random() * (max - min) + min;

export const randomInt = (min : number, max : number) : number => {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min + 1)) + min;
};
