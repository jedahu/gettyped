import * as Future from "fluture";

const global = window;

export class Success {
    readonly text : string;

    constructor(s : string) {
        this.text = s;
    }

    static mk(s : string) {
        return new Success(s);
    }
}

export class Failure {
    readonly reason : string;

    constructor(s : string) {
        this.reason = s;
    }

    static mk(s : string) {
        return new Failure(s);
    }
}

export type Result = Success | Failure;

export const fetchText =
    (uri : string) : Future<Failure, Success> =>
    Future.tryP(() => global.fetch(uri)).
    chain(
        r =>
            r.ok
            ? Future.tryP(() => r.text()).map(Success.mk)
            : Future.reject(Failure.mk(`${r.status} ${r.statusText}`)));
