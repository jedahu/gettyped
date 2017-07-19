export const delay = (time : number = 0) : Promise<void> =>
    new Promise(res => setTimeout(() => { res(); }));

export type CancelToken<A> = {
    canceled : A | undefined;
};

export class CancelSignal extends Error {
    constructor(message? : string) {
        super(message || "Task canceled");
        Object.setPrototypeOf(this, CancelSignal.prototype);
    }
}

export class TimeoutSignal extends CancelSignal {
    constructor(message? : string) {
        super(message || "Task timed out");
    }
}

export const rejectIfTimedOut =
    <A>(token : CancelToken<A | TimeoutSignal>) => {
        if (token.canceled instanceof TimeoutSignal) {
            throw token.canceled;
        }
    };

export const rejectIfCanceled =
    <A>(token : CancelToken<A | CancelSignal>) => {
        if (token.canceled instanceof CancelSignal) {
            throw token.canceled;
        }
    };

export type TimeoutOpts<A> = {
    timeout : number;
    tries : number;
    cancelToken? : CancelToken<A>;
};

const mkCancelToken = () => ({canceled: undefined});

export const withTimeout =
    <A, C>(
        task : (c : CancelToken<C | TimeoutSignal>) => Promise<A>,
        opts : TimeoutOpts<C | TimeoutSignal>
    ) : Promise<A> =>
    new Promise((res, rej) => {
        let fulfilled = false;
        const go = (tries : number) => {
            const cancelToken = opts.cancelToken || mkCancelToken();
            task(cancelToken).then(a => {
                fulfilled = true;
                res(a);
            });
            setTimeout(() => {
                if (!fulfilled) {
                    const signal =
                        new TimeoutSignal(
                            `Task timed out after ${opts.timeout}ms` +
                                ` on try ${opts.tries}.`);
                    cancelToken.canceled = signal;
                    if (tries < opts.tries) {
                        go(tries + 1);
                    }
                    else {
                        rej(signal);
                    }
                }
            }, opts.timeout);
        };
        go(0);
    });
