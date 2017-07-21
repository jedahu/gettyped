export const assertNever = (a : never) : never => {
    throw new Error(`Expected never, got ${a}`);
};

export const inIdleTime =
    (task : () => Iterator<void>) : Promise<void> =>
    new Promise((res, rej) => {
        const iter = task();
        requestIdleCallback(deadline => {
            let done = false;
            try {
                while (deadline.timeRemaining() > 0 && !done) {
                    done = iter.next().done;
                }
                done ? res() : res(inIdleTime(() => iter));
            }
            catch (e) {
                rej(e);
            }
        });
    });
