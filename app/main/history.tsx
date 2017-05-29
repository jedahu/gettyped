import {History, Location} from "history";
import createHistory from "history/createBrowserHistory";
import * as part from "./part";
import * as Future from "fluture";

const history : History = createHistory();

export class LocationChange {
    [Symbol.species] : "0a874b3f-f712-4959-858b-d5901e05dd3b";

    constructor(readonly location : Location) {}

    static mk(location : Location) {
        return new LocationChange(location);
    }

    static is<Z>(x : LocationChange | Z) : x is LocationChange {
        return x instanceof LocationChange;
    }
}

export type Out = LocationChange;

type In = {
    path : string;
};

export const mk = part.mk<In, {}, Out>(
    ({signal}) => {
        const unregister = history.listen(signal.emit(LocationChange.mk));
        return {
            render: ({}) => null,
            update: ({event}) => {
                if (part.End.is(event)) {
                    unregister();
                    return Future.of((s : {}) => s);
                }
                if (part.Change.is(event)) {
                    const {prevProps, props} = event;
                    if (prevProps.path !== props.path) {
                        history.push(props.path);
                    }
                    return Future.of((s : {}) => s);
                }
                return Future.of((s : {}) => s);
            }
        };
    });
