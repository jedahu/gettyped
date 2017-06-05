import {History, Location} from "history";
import createHistory from "history/createBrowserHistory";
import * as part from "./part";

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
            update:
                part.Signal.
                     ignore(part.Begin).
                     handle<part.Change<In, {}>>(
                         part.Change,
                         ({val: {prevProps, props}}) => {
                             if (prevProps.path !== props.path) {
                                 history.push(props.path);
                             }
                         }).
                     handle(part.End, unregister)
        };
    });
