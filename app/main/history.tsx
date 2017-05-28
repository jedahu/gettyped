import {History, Location} from "history";
import createHistory from "history/createBrowserHistory";
import * as adt from "./adt";
import * as part from "./part";
import * as Future from "fluture";

const history : History = createHistory();

declare module "./adt" {
    interface Cases<A, B, C> {
        "LocationChange-cbf24f58-e5e0-45bb-a95d-04fe40135fa1" : Location;
    }
}

export const LocationChange = "LocationChange-cbf24f58-e5e0-45bb-a95d-04fe40135fa1";

export type Out =
    adt.Case<typeof LocationChange>;

type In = {
    path : string;
};

export const mk = part.mk<In, {}, Out>(
    ({signal}) => {
        const unregister =
            history.listen(part.emit(signal, adt.ctor(LocationChange)));
        return {
            render: ({}) => null,
            update: ({event}) => {
                if (event._tag === part.End) {
                    unregister();
                    return Future.of((s : {}) => s);
                }
                if (event._tag === part.Change) {
                    const {prevProps, props} = event._val;
                    if (prevProps.path !== props.path) {
                        history.push(props.path);
                    }
                    return Future.of((s : {}) => s);
                }
                return Future.of((s : {}) => s);
            }
        };
    });
