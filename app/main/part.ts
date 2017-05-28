import * as React from "react";
import * as adt from "./adt";

declare module "./adt" {
    interface Cases<A, B, C> {
        "Begin-34139498-652b-41ba-976b-40446713d707" : {
            props : A;
            state : B;
        };

        "Change-2b657919-b403-4248-a4a0-e29afebcb8a7" : {
            prevProps : A;
            props : A;
            prevState : B;
            state : B
        };

        "End-17bc5f8f-83ac-4abd-aa82-aefd9b791065" : {
            props : A;
            state : B;
        };
    }
}

export const Begin = "Begin-34139498-652b-41ba-976b-40446713d707";
export const Change = "Change-2b657919-b403-4248-a4a0-e29afebcb8a7";
export const End = "End-17bc5f8f-83ac-4abd-aa82-aefd9b791065";

export type Signal<A> = (_:A) => void;

export const emit = <A, B>(s : Signal<A>, f : (_:B) => A) : Signal<B> =>
    b => s(f(b));

export const handle =
    <Z, T extends keyof adt.Cases, A = undefined, B = undefined, C = undefined>(
        a : T,
        f : (_:adt.Cases<A, B, C>[T]) => void,
        s : Signal<Z>
    ) : Signal<adt.Case<T, A, B, C> | Z> =>
    x => adt.isCase(a, x)
    ? f(x._val)
    : s(x);

export const react =
    <Z, T extends keyof adt.Cases, A = undefined, B = undefined, C = undefined>(
        a : T,
        f : (_:adt.Cases<A, B, C>[T]) => void,
        s : Signal<adt.Case<T, A, B, C> | Z>
    ) : Signal<adt.Case<T, A, B, C> | Z> =>
    x => {
        if (adt.isCase(a, x)) {
            f(x._val);
        }
        s(x);
    };

type Render<A, I> = (args : {props : A, state : I}) => null | JSX.Element;

type PartEvent<A, I> =
    adt.Case<typeof Begin, A, I> |
    adt.Case<typeof Change, A, I> |
    adt.Case<typeof End, A, I>;

export type Event<A, I> = PartEvent<A, I>;

type Update<A, I> =
    (args : {
        event : PartEvent<A, I>
    }) => void;

type Create<A, I, S> =
    (args : {
        updateState : (f : <K extends keyof I>(s:I, p:A) => Pick<I, K>) => void,
        signal : Signal<S>
    }) => {
        render : Render<A, I>;
        initialState? : (_:A) => I;
        update? : Update<A, I>;
    };

type CProps<A, I, S> = {
    props : A;
    create : Create<A, I, S>;
    signal : Signal<S>;
};

export class ComponentPart<A, I, S> extends React.PureComponent<CProps<A, I, S>, I> {
    readonly componentDidMount : () => void;
    readonly componentDidUpdate : (prevProps : CProps<A, I, S>, prevState : I) => void;
    readonly componentWillUnmount : () => void;
    readonly updateState : (_ : <K extends keyof I>(s : I) => Pick<I, K>) => void;

    constructor(cprops : CProps<A, I, S>) {
        super(cprops);
        this.updateState = this.setState.bind(this);
        const {create, signal} = cprops;
        const {initialState, render, update} = create({
            updateState: f => this.setState((s0, p0) => f(s0, p0.props)),
            signal
        });
        if (typeof initialState !== "undefined") {
            this.state = initialState(cprops.props);
        }
        this.render = () => render({
            props: this.props.props,
            state: this.state
        });

        this.componentDidMount = () => {
            if (update) {
                update({
                    event: adt.mk<typeof Begin, A, I>(Begin, {
                        props: this.props.props,
                        state: this.state
                    })
                });
            }
        };

        this.componentDidUpdate = (prevProps, prevState) => {
            if (update) {
                update({
                    event: adt.mk<typeof Change, A, I>(Change, {
                        prevProps: prevProps.props,
                        prevState,
                        props: this.props.props,
                        state: this.state
                    })
                });
            }
        };

        this.componentWillUnmount = () => {
            if (update) {
                update({
                    event: adt.mk<typeof End, A, I>(End, {
                        props: this.props.props,
                        state: this.state
                    })
                });
            }
        };
    }
}

export type Part<A, S> = (signal : Signal<S>) => (props : A) => JSX.Element;

export const mkPart =
    <A, I, S>(create : Create<A, I, S>) : Part<A, S> =>
    (signal : Signal<S>) =>
    (props : A) : JSX.Element =>
    React.createElement(ComponentPart, {props, create, signal});

export const mk = mkPart;
