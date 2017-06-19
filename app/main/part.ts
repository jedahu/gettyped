import * as React from "react";
import * as Func from "./func";
import {Val, Variant} from "./adt";

export class Begin<A, I> extends Val<{
    props : A;
    state : I;
},
"50f52568-41d7-4f6d-a04a-25f9bddd4e7d"> {}

export class Change<A, I> extends Val<{
    prevProps : A;
    props : A;
    prevState : I;
    state : I;
},
"5cb63142-0d65-4f3f-b9f6-5ee164d9c1de"> {}

export class End<A, I> extends Val<{
    props : A;
    state : I;
},
"56a8806f-5fc5-470d-859e-9ae025364489"> {}

export class Signal<A> {
    "@nominal": "f1433f30-848f-4c64-a2c4-92e8bcf176ad";

    private constructor(readonly variant : Variant<A, void>) {}

    static readonly none : Signal<never> =
        new Signal<never>(Variant.none<void>());

    static handle<A>(
        ctor : new (..._ : any[]) => A,
        f : (_:A) => void
    ) : Signal<A> {
        return Signal.none.handle(ctor, f);
    }

    static ignore<A>(ctor : new (..._ : any[]) => A) : Signal<A> {
        return Signal.none.ignore(ctor);
    }

    handle<B>(
        ctor : new (..._ : any[]) => B,
        f : (_:B) => void
    ) : Signal<A | B> {
        return new Signal<A | B>(this.variant.when(ctor, f));
    }

    ignore<B>(ctor : new (..._ : any[]) => B) : Signal<A | B> {
        return this.handle(ctor, _ => {});
    }

    emit<B>(f : (_:B) => A) : ((_:B) => void) {
        return Func.contramap(this.variant.run.bind(this.variant), f);
    }

    run(a : A) {
        return this.variant.run(a);
    }
}

type Render<A, I> = (args : {props : A, state : I}) => null | JSX.Element;

type PartEvent<A, I> = Begin<A, I> | Change<A, I> | End<A, I>;

export type Event<A, I> = PartEvent<A, I>;

type Update<A, I> = Signal<PartEvent<A, I>>;

type Create<A, I, S> =
    (args : {
        updateState : (f : (s:I, p:A) => Partial<I>) => void,
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
    readonly _componentDidMount : () => void;
    readonly _componentDidUpdate : (prevProps : CProps<A, I, S>, prevState : I) => void;
    readonly _componentWillUnmount : () => void;
    readonly updateState : (_ : (s : I) => Partial<I>) => void;

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

        this._componentDidMount = () => {
            if (update) {
                update.run(
                    new Begin({
                        props: this.props.props,
                        state: this.state
                    }));
            }
        };

        this._componentDidUpdate = (prevProps, prevState) => {
            if (update) {
                update.run(
                    new Change({
                        prevProps: prevProps.props,
                        prevState,
                        props: this.props.props,
                        state: this.state
                    }));
            }
        };

        this._componentWillUnmount = () => {
            if (update) {
                update.run(
                    new End({
                        props: this.props.props,
                        state: this.state
                    }));
            }
        };
    }

    componentDidMount() {
        this._componentDidMount();
    }

    componentDidUpdate(prevProps : CProps<A, I, S>, prevState : I) {
        this._componentDidUpdate(prevProps, prevState);
    }

    componentWillUnount() {
        this._componentWillUnmount();
    }
}

export type Part<A, S> = (signal : Signal<S>) => (props : A) => JSX.Element;

export const mkPart =
    <A, I, S>(create : Create<A, I, S>) : Part<A, S> =>
    (signal : Signal<S>) =>
    (props : A) : JSX.Element =>
    React.createElement(ComponentPart, {props, create, signal});

export const mk = mkPart;
