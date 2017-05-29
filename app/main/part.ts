import * as React from "react";
import * as Func from "./func";

type Begin_<A, I> = {
    props : A;
    state : I;
};

export class Begin<A, I> {
    [Symbol.species] : "0f0d8cc7-c860-4a00-a724-f523873a1fb9";
    readonly props : A;
    readonly state : I;

    constructor(args : Begin_<A, I>) {
        Object.assign(this, args);
    }

    static mk<A, I>(args : Begin_<A, I>) {
        return new Begin<A, I>(args);
    }

    static is<A, I, Z>(x : Begin<A, I> | Z) : x is Begin<A, I> {
        return x instanceof Begin;
    }
}

type Change_<A, I> = {
    prevProps : A;
    props: A;
    prevState: I;
    state: I;
};

export class Change<A, I> {
    [Symbol.species] : "5cb63142-0d65-4f3f-b9f6-5ee164d9c1de";
    readonly prevProps : A;
    readonly props : A;
    readonly prevState : I;
    readonly state : I;

    constructor(args : Change_<A, I>) {
        Object.assign(this, args);
    }

    static mk<A, I>(args : Change_<A, I>) {
        return new Change<A, I>(args);
    }

    static is<A, I, Z>(x : Change<A, I> | Z) : x is Change<A, I> {
        return x instanceof Change;
    }
}

type End_<A, I> = {
    props : A;
    state : I;
};

export class End<A, I> {
    [Symbol.species] : "56a8806f-5fc5-470d-859e-9ae025364489";
    readonly props : A;
    readonly state : I;

    constructor(args : End_<A, I>) {
        Object.assign(this, args);
    }

    static mk<A, I>(args : End_<A, I>) {
        return new End<A, I>(args);
    }

    static is<A, I, Z>(x : End<A, I> | Z) : x is End<A, I> {
        return x instanceof End;
    }
}

export class Signal<A> {
    [Symbol.species]: "f1433f30-848f-4c64-a2c4-92e8bcf176ad";

    private constructor(readonly run : (_:A) => void) {}

    static mk<A>(run : (_:A) => void) : Signal<A> {
        return new Signal<A>(run);
    }

    static readonly none : Signal<never> =
        new Signal<never>(_ => {});

    static handle<A>(
        test : (x : A|never) => x is A,
        f : (_:A) => void
    ) : Signal<A> {
        return Signal.none.handle(test, f);
    }

    static ignore<A>(test : (x : A|never) => x is A) : Signal<A> {
        return Signal.none.ignore(test);
    }

    handle<B>(
        test : (x : A|B) => x is B,
        f : (_:B) => void
    ) : Signal<A | B> {
        return new Signal<A | B>(ab => test(ab) ? f(ab) : this.run(ab));
    }

    ignore<B>(test : (x : A|B) => x is B) : Signal<A | B> {
        return this.handle(test, _ => {});
    }

    emit<B>(f : (_:B) => A) : Signal<B>["run"] {
        return Func.contramap(this.run, f);
    }
}

type Render<A, I> = (args : {props : A, state : I}) => null | JSX.Element;

type PartEvent<A, I> = Begin<A, I> | Change<A, I> | End<A, I>;

export type Event<A, I> = PartEvent<A, I>;

type Update<A, I> = Signal<PartEvent<A, I>>;

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
                update.run(
                    Begin.mk({
                        props: this.props.props,
                        state: this.state
                    }));
            }
        };

        this.componentDidUpdate = (prevProps, prevState) => {
            if (update) {
                update.run(
                    Change.mk({
                        prevProps: prevProps.props,
                        prevState,
                        props: this.props.props,
                        state: this.state
                    })
);
            }
        };

        this.componentWillUnmount = () => {
            if (update) {
                update.run(
                    End.mk({
                        props: this.props.props,
                        state: this.state
                    })
);
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
