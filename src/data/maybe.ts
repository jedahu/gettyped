export class Maybe<A> {
    static nothing<A>() : Maybe<A> {
        return new Maybe<A>(null);
    }

    static just<A>(a : A) : Maybe<A> {
        return new Maybe({just: a});
    }

    fold<B>(nothing: () => B, just: (a:A) => B): B {
        return this.value === null ? nothing() : just(this.value.just);
    }
    or(other : () => Maybe<A>) : Maybe<A> {
        return this.fold(other, _ => this);
    }
    
    valueOr(other : () => A) : A {
        return this.fold(other, a => a);
    }
    
    map<B>(f : (a:A) => B) : Maybe<B> {
        return this.fold(() => Maybe.nothing<B>(), a => Maybe.just<B>(f(a)));
    }
    
    flatMap<B>(f : (a:A) => Maybe<B>) : Maybe<B> {
        return this.fold(() => Maybe.nothing<B>(), f);
    }
    
    zip<B>(other : Maybe<B>) : Maybe<[A, B]> {
        return this.flatMap(a => other.map(b => <[A, B]>[a, b]));
    }
    
    justAct(f : (a:A) => void) : void {
        this.fold(() => {}, f);
    }
    
    static catchError<A>(f : () => A) : Maybe<A> {
        try {
            return Maybe.just(f());
        }
        catch (_) {
            return Maybe.nothing<A>();
        }
    }
    
    static when<A>(test : boolean, val : () => A) : Maybe<A> {
        return test ? Maybe.just(val()) : Maybe.nothing<A>();
    }
    
    static unless<A>(test : boolean, val : () => A) : Maybe<A> {
        return Maybe.when(!test, val);
    }
    private readonly value : null | {just : A};

    private constructor(value : null | {just : A}) {
        this.value = value;
    }

    protected [Symbol.species] : "f419bf14-c1e9-43f5-92c3-ac944652e72b";
}
