# Maybe – null done right

## Motivation

-   Handle optional values without guessing and without exceptions
-   Maintain type invariants in smart constructors

`Maybe` guarantees exception free and guess free optional value handling by
providing only one function for getting at the optional value, a function that
requires function arguments that handle both present and absent states.

## Info

type
: `Maybe<A>`

impl
: [Haskell][]([src][Haskell-src]),
  [Rust][]([src][Rust-src]),
  [F#][]([src][F#-src])

docs
: [Haskell wiki](https://wiki.haskell.org/Maybe),
  [F# post](https://fsharpforfunandprofit.com/posts/the-option-type/),
  [Wikipedia](https://en.wikipedia.org/wiki/Option_type)
  
[Haskell]: https://hackage.haskell.org/package/base/docs/Data-Maybe.html
[Haskell-src]: https://hackage.haskell.org/package/base/docs/src/Data.Maybe.html
[Rust]: https://doc.rust-lang.org/std/option/
[Rust-src]: https://doc.rust-lang.org/src/core/up/src/libcore/option.rs.html
[F#]: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/core.option-module-%5Bfsharp%5D
[F#-src]: https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSharp.Core/option.fs

## Minimal API

-   A nullary constructor
-   A 1-ary constructor
-   A function that folds over the constructor cases

``` {.ts}
class Maybe<A> {
    static nothing : Maybe<A>;

    static just(a : A) : Maybe<A>;

    fold<B>(nothing : () => B, just : (A) => B) : B;
}
```

This API provides no way to dereference the Maybe-wrapped value directly. `fold`
ensures that the `Nothing` case is handled.

## Extended API

``` {lang="ts"}
or(other : () => Maybe<A>) : Maybe<A>;

valueOr(other : () => A) : A;

map<B>(f : (a:A) => B) : Maybe<B>;

flatMap<B>(f : (a:A) => Maybe<B>) : Maybe<B>;

zip<B>(other : Maybe<B>) : Maybe<[A, B]>;

justAct(f : (a:A) => void) : void;

static catchError<A>(f : () => A) : Maybe<A>;

static when<A>(test : boolean, val : () => A) : Maybe<A>;

static unless<A>(test : boolean, val : () => A) : Maybe<A>;
```

## Implementation

``` {named lang="ts" module="demo/data/maybe/minimal"}
```

## Module

``` {named lang="ts" module="demo/data/maybe/extended"}
```

## Maybe vs null

Assuming the following values:

``` {named lang="ts" module="demo/data/maybe/null"}
```

A typical null-check,

``` {lang="ts" module="demo/data/maybe/null/typicalCheck"}
```

exhibits a lack of type safety in two ways:

**Assumption.** The programmer assumes `n` is not `null` and doesn’t write the
null check.

``` {lang="ts" module="demo/re/data/maybe/null/notNullAssumption"}
```

**Boolean blindness.** The compiler doesn’t stop accidental dereferencing, as in
this transposition.

``` {lang="ts" module="demo/re/data/maybe/null/accidentalDereference"}
```

Boolean blindness is [well described by Robert Harper][Harper]. Here’s the money
quote:

[Harper]: https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/

> Another harm is the condition of Boolean blindness alluded to earlier.
> Suppose that I evaluate the expression e=e’ to test whether e and e’
> are equal. I have in my hand a bit. The bit itself has no intrinsic
> meaning; I must associate a provenance with that bit in order to give
> it meaning. “This bit being true means that e and e’ are equal,
> whereas this other bit being false means that some other two
> expressions are not equal.” Keeping track of this information (or
> attempting to recover it using any number of program analysis
> techniques) is notoriously difficult. The only thing you can do with a
> bit is to branch on it, and pretty soon you’re lost in a thicket of
> if-the-else’s, and you lose track of what’s what. Evolve the program a
> little, and you’re soon out to sea, and find yourself in need of sat
> solvers to figure out what the hell is going on.

`Maybe` solves both issues.

``` {lang="ts" module="demo/data/maybe/null/solved"}
```

**Assumption.** The programmer cannot get at the value of a `Maybe` without
going through `Maybe.fold`.

``` {lang="ts" module="demo/ce/data/maybe/null/forcedToHandle"}
```

**Boolean blindness.** `Maybe.fold` provides a value only to the `just` case, so
an accidental transposition results in a compile time error:

``` {lang="ts" module="demo/ce/data/maybe/null/transpositionError"}
```
