# collect-errors

`CollectErrors es t` is a monad wrapper around values of type `t` which can accommodate (a list of)
(potential) errors of type `es` that have (maybe) occurred during the computation
of a value.  A value may be missing, leaving only the error(s).

The wrapper `CN t` is a special case of `CollectErrors es t` with `es` = `NumErrors`.

## Generated API documentation

See the [Hackage page](https://hackage.haskell.org/package/collect-errors).

## Feature highlights

### Error collecting

The `CN` wrapper propagates instances of `Floating`,
allowing us to write expressions with partial
functions (ie functions that fail for some inputs) instead of
branching after each application of such function:

    $ stack ghci collect-errors:lib --no-load --ghci-options Numeric.CollectErrors
    *Numeric.CollectErrors> a = 1 :: CN Double
    *Numeric.CollectErrors> (1/(a-1))+(sqrt (a-2))
    {{ERROR: division by 0; ERROR: out of domain: sqrt for negative arg -1.0}}

as opposed to:

    *Prelude> a = 1 :: Double
    *Prelude> (1/(a-1))+(sqrt (a-2))
    NaN

### Error investigation

Dealing with the errors can be moved outside the expression:

    *Numeric.CollectErrors> a = 1 :: CN Double
    *Numeric.CollectErrors> toEither $ 1/(a-1)
    Left {ERROR: division by 0}

    *Numeric.CollectErrors> toEither $ 1/a+(sqrt a)
    Right 2.0

An alternative way to branch based on errors is provided by the function `withErrorOrValue`:

    ...> a = 2 :: CN Double
    ...> withErrorOrValue (const 0) id (1/a)
    0.5

The `CN` wrapper can be forcibly removed as follows:

    ...> :t unCN (1/a)
    ... :: Double

    ...> unCN (1/a)
    0.5

    ...> unCN (1/(a-2))
    *** Exception: CollectErrors: {ERROR: division by 0}

### Undecided comparisons

The following examples require the interval arithmetic package [aern2-mp](https://github.com/michalkonecny/aern2) and its dependency [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num):

    $ stack ghci aern2-mp:lib --no-load --ghci-options AERN2.MP
    *AERN2.MP> import MixedTypesNumPrelude
    *AERN2.MP MixedTypesNumPrelude>

Comparisons involving sets (such as intervals) are undecided when the intervals overlap:

    ...> pi100 = piBallP (prec 100)
    ...> :t pi100
    pi100 :: MPBall
    ...> pi100
    [3.1415926535897932384626433832793333156439620213... ± ~7.8886e-31 ~2^(-100)]

    ...> 0 < pi100
    CertainTrue

    ...> pi100 == pi100
    TrueOrFalse

The values `CertainTrue` and `TrueOrFalse` are part of the three-valued type `Kleenean` provided by  
[mixed-types-num](https://hackage.haskell.org/package/mixed-types-num).

The above equality cannot be decided since `pi100` is not a single number but a set of numbers spanning the interval and the comparison operator cannot tell if the two operands sets represent the same number or a different number.

The Prelude `Floating` instance for `CN` cannot be used reliably with interval arithmetic because the instance relies on true/false comparisons:

    ...> import qualified Prelude as P
    ... P> (cn pi100) P./ (cn pi100 - cn pi100)
    *** Exception: Failed to decide equality of MPBalls.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of MPBalls returns Kleenean instead of Bool.

Using its Kleenean comparisons, package [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num) provides alternative numerical type classes in which errors are detected and collected correctly when using the `CN` wrapper:

    ...> (cn pi100) / (cn pi100 - cn pi100)
    {{POTENTIAL ERROR: division by 0}}

### Potential errors

As we see in the above example, the `CN` wrapper supports potential errors that sometimes arise as a consequence of undecided comparisons.

When an error is present (which can be checked using `hasError`), the function `hasCertainError` can be used to further distinguish cases where the error is certain or potential:

    ...> import qualified Numeric.CollectErrors as CN
    ...> CN.hasCertainError $ (cn pi100) / (cn 0)
    True

    ...> CN.hasCertainError $ (cn pi100) / (cn pi100 - cn pi100)
    False

Sometimes the potential errors are *harmless*:

    ...> sqrt (cn pi100 - cn pi100)
    [0.0000000000000006280369834735100420368561502297... ± ~6.2804e-16 ~2^(-50)]{{POTENTIAL ERROR: out of domain: negative sqrt argument}}

Such harmless potential errors can be ignored using `clearPotentialErrors`:

    ...> clearPotentialErrors $ sqrt (cn pi100 - cn pi100)
    [0.0000000000000006280369834735100420368561502297... ± ~6.2804e-16 ~2^(-50)]

