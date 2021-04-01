# collect-errors

`CollectErrors es t` is a monad wrapper around values of type `t` which can accommodate (a list of)
(potential) errors of type `es` that have (maybe) occurred during the computation
of a value.  A value may be missing, leaving only the error(s).

The wrapper `CN t` is a special case of `CollectErrors es t` with `es` = `NumErrors`.

The `CN` monad also propagates instances of `Floating`,
allowing us to write expressions with partial
functions (ie functions that fail for some inputs) instead of
branching after each application of such function:

    *Numeric.CollectErrors> a = 1 :: CN Double
    *Numeric.CollectErrors> (1/(a-1))+(sqrt (a-2))
    {[(division by 0,ERROR),(out of domain: sqrt for negative arg -1.0,ERROR)]}

as opposed to:

    *Prelude> a = 1 :: Double
    *Prelude> (1/(a-1))+(sqrt (a-2))
    NaN

Dealing with the errors can be moved outside the expression:

    *Numeric.CollectErrors> a = 1 :: CN Double
    *Numeric.CollectErrors> getValueIfNoError $ 1/(a-1)
    Left [(division by 0,ERROR)]

    *Numeric.CollectErrors> getValueIfNoError $ 1/a+(sqrt a)
    Right 2.0

If the error data contain enough information, their list can be used to trace the source of the errors.

The `CN` monad has support for **potential errors** so that it can be applied to a set arithmetic such as interval arithmetic.
The `Floating` instance cannot be used with a set arithmetic since the instance relies on true/false comparisons but a set arithmetic has only three-valued (true/false/undecided) comparisons.
The `mixed-types-num` package provides alternative numerical type classes in which this is possible.
