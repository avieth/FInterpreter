FInterpreter
============

The definitions in this module facilitate the interpretation of free monads
over functor sums using interpreters for free monads over each of the summands.
Follow the [demonstration](Demonstration.lhs) for an exaplanation and
motivating example (also available as a
[gist](https://gist.github.com/avieth/334201aa341d9a00c7fc) which respects
the markdown, but that gist is outdated).

Why use FInterpreter?
---------------------

If you have two or more DSLs and interpreters for each, this package may help
to combine the two into one composite DSL and interpreter. The DSLs must be
given by functors and free monads over them, and the interpreters must be
described by *interpreter functions into some monad transformer*. That last
points means that if `f` is the DSL's functor, then an interpreter is anything
of type `f (m n a) -> m n a` for some monad transformer `m` and monad `n`.
Two examples of this are found in the [demonstration](Demonstration.lhs) by
the names of `modularPlus` and `stdoutLog`.

Relation to extensible-effects
------------------------------

This package is similar to
[extensible-effects](https://hackage.haskell.org/package/extensible-effects).
Paraphrasing
[this comment from /r/haskell](http://www.reddit.com/r/haskell/comments/36ticq/interpreting_free_monads_of_functor_sums/crjzccy), both extensible-effects and FInterpreter are
built around the notion of free monads over functor sums.
The differences? Interpreting a composite DSL in extensible-effects proceeds
by handling/removing effects, one-by-one, until no effects remain, or effects
in one existing monad remain, at which point a value and possibly effects from
an existing monad can be extracted. With FInterpreter, terms of composite
DSLs (free monads over an explicit functor sums) are transformed into a monad
transformer stack, determined by a type annotation. Using type operators, this
looks something like (from the demonstration):

```Haskell
Interpreter
  (LogF String :+: PlusF)
  -- ^ The DSL we're interpreting.
  ((StdoutLogInterpreter :&: ModularPlusInterpreter) IO)
  -- ^ The transformer stack to use; IO Is the base monad.
```

The transformer value is produced by `iterM` from the free monad package, and
the transformer is torn down one-by-one as usual.
