Interpreting Free Monads of Functor Sums
========================================

This text deals with a way to compose certain kinds of monads, thereby mixing
their capabilities. It is a literate Haskell file, so let's begin with a
bunch of noise.

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE OverlappingInstances #-}
> 
> import Prelude hiding (log)
> import Control.Applicative
> import Control.Monad
> import Control.Monad.Free
> import Control.Monad.IO.Class
> import Control.Monad.Trans.Class
> import Control.Monad.Trans.Identity
> import Control.Monad.Trans.Reader

# A motivating problem

Here are two functors and their free monads which define DSLs for
addition of integers, and for logging.

> data PlusF a where
>    Plus :: Int -> Int -> (Int -> a) -> PlusF a
>
> instance Functor PlusF where
>     fmap f (Plus left right next) = Plus left right (fmap f next)
>
> data LogF s a where
>     Log :: s -> a -> LogF s a
>
> instance Functor (LogF s) where
>     fmap f (Log s next) = Log s (f next)
>
> type Plus = Free PlusF
> type Log s = Free (LogF s)
>
> plus :: Int -> Int -> Plus Int
> plus i j = liftF $ Plus i j id
>
> log :: s -> Log s ()
> log s = liftF $ Log s ()

Either of these free monads can be interpreted using `iterM` and an appropriate
interpreter function.

> type Interpreter f m = forall a . f (m a) -> m a
> type TypeOfIterM = forall f m a . (Functor f, Monad m) => Interpreter f m -> Free f a -> m a

A `Log String` term, for example, can be interpreted by `IO`:

> stdoutLog' :: Interpreter (LogF String) IO
> stdoutLog' term = case term of
>     Log str next -> putStrLn str >> next
>
> runStdoutLog :: Log String t -> IO t
> runStdoutLog = iterM stdoutLog'

Similarly, we could interpet `Plus` using a reader which holds a modulus like
so:

> modularPlus' :: Interpreter PlusF (Reader Int)
> modularPlus' term = case term of
>     Plus left right next -> do
>         modulus <- ask
>         next ((left + right) `mod` modulus)
>         
> runModularPlus :: Plus t -> Reader Int t
> runModularPlus = iterM modularPlus'

But suppose we want to mix `Plus` and `Log String`, so that we can add things
and also log strings. We have interpreters for each of these; it would be nice
to use them to define an interpreter for the composite.
This text shows one way to achieve this.

# A solution

To begin, we need the tools to define the monad composite of `Plus` and `Log`.
The functor sum allows us to build new functors from old, and free monads on
these sums are monads which contain the terms of each summand functor, which is
just what we need.

> infixr 8 :+:
> data (f :+: g) a = SumL (f a) | SumR (g a)
>
> instance (Functor f, Functor g) => Functor (f :+: g) where
>     fmap f term = case term of
>         SumL x -> SumL (fmap f x)
>         SumR x -> SumR (fmap f x)

For any functor `f`, we can always inject it into a free monad over a functor
sum in which it appears as a summand.

> injFL :: Functor f => Free f a -> Free (f :+: g) a
> injFR :: Functor f => Free f a -> Free (g :+: f) a

These are easy to spot:

> injFL term = case term of
>     Pure x -> Pure x
>     Free x -> Free (SumL (fmap injFL x))
>
> injFR term = case term of
>     Pure x -> Pure x
>     Free x -> Free (SumR (fmap injFR x))

Using typeclasses, we can extend this to sums of more than two summands,
just like a construction from
[datatypes à la carte](http://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf).

> class InjectSum f g where
>     inject :: f a -> g a
> 
> instance InjectSum f f where
>     inject = id
> 
> instance InjectSum f (f :+: h) where
>     inject = SumL
> 
> instance (InjectSum f h) => InjectSum f (g :+: h) where
>     inject = SumR . inject
>
> injectF_ :: (Functor g, InjectSum f g) => f a -> Free g a
> injectF_ = liftF . inject
>
> injectF :: (Functor f, Functor g, InjectSum f g) => Free f a -> Free g a
> injectF term = case term of
>     Pure a -> Pure a
>     Free fterm -> Free (inject (fmap injectF fterm))

Now we can tell GHC of our composite monad, `LogPlus`, and write terms in it.

> type LogPlusF = LogF String :+: PlusF
> type LogPlus = Free LogPlusF
>
> logPlusTerm :: Int -> LogPlus Int
> logPlusTerm i = do
>     injectF $ log ("Input is " ++ (show i))
>     sum <- injectF $ plus i 42
>     injectF $ log ("Sum computed successfully!")
>     return sum

But what use is this, when we have no way to interpret a `LogPlus`?
We need something of type `LogPlusF (m a) -> m a`, and we need to somehow
produce it from the existing interpreters for `Log` and `Plus`. If we could
do this, then what would `m` be in the type above? It must in some sense
contain the smaller interpreters, `IO` for `Log` and `Reader Int` for `Plus`.
I can think of one such monad, namely:

> type CompositeInterpreter = IdentityT (ReaderT Int IO) 

And this begs us to think that maybe, if both functors in a sum have interpreters
which are monad transformers, then the interpreter for their sum is these two
transformers stacked atop one-another. The type `:&:` is used to denote this
stacking, so that `(IdentityT :&: ReaderT Int) IO = CompositeInterpreter`.

> infixr 8 :&:
> newtype ((m :: (* -> *) -> * -> *) :&: (n :: (* -> *) -> * -> *)) (s :: * -> *) t = Trans {
>     runTrans :: m (n s) t
>   } deriving (Functor, Applicative, Monad, MonadIO)

If we rephrase the interpreters for `Log` and `Plus` as transformers, then we
can write the type of the `Interpreter` which can handle their sum.

> newtype ModularPlusInterpreter m t = MPI {
>     runMPI :: ReaderT Int m t
>   } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)
>
> newtype StdoutLogInterpreter m t = SLI {
>    runSLI :: IdentityT m t
>  } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)
>
> modularPlus :: Monad m => Interpreter (PlusF) (ModularPlusInterpreter m)
> modularPlus term = case term of
>     Plus left right next -> do
>         modulus <- MPI ask
>         next ((left + right) `mod` modulus)
>
> stdoutLog :: MonadIO m => Interpreter (LogF String) (StdoutLogInterpreter m)
> stdoutLog term = case term of
>     Log str next -> do
>         SLI (liftIO (putStrLn str))
>         next
>
> stdoutLogAndModularPlus
>   :: Interpreter
>        (LogF String :+: PlusF)
>        ((StdoutLogInterpreter :&: ModularPlusInterpreter) IO)

A value for that last type isn't obvious. It ought to be automatic, somehow
inferred by GHC given the two smaller `Interpreter`s. This calls for a
typeclass which determines an `Interpreter`. This class `FInterpreter m b f`
can be read as "m interprets f in base monad b".

> class Functor f => FInterpreter (m :: (* -> *) -> * -> *) (b :: * -> *) (f :: * -> *) where
>     finterpret :: Interpreter f (m b)

Now we need to give an instance for `:&:`. We want `m :&: n` to interpret
a functor sum `f :+: g` over some base `b`, but of course we have to qualify
this with some assumptions:

  - `n` interprets `g` over `b`
  - `m` interprets `f` over `n b`
  - `m`, `n` give monads when stacked over `b`
  - `m` can be "stripped off" and then reset, as described by the class `FTrans`.

That last assumption probably means that not all monad transformers can be
used as interpreters, but it's nothing to worry about, because `IdentityT` and
`ReaderT r` fulfill it, and these are just the monads we use for this
demonstration.

> instance
>     ( FInterpreter n b g
>     , FInterpreter m (n b) f
>     , Monad b
>     , Monad (n b)
>     , Monad (m (n b))
>     , FTrans m
>     ) => FInterpreter (m :&: n) b (f :+: g)
>   where
>     finterpret term = case term of
>         SumL left -> do
>             let term' = fmap runTrans left
>             Trans (finterpret term')
>         SumR right -> do 
>             -- This is where FTrans is needed.
>             -- We can strip off m inside the functor g, interpret using
>             -- FInterpreter n b g, then return to m :&: n using inFTrans.
>             outR <- outFTrans
>             let term' = fmap outR right
>             inFTrans (finterpret term')
>
> class FTrans (m :: (* -> *) -> * -> *) where
>     outFTrans :: (Monad (m (n b)), Monad (n b)) => (m :&: n) b ((m :&: n) b t -> n b t)
>     inFTrans :: (Monad (m (n b))) => n b t -> (m :&: n) b t
>
> instance FTrans IdentityT where
>     outFTrans = Trans (IdentityT (return (runIdentityT . runTrans)))
>     inFTrans = Trans . IdentityT
>
> instance FTrans (ReaderT r) where
>     outFTrans = Trans (ReaderT (\r -> return (\x -> runReaderT (runTrans x) r)))
>     inFTrans = Trans . ReaderT . const

With `FInterpreter` and `FTrans` instances for our interpreters, we have
enough machinery to give a value for `stdoutLogAndModularPlus`.

> instance Monad m => FInterpreter ModularPlusInterpreter m PlusF where
>     finterpret = modularPlus
>
> instance MonadIO m => FInterpreter StdoutLogInterpreter m (LogF String) where
>     finterpret = stdoutLog
>
> instance FTrans (ModularPlusInterpreter) where
>     outFTrans = Trans (MPI (ReaderT (\r -> return (\x -> runReaderT (runMPI (runTrans x)) r))))
>     inFTrans = Trans . MPI . ReaderT . const
>
> instance FTrans (StdoutLogInterpreter) where
>     outFTrans = Trans (SLI (IdentityT (return (runIdentityT . runSLI . runTrans))))
>     inFTrans = Trans . SLI . IdentityT
>
> stdoutLogAndModularPlus = finterpret

Actually, we don't need `FTrans ModularPlusInterpreter`, because it sits
below (on the right of) `StdoutLogInterpreter` in the composite interpreter,
but it's good to have anyway.

We can now interpret terms of `Free (LogF :+: PlusF)`. This demands running
every transformer in the stack, in-order, and stripping of the `:&:`
constructors via `runTrans`, like so:

> step1 :: (:&:) StdoutLogInterpreter ModularPlusInterpreter IO Int
> step1 = iterM stdoutLogAndModularPlus (logPlusTerm 5)
> 
> step2 :: ModularPlusInterpreter IO Int
> step2 = runIdentityT . runSLI . runTrans $ step1
>
> step3 :: Int -> IO Int
> step3 i = (flip runReaderT) i . runMPI $ step2

And that's it! Modular arithmetic with logging to stdout, arising from its
parts.

I'm excited about this technique because it may open up a kind of normal form
for programs, in which all business logic is expressed by DSLs defined by
functors, and these DSLs are mixed via functor sums to give the program's
principal DSL, which is then interpreted by choosing interpreters for each
part.
