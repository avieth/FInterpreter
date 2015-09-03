{-|
Module      : Control.Monad.FInterpreter
Description : Interpreation of free monads over functor sums.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Control.Monad.FInterpreter (

    (:+:)
  , (:&:)
  , Interpreter

  , FInterpreter(..)
  , FTrans(..)

  , Trans(..)

  , injectF_
  , injectF
  , injectMF

  , InjectFunctor
  , injectFunctor

  , IsMonadTransformer(..)
  , MonadProof(..)

  ) where

import Data.Proxy
import Data.Functor.Sum
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

infixr 8 :+:
type a :+: b = Sum a b

class (Functor f, Functor g) => InjectFunctorSingle f g where
    injectFunctorSingle :: f a -> g a

instance Functor f => InjectFunctorSingle f f where
    injectFunctorSingle = id

instance (Functor f, Functor h) => InjectFunctorSingle f (f :+: h) where
    injectFunctorSingle = InL

instance (Functor g, InjectFunctorSingle f h) => InjectFunctorSingle f (g :+: h) where
    injectFunctorSingle = InR . injectFunctorSingle

class (Functor f, Functor g) => InjectFunctorMultiple f g where
    injectFunctorMultiple :: f a -> g a

instance (Functor f, InjectFunctorSingle f h) => InjectFunctorMultiple f h where
    injectFunctorMultiple = injectFunctorSingle

instance
    ( InjectFunctorSingle f h
    , InjectFunctorMultiple g h
    ) => InjectFunctorMultiple (f :+: g) h
  where
    injectFunctorMultiple term = case term of
        InL x -> injectFunctorSingle x
        InR x -> injectFunctorMultiple x

data FSumType = FSumSingle | FSumMultiple

type family FSumIndicator (f :: * -> *) :: FSumType where
    FSumIndicator (g :+: h) = FSumMultiple
    FSumIndicator g = FSumSingle

class InjectFunctor' (indicator :: FSumType) f g where
    injectFunctor' :: Proxy indicator -> f a -> g a

instance InjectFunctorSingle f g => InjectFunctor' FSumSingle f g where
    injectFunctor' _ = injectFunctorSingle

instance InjectFunctorMultiple f g => InjectFunctor' FSumMultiple f g where
    injectFunctor' _ = injectFunctorMultiple

class InjectFunctor f g where
    injectFunctor :: f a -> g a

instance InjectFunctor' (FSumIndicator f) f g => InjectFunctor f g where
    injectFunctor = injectFunctor' (Proxy :: Proxy (FSumIndicator f))

injectF_ :: (MonadFree g m, Functor g, InjectFunctor f g) => f a -> m a
injectF_ = liftF . injectFunctor

injectF :: (Monad m, Functor f, Functor g, InjectFunctor f g) => FreeT f m a -> FreeT g m a
injectF = transFreeT injectFunctor

injectMF
    :: (MonadFree g m, Functor f, Functor g)
    => (forall a . f a -> g a)
    -> Free f a
    -> m a
injectMF trans = iterM (\term -> wrap (trans term))

infixr 8 :&:
type m :&: n = Trans m n

newtype Trans (m :: (* -> *) -> * -> *) (n :: (* -> *) -> * -> *) (s :: * -> *) t = Trans {
    runTrans :: m (n s) t
  } deriving (Functor, Applicative, Monad, MonadIO)

-- | Unfortunately, we cannot give a straightforward MonadTrans instance for
--   Trans m n !!! That's because we must be clever to produce the Monad (n s)
--   constraint necessary to lift an n s t term into m.
data MonadProof t where
    MonadProof :: Monad t => MonadProof t

class IsMonadTransformer (n :: (* -> *) -> * -> *) where
    monadProof :: forall s . Monad s => MonadProof (n s)

instance IsMonadTransformer (StateT s) where
    monadProof = MonadProof

instance IsMonadTransformer (ReaderT r) where
    monadProof = MonadProof

instance IsMonadTransformer (IdentityT) where
    monadProof = MonadProof

instance Functor f => IsMonadTransformer (FreeT f) where
    monadProof = MonadProof

instance (IsMonadTransformer n, MonadTrans m, MonadTrans n) => MonadTrans (Trans m n) where
    lift (term :: s t) =
        let nTerm = lift term :: n s t
            prf = monadProof :: MonadProof (n s)
        in  case prf of
                MonadProof -> Trans (lift nTerm)

transLift
    :: ( Monad s
       , MonadTrans m
       , MonadTrans n
       , Monad (n s)
       )
    => s t
    -> Trans m n s t
transLift = Trans . lift . lift

type Interpreter f m = forall a . f (m a) -> m a

class Functor f => FInterpreter (m :: (* -> *) -> * -> *) (b :: * -> *) (f :: * -> *) where
    finterpret :: Interpreter f (m b)

class FTrans (m :: (* -> *) -> * -> *) where
    transInterp
      :: ( Functor f
         , Functor n
         , Applicative n
         , Monad n
         )
      => Interpreter f n
      -> Interpreter f (m n)

instance FTrans IdentityT where
    transInterp interp = IdentityT . interp . fmap runIdentityT

instance FTrans (ReaderT r) where
    transInterp interp term = ReaderT $ \r -> interp (fmap (\x -> runReaderT x r) term)

instance FTrans (StateT s) where
    transInterp interp term = StateT $ \s -> interp (fmap (\x -> runStateT x s) term)

instance
    ( FInterpreter n b g
    , FInterpreter m (n b) f
    , FTrans m
    , Functor (n b)
    , Applicative (n b)
    , Monad (n b)
    ) => FInterpreter (m :&: n) b (f :+: g)
  where
    finterpret term = case term of
        InL left -> Trans . finterpret $ fmap runTrans left
        InR right -> Trans . transInterp finterpret $ fmap runTrans right
