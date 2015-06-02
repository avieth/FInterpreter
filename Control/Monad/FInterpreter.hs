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

module Control.Monad.FInterpreter (

    (:+:)
  , (:&:)
  , Interpreter

  , FInterpreter(..)
  , FTrans(..)

  , Trans(..)

  , injectF_
  , injectF

  ) where

import Data.Proxy
import Data.Functor.Sum
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

infixr 8 :+:
type a :+: b = Sum a b

class InjectFunctorSingle f g where
    injectFunctorSingle :: f a -> g a

instance InjectFunctorSingle f f where
    injectFunctorSingle = id

instance InjectFunctorSingle f (f :+: h) where
    injectFunctorSingle = InL

instance (InjectFunctorSingle f h) => InjectFunctorSingle f (g :+: h) where
    injectFunctorSingle = InR . injectFunctorSingle

class InjectFunctorMultiple f g where
    injectFunctorMultiple :: f a -> g a

instance InjectFunctorSingle f h => InjectFunctorMultiple f h where
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

injectF_ :: (Functor g, InjectFunctor f g) => f a -> Free g a
injectF_ = liftF . injectFunctor

injectF :: (Functor f, Functor g, InjectFunctor f g) => Free f a -> Free g a
injectF = hoistFree injectFunctor

infixr 8 :&:
type m :&: n = Trans m n

newtype Trans (m :: (* -> *) -> * -> *) (n :: (* -> *) -> * -> *) (s :: * -> *) t = Trans {
    runTrans :: m (n s) t
  } deriving (Functor, Applicative, Monad, MonadIO)

type Interpreter f m = forall a . f (m a) -> m a

class Functor f => FInterpreter (m :: (* -> *) -> * -> *) (b :: * -> *) (f :: * -> *) where
    finterpret :: Interpreter f (m b)

class FTrans (m :: (* -> *) -> * -> *) where
    transInterp :: Functor f => Interpreter f n -> Interpreter f (m n)

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
    ) => FInterpreter (m :&: n) b (f :+: g)
  where
    finterpret term = case term of
        InL left -> Trans . finterpret $ fmap runTrans left
        InR right -> Trans . transInterp finterpret $ fmap runTrans right
