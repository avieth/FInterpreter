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

import Data.Functor.Sum
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

infixr 8 :+:
type a :+: b = Sum a b

class InjectSum f g where
    inject :: f a -> g a

instance InjectSum f f where
    inject = id

instance InjectSum f (f :+: h) where
    inject = InL

instance (InjectSum f h) => InjectSum f (g :+: h) where
    inject = InR . inject

instance (InjectSum f h, InjectSum g h) => InjectSum (f :+: g) h where
    inject term = case term of
        InL x -> inject x
        InR x -> inject x

injectF_ :: (Functor g, InjectSum f g) => f a -> Free g a
injectF_ = liftF . inject

injectF :: (Functor f, Functor g, InjectSum f g) => Free f a -> Free g a
injectF term = case term of
    Pure a -> Pure a
    Free fterm -> Free (inject (fmap injectF fterm))

infixr 8 :&:
type m :&: n = Trans m n

newtype Trans (m :: (* -> *) -> * -> *) (n :: (* -> *) -> * -> *) (s :: * -> *) t = Trans {
    runTrans :: m (n s) t
  } deriving (Functor, Applicative, Monad, MonadIO)

type Interpreter f m = forall a . f (m a) -> m a

class Functor f => FInterpreter (m :: (* -> *) -> * -> *) (b :: * -> *) (f :: * -> *) where
    finterpret :: Interpreter f (m b)

instance
    ( FInterpreter n b g
    , FInterpreter m (n b) f
    , Monad b
    , Monad (n b)
    , Monad (m (n b))
    , FTrans m
    ) => FInterpreter (m :&: n) b (f :+: g)
  where
    finterpret term = case term of
        InL left -> do
            let term' = fmap runTrans left
            Trans (finterpret term')
        InR right -> do 
            outR <- outFTrans
            let term' = fmap outR right
            inFTrans (finterpret term')

class FTrans (m :: (* -> *) -> * -> *) where
    outFTrans :: (Monad (m (n b)), Monad (n b)) => (m :&: n) b ((m :&: n) b t -> n b t)
    inFTrans :: (Monad (m (n b))) => n b t -> (m :&: n) b t

instance FTrans IdentityT where
    outFTrans = Trans (IdentityT (return (runIdentityT . runTrans)))
    inFTrans = Trans . IdentityT

instance FTrans (ReaderT r) where
    outFTrans = Trans (ReaderT (\r -> return (\x -> runReaderT (runTrans x) r)))
    inFTrans = Trans . ReaderT . const
