{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}

import Control.Monad.FInterpreter
import Control.Monad.Free

data FunctorA a = A

data FunctorB b = B

data FunctorC c = C

instance Functor FunctorA where
    fmap f A = A

instance Functor FunctorB where
    fmap f B = B

instance Functor FunctorC where
    fmap f C = C

injectionExamples :: Free (FunctorA :+: FunctorB :+: FunctorC) ()
injectionExamples = do
    injectF termA
    injectF termB
    injectF termC
    injectF termAB
    injectF termAC
    injectF termBC
    injectF termCB
    injectF termCA
    injectF termBA
    injectF termABC
    injectF termACB
    injectF termBAC
  where
    termA :: (Free FunctorA) ()
    termA = undefined
    termB :: (Free FunctorB) ()
    termB = undefined
    termC :: (Free FunctorC) ()
    termC = undefined
    termAB :: (Free (FunctorA :+: FunctorB)) ()
    termAB = undefined
    termAC :: (Free (FunctorA :+: FunctorC)) ()
    termAC = undefined
    termBC :: (Free (FunctorB :+: FunctorC)) ()
    termBC = undefined
    termCB :: (Free (FunctorC :+: FunctorB)) ()
    termCB = undefined
    termCA :: (Free (FunctorC :+: FunctorA)) ()
    termCA = undefined
    termBA :: (Free (FunctorB :+: FunctorA)) ()
    termBA = undefined
    termABC :: (Free (FunctorA :+: FunctorB :+: FunctorC)) ()
    termABC = undefined
    termACB :: (Free (FunctorA :+: FunctorC :+: FunctorB)) ()
    termACB = undefined
    termBAC :: (Free (FunctorB :+: FunctorA :+: FunctorC)) ()
    termBAC = undefined
