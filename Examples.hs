{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.FInterpreter
import Control.Monad.Trans.Free

data FunctorA a = A

data FunctorB b = B

data FunctorC c = C

instance Functor FunctorA where
    fmap f A = A

instance Functor FunctorB where
    fmap f B = B

instance Functor FunctorC where
    fmap f C = C

injectionExamples :: forall m . Monad m => FreeT (FunctorA :+: FunctorB :+: FunctorC) m ()
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
    termA :: (FreeT FunctorA m) ()
    termA = undefined
    termB :: (FreeT FunctorB m) ()
    termB = undefined
    termC :: (FreeT FunctorC m) ()
    termC = undefined
    termAB :: (FreeT (FunctorA :+: FunctorB) m) ()
    termAB = undefined
    termAC :: (FreeT (FunctorA :+: FunctorC) m) ()
    termAC = undefined
    termBC :: (FreeT (FunctorB :+: FunctorC) m) ()
    termBC = undefined
    termCB :: (FreeT (FunctorC :+: FunctorB) m) ()
    termCB = undefined
    termCA :: (FreeT (FunctorC :+: FunctorA) m) ()
    termCA = undefined
    termBA :: (FreeT (FunctorB :+: FunctorA) m) ()
    termBA = undefined
    termABC :: (FreeT (FunctorA :+: FunctorB :+: FunctorC) m) ()
    termABC = undefined
    termACB :: (FreeT (FunctorA :+: FunctorC :+: FunctorB) m) ()
    termACB = undefined
    termBAC :: (FreeT (FunctorB :+: FunctorA :+: FunctorC) m) ()
    termBAC = undefined
