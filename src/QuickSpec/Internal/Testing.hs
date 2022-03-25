-- A type of test case generators.
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}
module QuickSpec.Internal.Testing where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           QuickSpec.Internal.Prop

class Monad m => MonadTester testcase term m | m -> testcase term where
  test :: Prop term -> m (Maybe testcase)

  default test :: (MonadTrans t, MonadTester testcase term m', m ~ t m') => Prop term -> m (Maybe testcase)
  test = lift . test

instance MonadTester testcase term m => MonadTester testcase term (StateT s m)
instance MonadTester testcase term m => MonadTester testcase term (ReaderT r m)
