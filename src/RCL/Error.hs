{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module RCL.Error where



-- imports

import           Control.Monad.Error



-- exported functions

withError :: MonadError a m => Either a b -> m b
withError = either throwError return

testError :: MonadError e m => m a -> m Bool
testError e = (e >> return False) `catchError` const (return True)
