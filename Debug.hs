-- FIXME: in order debug to work
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Debug where

import Control.Monad
import Control.Monad.Trans

class Debug x where
    debug :: (MonadIO m) => x -> m ()

instance (Show x) => Debug x where
    debug = liftIO . putStrLn . show . show

instance Debug String where
    debug = liftIO . putStrLn . show

