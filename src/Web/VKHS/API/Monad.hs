{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.VKHS.API.Monad
  ( api
  , module Web.VKHS.API.Types
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
-- import Control.Monad.Trans
import Data.Aeson as A
import Data.Aeson.Types as A

import Web.VKHS.Types
import Web.VKHS.API.Types
import Web.VKHS.API.Aeson as VK
import Web.VKHS.Login as VK

newtype VKAPI m a = VKAPI { unVKAPI :: ReaderT (Env LoginEnv) (StateT AccessToken (ErrorT APIError m)) a }
  deriving(Monad, Applicative, Functor, MonadIO, MonadState AccessToken, MonadReader (Env LoginEnv), MonadError APIError)

-- TODO: report whole error stack
apiWrapped :: (A.FromJSON a, MonadIO m) => Int -> String -> [(String,String)] -> VKAPI m a
apiWrapped nr name args = do
  env <- ask
  (at,_,_) <- get
  res <- liftIO $ VK.api' (callEnv env at) name args
  case (nr,res) of
    (0, Left e) -> throwError e
    (_, Left e@(AIE_other _)) -> throwError e
    (x, Left e@(AIE (ER c m))) -> do
      res <- liftIO $ VK.login env
      case res of
        Left err -> throwError (AIE_other err)
        Right at' -> do
          put at' >> apiWrapped (x-1) name args
    (_, Right a) -> return a

