{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.VKHS.API.Monad
  ( apiM
  , apiM'
  , runVKAPI
  , VKAPI(..)
  , module Web.VKHS.API.Types
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson as A

import Web.VKHS.Types
import Web.VKHS.API.Types
import Web.VKHS.API.Aeson as VK
import Web.VKHS.Login as VK

newtype VKAPI m a = VKAPI { unVKAPI :: ReaderT (Env LoginEnv) (StateT AccessToken (ErrorT APIError m)) a }
  deriving(Monad, Applicative, Functor, MonadIO, MonadState AccessToken, MonadReader (Env LoginEnv), MonadError APIError)

runVKAPI :: (MonadIO m) => VKAPI m a -> AccessToken -> Env LoginEnv -> m (Either APIError (a,AccessToken))
runVKAPI m at e = runErrorT (runStateT (runReaderT (unVKAPI m) e) at)

shallTryRelogin :: APIError -> Bool
shallTryRelogin (APIE_other _) = False
shallTryRelogin _ = True

-- TODO: report whole error stack
apiWrapped :: (A.FromJSON a, MonadIO m) => Int -> String -> [(String,String)] -> VKAPI m a
apiWrapped nr name args = do
  e <- ask
  (at,_,_) <- get
  r <- liftIO $ VK.api' (callEnv e at) name args
  case (nr,r) of
    (0, Left er) -> throwError er
    (x, Left er)
      | shallTryRelogin er -> do
        res <- liftIO $ VK.login e
        case res of
          Left err -> throwError (APIE_other err)
          Right at' -> do
            put at' >> apiWrapped (x-1) name args
      | otherwise  -> throwError er
    (_, Right a) -> return a

apiM' :: (A.FromJSON a, MonadIO m) => String -> [(String,String)] -> VKAPI m a
apiM' = apiWrapped 1

apiM :: (MonadIO m) => String -> [(String,String)] -> VKAPI m A.Value
apiM = apiM'

