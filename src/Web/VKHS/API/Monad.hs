{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.VKHS.API.Monad
  ( api
  , api'
  , runVKAPI
  , VKAPI(..)
  , module Web.VKHS.API.Types
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import Data.Aeson as A

import Web.VKHS.API.Types
import qualified Web.VKHS.API.Aeson as VK
import qualified Web.VKHS.Login as VK
import Web.VKHS as VK (callEnv, Env(..), LoginEnv(..), AccessToken, APIError(..))

newtype VKAPI m a = VKAPI { unVKAPI :: ReaderT (Env LoginEnv) (StateT AccessToken (ErrorT APIError m)) a }
  deriving(Monad, Applicative, Functor, MonadIO, MonadState AccessToken, MonadReader (Env LoginEnv), MonadError APIError)

runVKAPI :: (MonadIO m) => VKAPI m a -> VK.AccessToken -> Env LoginEnv -> m (Either VK.APIError (a,AccessToken))
runVKAPI m at e = runErrorT (runStateT (runReaderT (unVKAPI m) e) at)

shallTryRelogin :: APIError -> Bool
shallTryRelogin (APIE_other _) = False
shallTryRelogin _ = True

-- TODO: report whole error stack
apiRetryWrapper :: (A.FromJSON a, MonadIO m) => Int -> String -> [(String,String)] -> VKAPI m a
apiRetryWrapper nr name args = do
  e <- ask
  (at,_,_) <- get
  r <- liftIO $ VK.api' (callEnv e at) name args
  case (nr,r) of
    (0, Left er) -> throwError er
    (x, Left er)
      | shallTryRelogin er -> do
        res <- liftIO $ VK.login e
        case res of
          Left err -> throwError (VK.APIE_other err)
          Right at' -> do
            put at' >> apiRetryWrapper (x-1) name args
      | otherwise  -> throwError er
    (_, Right a) -> return a

apiForewerWrapper :: (A.FromJSON a, MonadIO m) => String -> [(String,String)] -> VKAPI m a
apiForewerWrapper name args = do
  e <- ask
  let call_api = do
        (at,_,_) <- get
        r <- liftIO $ VK.api' (callEnv e at) name args
        case r of
          (Left _) -> do_login
          (Right a) -> return a
      do_login = do
        sleep 3
        r <- liftIO $ VK.login e
        case r of
          Left _ -> do_login
          Right at' -> put at' >> call_api
      sleep x = 
        liftIO $ threadDelay (1000 * 1000 * x); -- convert sec to us
  call_api

api' :: (A.FromJSON a, MonadIO m) => String -> [(String,String)] -> VKAPI m a
api' = apiForewerWrapper

api :: (MonadIO m) => String -> [(String,String)] -> VKAPI m A.Value
api = api'

