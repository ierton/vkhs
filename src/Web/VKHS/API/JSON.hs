module Web.VKHS.API.JSON
    ( api
    )  where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Error

import Text.JSON as J

import Web.VKHS.Types
import qualified Web.VKHS.API as Base

liftR (Ok a) = return a
liftR (Error s) = fail s

api :: Env -> AccessToken -> String -> [(String,String)] -> IO (Either String JSValue)
api e a mn mp = runErrorT $ ErrorT (Base.api e a mn mp)  >>= liftR . J.decode
