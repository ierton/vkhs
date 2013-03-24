module Web.VKHS.API.Aeson
    ( api
    , api'
    )  where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Error

import Data.Aeson as A
import Data.ByteString.Lazy as BS

import Web.VKHS.Types
import qualified Web.VKHS.API as Base

api' :: (A.FromJSON a) => Env CallEnv -> String -> [(String,String)] -> IO (Either String a)
api' e mn mp = runErrorT $ do
  e <- ErrorT (Base.api e mn mp)
  let check (Just x) = return x
      check (Nothing) = fail $ "AESON: error parsing JSON: " ++ show e
  check $ A.decode $ BS.fromStrict e

api :: Env CallEnv -> String -> [(String,String)] -> IO (Either String A.Value)
api = api'
