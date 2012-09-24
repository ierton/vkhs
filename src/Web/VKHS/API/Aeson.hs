module Web.VKHS.API.Aeson
    ( api
    )  where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Error

import Data.Aeson as J
import Data.ByteString.Lazy.Char8 as BS

import Web.VKHS.Types
import qualified Web.VKHS.API as Base

api :: Env CallEnv -> String -> [(String,String)] -> IO (Either String J.Value)
api e mn mp = runErrorT $ do
  e <- ErrorT (Base.api e mn mp)
  let check (Just x) = return x
      check (Nothing) = fail $ "AESON: error parsing JSON: " ++ e
  check $ J.decode (BS.pack e)

