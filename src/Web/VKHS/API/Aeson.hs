module Web.VKHS.API.Aeson
    ( api
    )  where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Error

import Data.Aeson as J
import Data.ByteString.Lazy as BS

import Web.VKHS.Types
import qualified Web.VKHS.API as Base

api :: Env CallEnv -> String -> [(String,String)] -> IO (Either String J.Value)
api e mn mp = runErrorT $ do
  e <- ErrorT (Base.api e mn mp)
  let check (Just x) = return x
      check (Nothing) = fail $ "AESON: error parsing JSON: " ++ show e
  -- let decodeU8 s = J.decode $ BS.pack s
  -- check $ decodeU8 (U.toRep $ U.fromString e)
  check $ J.decode $ BS.fromStrict e

