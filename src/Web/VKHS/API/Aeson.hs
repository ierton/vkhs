{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.VKHS.API.Aeson
    ( api
    , api'
    , module Web.VKHS.API.Types
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Error

import Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson (FromJSON, (.:), (.:?))
import Data.Aeson.Generic as AG
import Data.ByteString.Lazy as BS
import Data.Typeable
import Data.Data
import Data.Vector as V (head, tail, toList)

import Web.VKHS.Types
import Web.VKHS.API.Types
import qualified Web.VKHS.API as Base

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (A.Object v) = do
    a <- v .: "response"
    x <- A.parseJSON a
    return (Response x)

parseGeneric a =
  case AG.fromJSON a of
    A.Success a -> return a
    A.Error s -> fail $ "parseGeneric fails:" ++ s

instance FromJSON MusicRecord where
  parseJSON = parseGeneric

instance FromJSON WallRecord where
  parseJSON (Object o) = 
    WR <$> (o .: "id")
       <*> (o .: "to_id")
       <*> (o .: "from_id")
       <*> (o .: "text")
       <*> (o .: "date")

instance (FromJSON a) => FromJSON (SizedList [a]) where
  parseJSON (A.Array v) = do
    n <- A.parseJSON (V.head v)
    t <- A.parseJSON (A.Array (V.tail v))
    return (SL n t)

api' :: (A.FromJSON a) => Env CallEnv -> String -> [(String,String)] -> IO (Either String a)
api' e mn mp = runErrorT $ do
  e <- ErrorT (Base.api e mn mp)
  let check (Just x) = return x
      check (Nothing) = fail $ "AESON: error parsing JSON: " ++ show e
  check $ A.decode $ BS.fromStrict e

api :: Env CallEnv -> String -> [(String,String)] -> IO (Either String A.Value)
api = api'

