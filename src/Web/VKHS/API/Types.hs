{-# LANGUAGE DeriveDataTypeable #-}

module Web.VKHS.API.Types where

import Data.Typeable
import Data.Data

data Response a = Response a
  deriving(Show)

data SizedList a = SL Int a

data MusicRecord = MR
  { aid :: Int
  , owner_id :: Int
  , artist :: String
  , title :: String
  , duration :: Int
  , url :: String
  } deriving (Show, Data, Typeable)


data UserRecord = UR
  { uid :: Int
  , first_name :: String
  , last_name :: String
  , photo :: String
  , university :: Maybe Int
  , university_name :: Maybe String
  , faculty :: Maybe Int
  , faculty_name :: Maybe String
  , graduation :: Maybe Int
  } deriving(Show,Data,Typeable)

