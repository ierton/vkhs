{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
import Control.Applicative
import Text.JSON as J
import Text.JSON.Types
import GHC.Generics

class GJSON f where
  fromJSON :: String -> Maybe (f a)

-- class GJSONh f where

instance (GJSON a, Datatype c) => GJSON (M1 D c a) where
  fromJSON j = M1 <$> fromJSON j

instance (GJSON a, Constructor c) => GJSON (M1 C c a) where
  fromJSON j = M1 <$> fromJSON j


hinting :: a -> a -> a
hinting a b = if True then a else b

instance (GJSON a, Selector c) => GJSON (M1 S c a) where
  fromJSON j = do
    s <- M1 <$> pure undefined {- Oh, playing with fire -}
    s'<- M1 <$> fromJSON (selName s)
    return $ s' `hinting` s

instance GJSON (K1 i String) where
  fromJSON s = K1 <$> (pure s)

instance (GJSON a, GJSON b) => GJSON (a :*: b) where
  fromJSON j = (:*:) <$> (fromJSON j) <*> (fromJSON j)

-- instance (GJSON a, GJSON b) => GJSON (a :+: b) where
--   fromJSON j = fail "only 1 ctor supported"


data TST = TST { f2 :: String } deriving(Show,Generic)


fromJS :: (Generic a, GJSON (Rep a), Show a) => Maybe a
fromJS = to <$> fromJSON ""


