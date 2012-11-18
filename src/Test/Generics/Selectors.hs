{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Selectors where

import Text.JSON as J
import Text.JSON.Types
import GHC.Generics

class Selectors f where
  selectors :: f a -> [String]

instance (Selectors a) => Selectors (M1 S NoSelector a) where
  selectors m = []

instance (Selectors a, Selector c) => Selectors (M1 S c a) where
  selectors m = [selName m]

instance (Selectors a, Constructor c) => Selectors (M1 C c a) where
  selectors m = selectors $ unM1 m

instance (Selectors a, Datatype c) => Selectors (M1 D c a) where
  selectors m = selectors $ unM1 m

instance Selectors (K1 i a) where
  selectors m = []

instance Selectors U1 where
  selectors m = []

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors (a :*: b) = (selectors a) ++ (selectors b)

instance (Selectors a, Selectors b) => Selectors (a :+: b) where
  selectors (L1 l) = selectors l
  selectors (R1 r) = selectors r

data TST = TST { f1 :: String } deriving(Generic)

selectors' :: (Generic a, Selectors (Rep a)) => a -> [String]
selectors' = selectors . from

