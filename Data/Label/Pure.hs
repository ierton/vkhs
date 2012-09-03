{-
Copyright (c) Erik Hesselink & Sebastiaan Visser 2008

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
-}
{-# LANGUAGE TypeOperators #-}
module Data.Label.Pure
( (:->)
, lens
, get
, set
, modify
-- , glue
, fmapL
)
where

import qualified Data.Label.Abstract as A
import Control.Applicative

type PureLens f a = A.Lens (->) f a

-- | Pure lens type specialized for pure accessor functions.

type (f :-> a) = PureLens f a

-- | Create a pure lens from a getter and a setter.
--
-- We expect the following law to hold:
--
-- > get l (set l a f) == a
--
-- Or, equivalently:
--
-- > set l (get l f) f == f

lens :: (f -> a) -> (a -> f -> f) -> f :-> a
lens g s = A.lens g (uncurry s)

-- | Getter for a pure lens.

get :: (f :-> a) -> f -> a
get = A.get

-- | Setter for a pure lens.

set :: (f :-> a) -> a -> f -> f
set = curry . A.set

-- | Modifier for a pure lens.

modify :: (f :-> a) -> (a -> a) -> f -> f
modify = curry . A.modify


fmapL :: (Alternative f) => (a :-> b) -> (f a :-> f b)
fmapL (A.Lens (A.Point g' s')) = lens g s where
    g a = fmap g' a
    s fb fa = ((\b a->s' (b,a)) <$> fb <*> fa) <|> fa

-- glue :: (Alternative f) => (o :-> f a) -> (a :-> b) -> (o :-> f b)
-- glue l1 l@(A.Lens (A.Point g2 s2)) = lens g s where
--     g o = fmap g2 (get l1 o)
--     s fb o = set l1 (set ll fb fa) o where
--         fa = get l1 o
--         ll = fmapL l

-- glue2 :: (Alternative f) => (o :-> f a) -> (a :-> f b) -> (o :-> f b)
-- glue2 l1 l2 = lens g s where

