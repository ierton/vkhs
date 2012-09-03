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
{-# LANGUAGE TypeOperators, TupleSections #-}
module Data.Label.Maybe
( (:~>)
, lens
, get
, set
, set'
, modify
, modify'
, embed
, lift
)
where

import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.Maybe
import Prelude hiding ((.), id)
import qualified Data.Label.Abstract as A

type MaybeLens f a = A.Lens (Kleisli (MaybeT Identity)) f a

-- | Lens type for situations in which the accessor functions can fail. This is
-- useful, for example, when accessing fields in datatypes with multiple
-- constructors.

type f :~> a = MaybeLens f a

run :: Kleisli (MaybeT Identity) f a -> f -> Maybe a
run l = runIdentity . runMaybeT . runKleisli l

-- | Create a lens that can fail from a getter and a setter that can themselves
-- potentially fail.

lens :: (f -> Maybe a) -> (a -> f -> Maybe f) -> f :~> a
lens g s = A.lens (kl g) (kl (uncurry s))
  where kl a = Kleisli (MaybeT . Identity . a)

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: (f :~> a) -> f -> Maybe a
get l = run (A.get l)

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

set :: f :~> a -> a -> f -> Maybe f
set l v = run (A.set l . arr (v,))

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: (f :~> a) -> a -> f -> f
set' l v f = f `fromMaybe` set l v f

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

modify :: (f :~> a) -> (a -> a) -> f -> Maybe f
modify l m = run (A.modify l . arr (arr m,))

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: (f :~> a) -> (a -> a) -> f -> f
modify' l m f = f `fromMaybe` modify l m f

-- | Embed a pure lens that points to a `Maybe` field into a lens that might
-- fail.

embed :: A.Lens (->) f (Maybe a) -> f :~> a
embed l = lens (A.get l) (\a f -> Just (A.set l (Just a, f)))


lift :: A.Lens(->) f a -> f :~> a
lift l = lens (\f -> Just $ A.get l f) (\a f -> Just $ A.set l (a,f))



