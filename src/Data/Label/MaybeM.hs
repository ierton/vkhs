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
module Data.Label.MaybeM
(
-- * 'MonadState' lens operations.
  gets

-- * 'MonadReader' lens operations.
, asks
)
where

import Control.Monad
import Data.Label.Maybe ((:~>))
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State  as M
import qualified Data.Label.Maybe     as L

-- | Get a value out of state, pointed to by the specified lens that might
-- fail.  When the lens getter fails this computation will fall back to
-- `mzero'.

gets :: (M.MonadState f m, MonadPlus m) => (f :~> a) -> m a
gets l = (L.get l `liftM` M.get) >>= (mzero `maybe` return)

-- | Fetch a value, pointed to by a lens that might fail, out of a reader
-- environment. When the lens getter fails this computation will fall back to
-- `mzero'.

asks :: (M.MonadReader f m, MonadPlus m) => (f :~> a) -> m a
asks l = (L.get l `liftM` M.ask) >>= (mzero `maybe` return)

