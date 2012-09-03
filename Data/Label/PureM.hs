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
{-# LANGUAGE TypeOperators  #-}
module Data.Label.PureM
(
-- * 'MonadState' lens operations.
  gets
, puts
, modify
, (=:)
, (=.)

-- * 'MonadReader' lens operations.
, asks
, local
)
where

import Data.Label.Pure ((:->))
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State  as M
import qualified Data.Label.Pure      as L

-- | Get a value out of the state, pointed to by the specified lens.

gets :: M.MonadState s m => s :-> a -> m a
gets = M.gets . L.get

-- | Set a value somewhere in the state, pointed to by the specified lens.

puts :: M.MonadState s m => s :-> a -> a -> m ()
puts l = M.modify . L.set l

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens.

modify :: M.MonadState s m => s :-> a -> (a -> a) -> m ()
modify l = M.modify . L.modify l

-- | Alias for `puts' that reads like an assignment.

infixr 2 =:
(=:) :: M.MonadState s m => s :-> a -> a -> m ()
(=:) = puts

-- | Alias for `modify' that reads more or less like an assignment.

infixr 2 =.
(=.) :: M.MonadState s m => s :-> a -> (a -> a) -> m ()
(=.) = modify

-- | Fetch a value pointed to by a lens out of a reader environment.

asks :: M.MonadReader r m => (r :-> a) -> m a
asks = M.asks . L.get

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

local :: M.MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
local l f = M.local (L.modify l f)

