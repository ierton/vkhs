{-
Copyright (c) Sebastiaan Visser 2008

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
module Network.Protocol.Uri.Remap where

import Control.Category
import Data.List 
import Data.Label
import Network.Protocol.Uri.Data
import Prelude hiding ((.), id, mod)

-- | Map one URI to another using a URI mapping scheme. A URI mapping scheme is
-- simply a pair of URIs of which only the host part, port number and path will
-- be taken into account when mapping.

remap :: (Uri, Uri) -> Uri -> Maybe Uri
remap (f, t) u =
  let
    ftu = [f, t, u]
    hst = _host . authority
    [h0, h1, h2] = map (get hst)      ftu
    [p0, p1, p2] = map (get port)     ftu
    [s0, s1, s2] = map (get segments) ftu
  in case
     ( remapHost h0 h1 h2
     , remapPort p0 p1 p2
     , remapPath s0 s1 s2
     ) of
    (Just h, Just p, Just s)
      -> Just (set hst h . set port p . set segments s $ u)
    _ -> Nothing
  where
  remapHost (Hostname (Domain a))
            (Hostname (Domain b))   (Hostname (Domain c))          = fmap (Hostname . Domain . (++b)) (a `stripPrefix` reverse c)
  remapHost (Hostname (Domain a)) b (Hostname (Domain c)) | a == c = Just b
  remapHost (RegName a)           b (RegName c)           | a == c = Just b
  remapHost (IP a)                b (IP c)                | a == c = Just b
  remapHost _                     _ _                              = Nothing
  remapPath xs ys zs = fmap (ys++) (xs `stripPrefix` zs)
  remapPort x y z = if x == z then Just y else Nothing

-- from = toUri "http://myhost:8080/ggl"
-- to   = toUri "http://google.com/gapp"
-- testRemap =
--   do let x = remap from to (toUri "http://images.myhost:8080/ggl/search?q=aapjes")
--      print x


