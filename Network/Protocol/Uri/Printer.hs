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
module Network.Protocol.Uri.Printer where

import Network.Protocol.Uri.Data

printPath :: Path -> ShowS
printPath (Path ("":xs)) = sc '/' . printPath (Path xs)
printPath (Path xs)      = intersperseS (sc '/') (map ss xs)

showPath :: Path -> String
showPath = flip printPath ""

-- instance Show Path where
--   showsPrec _ (Path ("":xs)) = sc '/' . shows (Path xs)
--   showsPrec _ (Path xs)      = intersperseS (sc '/') (map ss xs)

printIPv4 :: IPv4 -> ShowS
printIPv4 (IPv4 a b c d) = intersperseS (sc '.') (map shows [a, b, c, d])

-- instance Show IPv4 where
--   showsPrec _ (IPv4 a b c d) = intersperseS (sc '.') (map shows [a, b, c, d])

printDomain :: Domain -> ShowS
printDomain (Domain d) = intersperseS (sc '.') (map ss d)

-- instance Show Domain where
--   showsPrec _ (Domain d) = intersperseS (sc '.') (map ss d)

printHost :: Host -> ShowS
printHost (Hostname d) = printDomain d
printHost (IP i)       = printIPv4 i 
printHost (RegName r)  = ss r

showHost :: Host -> String
showHost = flip printHost ""

printAuthority :: Authority -> ShowS
printAuthority (Authority u h p) =
    let u' = if null u then id else ss u . ss "@"
        p' = maybe id (\s -> sc ':' . shows s) p
    in u' . printHost h . p'

showAuthority :: Authority -> String
showAuthority = flip printAuthority ""

printUri :: Uri -> ShowS
printUri (Uri _ s a p q f) =
    let s' = if null s then id else ss s . sc ':'
        a' = printAuthority a ""
        p' = printPath p
        q' = if null q then id else sc '?' . ss q
        f' = if null f then id else sc '#' . ss f
        t' = if null a' then id else ss "//"
    in s' . t' . ss a' . p' . q' . f'

showUri :: Uri -> String
showUri = flip printUri ""

ss :: String -> ShowS
ss = showString

sc :: Char -> ShowS
sc = showChar

-- | ShowS version of intersperse.

intersperseS :: ShowS -> [ShowS] -> ShowS
intersperseS _ []     = id
intersperseS s (x:xs) = foldl (\a b -> a.s.b) x xs

